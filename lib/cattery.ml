(*
 * Copyright (c) 2024 Thomas Leonard <talex5@gmail.com>
 *                  & Simon Grondin <simon.grondin@outlook.com>
 * SPDX-License-Identifier: ISC
 * Copyright (c) 2026 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* A pool is a sequence of cells containing either available slots or consumers
   waiting for them. A slot may or may not contain an actual resource.

   To use a resource:

   1. Get the next "suspend" cell. If it contains a resource slot, use it.
   2. If no slot is ready and we're below capacity, create a new slot and add it
      (to the next resume cell).
   3. Either way, wait for the cell to be resumed with a slot.
   4. Once you have a slot, ensure it contains a resource, creating one if not.
   5. When done, add the slot back (in the next resume cell).
*)

type 'a slot = 'a option ref

module Cell = struct
  (* The possible behaviours are:

     1. Suspender : In_transition -> Request  |Suspender waits for a resource
        1.1. Resumer : Request -> Finished    |Resumer then providers a resource
        1.2. Suspender : Request -> Finished  |Suspender cancels
     2. Resumer : In_transition -> Resource   |Resumer provides a spare resource
        2.1. Suspender : Resource -> Finished |Suspender doesn't need to wait
  *)

  type 'a t =
    | In_transition
    | Request of ('a slot -> unit)
    | Resource of 'a slot
    | Finished

  let init = In_transition
  let segment_order = 2

  let dump f = function
    | In_transition -> Fmt.string f "In_transition"
    | Request _ -> Fmt.string f "Request"
    | Resource _ -> Fmt.string f "Resource"
    | Finished -> Fmt.string f "Finished"
end

module Q = Cells.Make (Cell)

type 'a t = {
  slots : int Atomic.t; (* Total resources, available and in use *)
  max_slots : int;
  alloc : unit -> 'a;
  validate : 'a -> bool;
  dispose : 'a -> unit;
  q : 'a Q.t;
}

let create ?(validate = Fun.const true) ?(dispose = ignore) max_size alloc =
  if max_size <= 0 then invalid_arg "Pool.create: max_size is <= 0";
  {
    slots = Atomic.make 0;
    max_slots = max_size;
    alloc;
    validate;
    dispose;
    q = Q.make ();
  }

(* [add t x] adds [x] to the queue of available slots. *)
let rec add t x =
  let cell = Q.next_resume t.q in
  let rec go () =
    match Atomic.get cell with
    | In_transition ->
        if not (Atomic.compare_and_set cell In_transition (Resource x)) then
          go ()
    | Finished ->
        add t x (* The consumer cancelled. Get another cell and retry. *)
    | Request r as prev ->
        if Atomic.compare_and_set cell prev Finished then
          r x (* We had a consumer waiting. Give it to them. *)
        else add t x (* Consumer cancelled; retry with another cell. *)
    | Resource _ -> assert false
    (* Can't happen; only a resumer can set this, and we're the resumer. *)
  in
  go ()

(* Try to cancel by transitioning from [Request] to [Finished].
   This can only be called after previously transitioning to [Request]. *)
let cancel segment cell =
  match Atomic.exchange cell Cell.Finished with
  | Request _ ->
      Q.cancel_cell segment;
      true
  | Finished -> false (* Already resumed; reject cancellation *)
  | In_transition | Resource _ ->
      assert false (* Can't get here from [Request]. *)

(* If [t] is under capacity, add another (empty) slot. *)
let rec maybe_add_slot t current =
  if current < t.max_slots then
    if Atomic.compare_and_set t.slots current (current + 1) then
      add t (ref None)
    else maybe_add_slot t (Atomic.get t.slots)
(* Concurrent update; try again *)

(* [run_with t fn slot] ensures that [slot] contains a valid resource and then
   runs [fn resource] with it. Afterwards, the slot is returned to [t]. *)
let run_with t fn slot =
  try
    let result =
      match !slot with
      | Some x when t.validate x -> fn x
      | Some x ->
          slot := None;
          t.dispose x;
          let x = t.alloc () in
          slot := Some x;
          fn x
      | None ->
          let x = t.alloc () in
          slot := Some x;
          fn x
    in
    add t slot;
    result
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    add t slot;
    Printexc.raise_with_backtrace exn bt

(* Creates a fresh resource [x], runs [fn x], then disposes of [x] *)
let run_new_and_dispose t fn =
  let x = t.alloc () in
  match fn x with
  | result ->
      t.dispose x;
      result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      t.dispose x;
      Printexc.raise_with_backtrace exn bt

module Ivar = Miou.Computation

let use t ?(never_block = false) fn =
  let segment, cell = Q.next_suspend t.q in
  match Atomic.get cell with
  | Finished | Request _ -> assert false
  | Resource slot ->
      Atomic.set cell Finished;
      (* Allow value to be GC'd *)
      run_with t fn slot
  | In_transition -> begin
      let current = Atomic.get t.slots in
      match current < t.max_slots with
      | false when never_block -> begin
          (* We are at capacity, but cannot block.
             Create a new resource to run [fn] but don't add it to the pool. *)
          match Atomic.exchange cell Finished with
          | Resource slot -> run_with t fn slot
          | _ -> run_new_and_dispose t fn
        end
      | can_add ->
          (* Create a slot if not at capacity. *)
          if can_add then maybe_add_slot t current;
          (* No item is available right now. Start waiting.

             This part must handle cancellation, hence the use of
             [Miou.Ownership]. There are two cases:
             1) either we are waiting for a slot (the [compare_and_set]
                succeeded),
             2) or someone has concurrently just provided us with a slot.

             In both branches, the goal is to fill our [Ivar]. Then, we await
             our [Ivar]. In the second case, there will be no waiting time since
             it has just been filled. However, within the project, it is during
             the release of another task that our [Ivar] will be filled, so we
             are in a waiting state.

             During this wait, we may be subject to cancellation. In this event,
             [Miou.Ownership] guarantees that our [finally] is executed. The
             latter cancels the segment and discontinues [Ivar.await_exn].
             Consequently, [disown] and [run_with] are not executed, and the
             resource is properly cleaned up. *)
          let finally () = ignore (cancel segment cell) in
          let res = Miou.Ownership.create ~finally () in
          Miou.Ownership.own res;
          let ivar = Ivar.create () in
          let fill slot = ignore (Ivar.try_return ivar slot) in
          let set = Atomic.compare_and_set cell In_transition (Request fill) in
          if not set then begin
            match Atomic.exchange cell Finished with
            | Resource slot -> ignore (Ivar.try_return ivar slot)
            | _ -> assert false
          end;
          let slot = Ivar.await_exn ivar in
          Miou.Ownership.disown res;
          run_with t fn slot
    end
