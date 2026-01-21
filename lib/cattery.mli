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

type 'a t

val create :
  ?validate:('a -> bool) -> ?dispose:('a -> unit) -> int -> (unit -> 'a) -> 'a t
(** [create n alloc] is a fresh pool which allows up to [n] resources to be live
    at a time. It uses [alloc] to create new resources as needed. If [alloc]
    raises an exception then that us fails, but future calls to {!val:use} will
    retry. *)

val use : 'a t -> ?never_block:bool -> ('a -> 'b) -> 'b
