(* Copyright (C) 2015, Volker Diels-Grabsch
 *
 * Permission to use, copy, modify, and/or distribute this software for any
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

type transition =
  | Nothing
  | Checkpoint
  | Forward
  | Prepare
  | Reverse

type t

val from_list : transition list list -> t
val to_list : t -> transition list list
val from_func : int * (int -> int -> transition) -> t
val to_func : t -> int * (int -> int -> transition)
val iter_times : t -> (int -> ((int -> transition -> unit) -> unit) -> unit) -> unit
val iter_steps_rev : t -> (int -> ((int -> transition -> unit) -> unit) -> unit) -> unit
val processes : transition list -> Unit.processes
val resources : transition list -> Unit.resources
val summary : t -> Unit.summary
