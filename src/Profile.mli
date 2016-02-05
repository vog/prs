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

type t

val zero : t
val p : int -> int -> t
val scalarmul : int -> t -> t
val add : t -> t -> t
val shift : t -> int -> t
val to_list : t -> (int * int) list

val values : t -> int list
val nonnegative : t -> bool
val duration : t -> int
val max : t -> int
val negate : t -> t
val sum : t list -> t
val sub : t -> t -> t
val le : t -> t -> bool
val arrange : t -> t -> t
val e : t
val combine_processes : t -> t -> t
val combine_resources : t -> t -> t
