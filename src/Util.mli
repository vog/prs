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

val read_all_channel : in_channel -> string
val read_all : string -> string
val write_atomic : string -> string -> unit

val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
val safe_concat : 'a list list -> 'a list
val safe_map : ('a -> 'b) -> 'a list -> 'b list
val prefixes : 'a list -> 'a list list
val list_trim_begin : 'a -> 'a list -> 'a list
val list_trim_end : 'a -> 'a list -> 'a list
val last : 'a list -> 'a option
val without_last : 'a list -> 'a list
val map_without_last : ('a -> 'b) -> 'a list -> 'b list
val map_pairs : ('a -> 'a -> 'b) -> 'a list -> 'b list
val max : 'a -> 'a list -> 'a
val sum : int list -> int
val map_range : int -> int -> (int -> 'a) -> 'a list
val rev_map_range : int -> int -> (int -> 'a) -> 'a list
val rev_collect_if : ('a -> bool) -> (('a -> unit) -> unit) -> 'a list
val rev_collect : (('a -> unit) -> unit) -> 'a list
val collect_if : ('a -> bool) -> (('a -> unit) -> unit) -> 'a list
val collect : (('a -> unit) -> unit) -> 'a list

val concat_lines : string list -> string
val trim_end : string -> string
