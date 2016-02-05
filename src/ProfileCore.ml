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

type t =
  (int * int) list

let zero : t =
  []

let p (a : int) (t : int) : t =
  match a with
  | 0 -> zero
  | a -> [(a, t)]

let scalarmul (a : int) (f : t) : t =
  match a with
  | 0 -> zero
  | a -> List.map (fun (b, t) -> (a * b, t)) f

let add (f : t) (g : t) : t =
  let rec aux (f : t) (g : t) : t =
    match f, g with
    | f, [] -> f
    | [], g -> g
    | (a, t)::r, (b, u)::s when t = u && a + b = 0 -> aux r s
    | (a, t)::r, (b, u)::s when t = u -> (a + b, t) :: aux r s
    | (a, t)::r, (_, u)::_ when t < u -> (a, t) :: aux r g
    | (_, _)::_, (b, u)::s -> (b, u) :: aux f s
  in
  aux f g

let shift (f : t) (t : int) : t =
  let aux (b, u) = (b, u + t) in
  List.map aux f

let to_list (f : t) : (int * int) list =
  f
