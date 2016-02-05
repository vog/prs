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

type t = ProfileCore.t

let zero = ProfileCore.zero
let p = ProfileCore.p
let scalarmul = ProfileCore.scalarmul
let add = ProfileCore.add
let shift = ProfileCore.shift
let to_list = ProfileCore.to_list

let values (f : t) : int list =
  let aux sum_list_rev (a, _) =
    (List.hd sum_list_rev + a)::sum_list_rev
  in
  let l = to_list f in
  let sum_list_rev = List.fold_left aux [0] l in
  List.rev sum_list_rev

let nonnegative (f : t) : bool =
  f |> values |> List.for_all ((<=) 0)

let duration (f : t) : int =
  let aux _ (_, t) = t in
  let l = to_list f in
  let last_time_or_zero = List.fold_left aux 0 l in
  last_time_or_zero

let max (f : t) : int =
  f |> values |> List.fold_left Pervasives.max 0

let negate (f : t) : t =
  scalarmul (-1) f

let sum (l : t list) : t =
  List.fold_left add zero l

let sub (f : t) (g : t) : t =
  add f (negate g)

let le (f : t) (g : t) : bool =
  nonnegative (sub g f)

let arrange (f : t) (g : t) : t =
  add (shift f ((duration g) / 2)) (shift g (duration f))

let e : t =
  add (p 1 0) (p (-1) 2)

let combine_processes (f : t) (g : t) : t =
  sum [arrange f g; p 1 0; p (-1) ((duration g) / 2)]

let combine_resources (f : t) (g : t) : t =
  sum [arrange f g; p 2 0; p (-1) ((duration g) / 2); p (-1) (duration f)]
