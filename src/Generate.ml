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

exception Failure

let rec generate_pk (k : int) : Tree.t =
  (* Walther schedules *)
  match k with
  | 1 -> Parse.tree "e"
  | 2 -> Parse.tree "(e,e)"
  | _ -> Tree.T (generate_pk (k-2), generate_pk (k-1))

let generate_1 (_ : int) : Tree.t =
  Tree.E

let rec generate_2 (k : int) : Tree.t =
  match k with
  | 2 -> Parse.tree "(e,e)"
  | _ -> Tree.T (generate_2 (k-1), Tree.E)

let rec generate_3 (k : int) : Tree.t =
  match k with
  | 4 -> Tree.T (generate_2 3, generate_2 2)
  | _ -> Tree.T (generate_3 (k-1), generate_2 (k-2))

let rec torec (reach : int) : Tree.t =
  (* total recalculation schedules *)
  match reach with
  | 1 -> Tree.E
  | _ -> Tree.T (Tree. E, torec (reach-1))

let rec piled_torec (p : int) (k : int) : Tree.t =
  (* piled total recalculation schedules *)
  match k with
  | _ when k = p -> torec 2
  | _ -> Tree.T (piled_torec p (k-1), torec (2*p-3))

let rec generate_more (p : int) (k : int) : Tree.t =
  match k with
  | _ when k = p+1 -> Tree.T (piled_torec (p-1) p, torec (2*p-5))
  | _ -> Tree.T (generate_more p (k-1), piled_torec (p-1) (k-2))

let maxp (k : int) : int =
  (* int_of_float (ceil ((float_of_int (k + 1)) /. 2.)) *)
  k/2 + 1

let generate (processes : Unit.processes) (resources : Unit.resources) : Tree.t =
  let p = Unit.processes processes in
  let k = Unit.resources resources in
  match p with
  | _ when p = -1 -> torec k
  | _ when p <= -2 -> piled_torec (-p) k
  | _ when p <= 0 -> raise Failure
  | _ when k <= 0 -> raise Failure
  | _ when p > k -> raise Failure
  | _ when p = k -> generate_pk k
  | 1 -> generate_1 k
  | 2 -> generate_2 k
  | 3 -> generate_3 k
  | _ -> generate_more p k
