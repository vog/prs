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
  | E
  | T of t * t

let map (e : 'a) (f : 'a -> 'a -> 'a) (tree : t) : 'a =
  let rec aux tree =
    match tree with
    | E -> e
    | T (a, b) -> f (aux a) (aux b)
  in
  aux tree

let reach (tree : t) : int =
  map 1 (+) tree

let to_schedule (tree : t) : CompleteSchedule.t =
  let f_leaf time step =
    match time, step with
    | 0, 1 -> CompleteSchedule.Prepare
    | 1, 1 -> CompleteSchedule.Reverse
    | _ -> CompleteSchedule.Nothing
  in
  let f_tree steps_a f_a steps_b f_b time step =
    let diag = 2*(steps_a+steps_b)-1 - time + step in
    let schedule_b_diag = 2 * steps_b in
    let schedule_a_diag = 2 * (steps_a + steps_b) in
    match () with
    | _ when step = 0 && schedule_b_diag <= diag && diag < schedule_a_diag -> CompleteSchedule.Checkpoint
    | _ when 1 <= step && step <= steps_b && diag = schedule_a_diag -> CompleteSchedule.Forward
    | _ when diag <= schedule_b_diag -> f_b (time - 2*steps_a) step
    | _ -> f_a (time - steps_b) (step - steps_b)
  in
  let f_combine (steps_a, f_a) (steps_b, f_b) =
    steps_a + steps_b, f_tree steps_a f_a steps_b f_b
  in
  CompleteSchedule.from_func (map (1, f_leaf) f_combine tree)
