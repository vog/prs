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

type fiber =
  | Checkpoint
  | Process of int

type fiber_schedule = fiber option list list

let schedule (fiber_schedule : fiber_schedule) : bool list list =
  Util.map_without_last (Util.map_without_last ((!=) None)) fiber_schedule

let fiber_schedule_prev (next : fiber option) (next_up : fiber option) : fiber option =
  match next, next_up with
  | (None | Some Process _), (None | Some Checkpoint) -> None
  | (None | Some Process _), Some Process n -> Some (Process n)
  | Some Checkpoint, (None | Some Checkpoint | Some Process 0) -> Some Checkpoint
  | Some Checkpoint, Some Process n -> Some (Process (n-1))

let fiber_schedule (fibers : fiber list) : fiber_schedule =
  let rec aux (rev_fibers : fiber list) : fiber_schedule =
    match rev_fibers with
    | [] ->
       []
    | [fiber] ->
       [[Some fiber]]
    | fiber::rest ->
       let rest_schedule = aux rest in
       let fibers_next = List.hd rest_schedule in
       let fibers_cur = Util.map_pairs fiber_schedule_prev (fibers_next @ [None]) @ [Some fiber] in
       fibers_cur :: rest_schedule
  in
  aux (Checkpoint :: List.rev fibers)

let from_fibers (fibers : fiber list) : bool list list =
  fibers |> fiber_schedule |> schedule

let top_transition (state : bool list) : CompleteSchedule.transition list =
  let transform = function
    | false -> CompleteSchedule.Nothing
    | true -> CompleteSchedule.Checkpoint
  in
  List.map transform state

let transition (state1 : bool list) (state2 : bool list) : CompleteSchedule.transition list =
  let transform part1 part2 =
    match part1, part2 with
    | false, false -> CompleteSchedule.Nothing
    | false, true -> CompleteSchedule.Forward
    | true, false -> CompleteSchedule.Nothing
    | true, true -> CompleteSchedule.Checkpoint
  in
  List.map2 transform state1 (state2 @ [true])

let to_complete_schedule (prime_schedule : bool list list) : CompleteSchedule.t =
  match prime_schedule with
  | [] ->
     CompleteSchedule.from_list []
  | first_state::prime_schedule_tail ->
     let top_transition = top_transition (first_state @ [true]) in
     let transitions = List.map2 transition prime_schedule (prime_schedule_tail @ [[]]) in
     let forward_sweep = top_transition |> Util.prefixes |> List.map
       (fun prefix -> [CompleteSchedule.Checkpoint] @ prefix @ [CompleteSchedule.Forward])
     in
     let top = [
       [CompleteSchedule.Checkpoint] @ top_transition @ [CompleteSchedule.Checkpoint; CompleteSchedule.Forward];
       [CompleteSchedule.Checkpoint] @ top_transition @ [CompleteSchedule.Checkpoint; CompleteSchedule.Nothing; CompleteSchedule.Prepare];
       [CompleteSchedule.Checkpoint] @ top_transition @ [CompleteSchedule.Nothing; CompleteSchedule.Prepare; CompleteSchedule.Reverse];
     ] in
     let reverse_sweep = transitions |> List.map
       (fun transition -> [CompleteSchedule.Checkpoint] @ transition @ [CompleteSchedule.Nothing; CompleteSchedule.Prepare; CompleteSchedule.Reverse])
     in
     let last_steps = [
       [CompleteSchedule.Checkpoint; CompleteSchedule.Nothing; CompleteSchedule.Prepare; CompleteSchedule.Reverse];
       [CompleteSchedule.Nothing; CompleteSchedule.Prepare; CompleteSchedule.Reverse];
       [CompleteSchedule.Nothing; CompleteSchedule.Reverse];
     ] in
     CompleteSchedule.from_list (forward_sweep @ top @ reverse_sweep @ last_steps)
