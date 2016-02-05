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

type t = int * (int -> int -> transition)

let from_list (l : transition list list) : t =
  let a = l |> List.map Array.of_list |> Array.of_list in
  let steps = Array.length a / 2 in
  let f time step =
    try
      let time_index = time in
      let steps_array = Array.get a time_index in
      Array.get steps_array step
    with
    | Invalid_argument _ -> Nothing
  in
  steps, f

let to_list (steps, f : t) : transition list list =
  let transitions time =
    f time
    |> Util.map_range 0 steps
    |> Util.list_trim_end Nothing
  in
  Util.map_range 0 (2*steps - 1) transitions

let from_func (steps, f : int * (int -> int -> transition)) : t =
  steps, f

let to_func (steps, f : t) : int * (int -> int -> transition) =
  steps, f

let iter_times (steps, f : t) (callback : int -> ((int -> transition -> unit) -> unit) -> unit) : unit =
  for time = 0 to 2*steps - 1 do
    let iter_steps inner_callback =
      for step = 0 to min (time+1) (2*steps - time) do
        inner_callback step (f time step)
      done
    in
    callback time iter_steps
  done

let iter_steps_rev (steps, f : t) (callback : int -> ((int -> transition -> unit) -> unit) -> unit) : unit =
  for step = steps downto 0 do
    let iter_times_rev inner_callback =
      for time = 0 to 2*steps - 1 do
        inner_callback time (f time step)
      done
    in
    callback step iter_times_rev
  done

let processes (transitions : transition list) : Unit.processes =
  let processes_of_transition = function
    | Nothing -> 0
    | Checkpoint -> 0
    | Forward | Prepare | Reverse -> 1
  in
  transitions |> List.map processes_of_transition |> Util.sum |> Unit.from_processes

let resources (transitions : transition list) : Unit.resources =
  let resources_of_transition = function
    | Nothing -> 0
    | Checkpoint -> 1
    | Forward | Prepare | Reverse -> 1
  in
  transitions |> List.map resources_of_transition |> Util.sum |> Unit.from_resources

let profile_processes (complete_schedule : t) : Unit.processes list =
  complete_schedule |> to_list |> List.map processes

let profile_resources (complete_schedule : t) : Unit.resources list =
  complete_schedule |> to_list |> List.map resources

let profile_steps (complete_schedule : t) : Unit.steps list =
  let aux transitions =
    if (Util.last transitions) = Some Reverse then
      Unit.from_steps (List.length transitions - 1)
    else
      Unit.from_steps 0
  in
  complete_schedule |> to_list |> List.map aux

let summary (complete_schedule : t) : Unit.summary =
  let max_processes = complete_schedule |> profile_processes |> Util.max (Unit.from_processes 0) in
  let max_resources = complete_schedule |> profile_resources |> Util.max (Unit.from_resources 0) in
  let max_steps = complete_schedule |> profile_steps |> Util.max (Unit.from_steps 0) in
  max_processes, max_resources, max_steps
