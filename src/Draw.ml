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

let summary (processes, resources, steps) =
  Format.sprintf "p=%d, k=%d, l=%d" (Unit.processes processes) (Unit.resources resources) (Unit.steps steps)

let prime_part (part : bool) : string =
  match part with
  | false -> "."
  | true -> "O"

let prime_state (state : bool list) : string =
  state |> List.map prime_part |> String.concat ""

let prime_schedule (prime_schedule : bool list list) : string =
  prime_schedule |> List.map prime_state |> Util.concat_lines

let ascii (complete_schedule : CompleteSchedule.t) : string =
  let symbol = function
    | CompleteSchedule.Nothing ->
       "  ",
       " ."
    | CompleteSchedule.Checkpoint ->
       " |",
       " O"
    | CompleteSchedule.Forward ->
       "\\ ",
       " O"
    | CompleteSchedule.Prepare ->
       "\\ ",
       " P"
    | CompleteSchedule.Reverse ->
       "/ ",
       "  "
  in
  let line1 transitions = transitions |> List.map symbol |> List.map fst |> String.concat "" |> Util.trim_end in
  let line2 transitions = transitions |> List.map symbol |> List.map snd |> String.concat "" |> Util.trim_end in
  let line transitions =
    match line1 transitions, line2 transitions with
    | "", l2 -> l2
    | l1, l2 -> l1 ^ "\n" ^ l2
  in
  let lines = [" *"] @ (List.map line (CompleteSchedule.to_list complete_schedule)) in
  Util.concat_lines lines

let ascii_small_symbol = function
  | CompleteSchedule.Nothing ->
     "."
  | CompleteSchedule.Checkpoint ->
     "|"
  | CompleteSchedule.Forward ->
     "\\"
  | CompleteSchedule.Prepare ->
     "\\"
  | CompleteSchedule.Reverse ->
     "/"

let ascii_letters_symbol = function
  | CompleteSchedule.Nothing ->
     "."
  | CompleteSchedule.Checkpoint ->
     "C"
  | CompleteSchedule.Forward ->
     "F"
  | CompleteSchedule.Prepare ->
     "P"
  | CompleteSchedule.Reverse ->
     "R"

let ascii_oneline (symbol : CompleteSchedule.transition -> string) (complete_schedule : CompleteSchedule.t) : string =
  let tpl = Tpl.create () in
  let s format = Tpl.string tpl format in
  let l format = Tpl.line tpl format in
  CompleteSchedule.iter_times complete_schedule (fun _ iter_steps ->
    iter_steps (fun _ transition ->
      s "%s" (symbol transition);
    );
    l "";
  );
  Tpl.result tpl

let ascii_oneline_summary (symbol : CompleteSchedule.transition -> string) (complete_schedule : CompleteSchedule.t) : string =
  let processes, resources, steps = CompleteSchedule.summary complete_schedule in
  let line transitions =
    let processes = transitions |> CompleteSchedule.processes |> Unit.processes in
    let resources = transitions |> CompleteSchedule.resources |> Unit.resources in
    let transitions_line = transitions |> List.map symbol |> String.concat "" in
    match processes, resources with
    | 0, 0 ->
       Format.sprintf "%2s %2s  %s" "" "" transitions_line
    | _ ->
       Format.sprintf "%2d %2d  %s" processes resources transitions_line
  in
  Format.sprintf
    "%s\n%2d %2d  %d\n"
    (complete_schedule |> CompleteSchedule.to_list |> List.map line |> Util.concat_lines)
    (Unit.processes processes)
    (Unit.resources resources)
    (Unit.steps steps)

let ascii_small (complete_schedule : CompleteSchedule.t) : string =
  ascii_oneline ascii_small_symbol complete_schedule

let ascii_small_summary (complete_schedule : CompleteSchedule.t) : string =
  ascii_oneline_summary ascii_small_symbol complete_schedule

let ascii_letters (complete_schedule : CompleteSchedule.t) : string =
  ascii_oneline ascii_letters_symbol complete_schedule

let ascii_letters_summary (complete_schedule : CompleteSchedule.t) : string =
  ascii_oneline_summary ascii_letters_symbol complete_schedule

let profile (f : Profile.t) : string =
  let aux (a, t) = Printf.sprintf "%+d@%d" a t in
  let l = Profile.to_list f in
  match l with
  | [] -> "0"
  | l -> l |> List.map aux |> String.concat ","

let tree (tree : Tree.t) : string =
  Tree.map "e" (Printf.sprintf "(%s,%s)") tree

let tree_schedule (f : TreeSchedule.t) : string =
  Printf.sprintf
    "%s [%s]\n  p=%s\n  r=%s\n"
    (f |> TreeSchedule.to_tree |> tree)
    (f |> TreeSchedule.summary |> summary)
    (f |> TreeSchedule.to_proc_profile |> profile)
    (f |> TreeSchedule.to_res_profile |> profile)
