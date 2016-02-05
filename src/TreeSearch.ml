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

let combine_trees
    (max_processes : int)
    (max_resources : int)
    (left : TreeSchedule.t list)
    (right : TreeSchedule.t list)
    : TreeSchedule.t list =
  let pred = TreeSchedule.within_bounds max_processes max_resources in
  left
  |> List.filter (fun left_tree ->
    TreeSchedule.resources left_tree <= max_resources - 1
  )
  |> List.map (fun left_tree ->
    right
    |> List.map (fun right_tree ->
      (
        if TreeSchedule.combine_makes_sense left_tree right_tree then
          [TreeSchedule.combine left_tree right_tree]
        else
          []
      )
      |> List.filter pred
    )
    |> List.flatten
  )
  |> List.flatten

let next_reach_trees
    (max_processes : int)
    (max_resources : int)
    (reach_trees : TreeSchedule.t list list)
    : TreeSchedule.t list =
  List.map2 (combine_trees max_processes max_resources) reach_trees (List.rev reach_trees)
  |> List.flatten
  |> MinList.from_list
  |> MinList.to_list
  |> List.sort compare

let current_max_reach
    (reach_trees : TreeSchedule.t list list) : int =
  List.length reach_trees

let rec search_next
    (max_processes : int)
    (max_resources : int)
    (max_reach : int)
    (reach_trees : TreeSchedule.t list list)
    (callback : TreeSchedule.t -> unit)
    : unit =
  let () = reach_trees |> List.hd |> List.iter callback in
  if current_max_reach reach_trees < max_reach then
    let next_trees = next_reach_trees max_processes max_resources reach_trees in
    if next_trees != [] then
      search_next max_processes max_resources max_reach (next_trees :: reach_trees) callback

let search
    (max_processes : int)
    (max_resources : int)
    (max_reach : int)
    (callback : TreeSchedule.t -> unit)
    : unit =
  search_next max_processes max_resources max_reach [[TreeSchedule.e]] callback
