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

let enumerate (max_processes : Unit.processes) (max_resources : Unit.resources) (steps : Unit.steps) (callback : TreeSchedule.t -> unit) : unit =
  let rec aux steps callback =
    let callback_if_filter s =
      let processes, resources, _ = TreeSchedule.summary s in
      if processes <= max_processes && resources <= max_resources then
        callback s
    in
    match steps with
    | steps when steps <= 0 ->
       ()
    | 1 ->
       callback_if_filter TreeSchedule.e
    | steps ->
       for i = 1 to (steps - 1) do
         aux i (fun a -> aux (steps - i) (fun b -> callback_if_filter (TreeSchedule.combine a b)))
       done
  in
  aux (Unit.steps steps) callback

let has_exact_processes (processes : Unit.processes) (s : TreeSchedule.t) : bool =
  let tree_processes, _, _ = TreeSchedule.summary s in
  tree_processes = processes

let search (processes : Unit.processes) (max_resources : Unit.resources) (callback : TreeSchedule.t list -> unit) : unit =
  let rec aux resources steps best_smaller_trees =
    if resources <= max_resources then
      let trees = enumerate processes resources steps |> Util.collect in
      match trees with
      | [] ->
         let output_trees = List.filter (has_exact_processes processes) best_smaller_trees in
         if output_trees != [] then
           callback output_trees;
         aux (Unit.from_resources (Unit.resources resources + 1)) steps []
      | _::_ ->
         aux resources (Unit.from_steps (Unit.steps steps + 1)) trees
  in
  aux (Unit.from_resources 1) (Unit.from_steps 1) []
