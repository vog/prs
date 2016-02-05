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

type state = bool list
type stateset = state list

let list (stateset : stateset) : bool list list =
  stateset

let initial_stateset : stateset =
  [[]]

let rec prime_processes (state1 : state) (state2 : state) : int =
  match state1, state2 with
  | [], _ -> 0
  | _, [] -> 0
  | false::rest1, true::rest2 -> 1 + prime_processes rest1 rest2
  | _::rest1, _::rest2 -> prime_processes rest1 rest2

let rec prime_resources : state -> int = function
  | [] -> 0
  | false::rest -> prime_resources rest
  | true::rest -> 1 + prime_resources rest

let predecessors max_processes max_resources state =
  let state_appended = List.append state [true] in
  let satisfies_resources pred =
    (prime_processes pred state_appended <= (Unit.prime_processes max_processes))
    && (prime_resources pred <= (Unit.prime_resources max_resources)) in
  let result = ref [] in
  let rec preds (state : state) (aux_first : bool) (aux : state) : unit =
    let new_aux = aux_first::aux in
    match state, aux_first with
    | [], _ ->
       let new_pred = List.rev new_aux in
       if satisfies_resources new_pred then
         result := new_pred::!result;
    | false::false::_, true ->
       ()
    | false::(true::_ | [] as rest), true ->
       preds rest false new_aux
    | _::(true::_ | [] as rest), false ->
       preds rest true new_aux
    | _::rest, _ ->
       (preds rest true new_aux;
        preds rest false new_aux)
  in
  preds state true [];
  preds state false [];
  !result

let stateset_predecessors (max_processes : Unit.processes) (max_resources : Unit.resources) (states : stateset) : stateset =
  Util.sort_uniq
    compare
    (Util.safe_concat
       (Util.safe_map
          (predecessors max_processes max_resources)
          states
       )
    )

let greater (state1 : state) (state2 : state) : bool =
  let rec aux found state1 state2 =
    match state1, state2 with
    | _::_, [] -> false
    | [], _::_ -> false
    | false::_, true::_ -> false
    | true::rest1, false::rest2 -> aux true rest1 rest2
    | _::rest1, _::rest2 -> aux found rest1 rest2
    | [], [] -> found in
  aux false state1 state2

let greater_some (state : state) (states : stateset) : bool =
  List.exists (greater state) states

let stateset_min_predecessors max_processes max_resources states =
  let preds = stateset_predecessors max_processes max_resources states in
  List.filter
    (fun pred -> not (greater_some pred preds))
    preds

let calc_l_max max_processes max_resources progress =
  let progress_line line = progress (line ^ "\n") in
  let p format = Format.ksprintf progress_line format in
  p "p =%d s =%d" (Unit.processes max_processes) (Unit.resources max_resources);
  p "p'=%d s'=%d" (Unit.prime_processes max_processes) (Unit.prime_resources max_resources);
  let max_steps_ref = ref (Unit.from_steps 4) in
  let stateset = ref initial_stateset in
  let continue = ref true in
  while !continue do
    let preds = stateset_min_predecessors max_processes max_resources !stateset in
    let preds_list = list preds in
    match preds_list with
    | [] ->
       let max_steps = !max_steps_ref in
       p "";
       p "p =%d s =%d l=%d" (Unit.processes max_processes) (Unit.resources max_resources) (Unit.steps max_steps);
       p "p'=%d s'=%d" (Unit.prime_processes max_processes) (Unit.prime_resources max_resources);
       continue := false
    | _::_ ->
       p "";
       p "%d predecessors:" (List.length preds_list);
       p "";
       List.iter
         (fun pred ->
          max_steps_ref := Unit.from_prime_steps (List.length pred);
          p "%s (%d)" (Draw.prime_state pred) (Unit.steps !max_steps_ref)
         )
         preds_list;
       stateset := preds
  done

let calc_l_max_result max_processes max_resources =
  let buffer = Buffer.create 1024 in
  let () = calc_l_max max_processes max_resources (Buffer.add_string buffer) in
  Buffer.contents buffer
