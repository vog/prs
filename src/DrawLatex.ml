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

let tree (tree : Tree.t) : string =
  Tree.map "\\e" (Printf.sprintf "(%s,%s)") tree

let profile (f : Profile.t) : string =
  let coeff i a =
    match i, a with
    | 0, 1 -> ""
    | _, 1 -> "+"
    | _, -1 -> "-"
    | 0, a -> Printf.sprintf "%d" a
    | _, a -> Printf.sprintf "%+d" a
  in
  let aux i (a, t) = Printf.sprintf "%s\\p{%d}" (coeff i a) t in
  match Profile.to_list f with
  | [] -> "0"
  | l -> l |> List.mapi aux |> String.concat ""

let symbol = function
  | CompleteSchedule.Nothing ->
     "\\dN"
  | CompleteSchedule.Checkpoint ->
     "\\dC"
  | CompleteSchedule.Forward ->
     "\\dF"
  | CompleteSchedule.Prepare ->
     "\\dP"
  | CompleteSchedule.Reverse ->
     "\\dR"

let schedule (complete_schedule : CompleteSchedule.t) : string =
  let tpl = Tpl.create () in
  let s format = Tpl.string tpl format in
  let l format = Tpl.line tpl format in
  l "\\begin{Schedule}";
  l "{";
  CompleteSchedule.iter_steps_rev complete_schedule (fun _ iter_times ->
    iter_times (fun time transition ->
      if time >= 1 then
        s " & ";
      s "%s" (symbol transition);
    );
    l " \\\\";
  );
  l "};";
  l "\\end{Schedule}";
  Tpl.result tpl
