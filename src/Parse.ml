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

exception Invalid_prime_part_string of string
exception Invalid_fiber_string of string
exception String_does_not_start_with_tree of string
exception Missing_last_newline

let prime_part (s : string) : bool =
  match s with
  | "." -> false
  | "O" -> true
  | s -> raise (Invalid_prime_part_string s)

let prime_state (s : string) : bool list =
  s |> Str.split (Str.regexp "") |> List.map prime_part

let prime_schedule_delim = Str.regexp "\n"

let prime_schedule (s : string) : bool list list =
  match String.length s with
  | 0 ->
     []
  | 1 when s = "\n" ->
     [[]]
  | len when s.[len - 1] = '\n' ->
     String.sub s 0 (len - 1) |> Str.split_delim prime_schedule_delim |> List.map prime_state
  | _ ->
     raise Missing_last_newline

let fiber (s : string) : PrimeSchedule.fiber =
  match s with
  | "c" -> PrimeSchedule.Checkpoint
  | "0" -> PrimeSchedule.Process 0
  | "1" -> PrimeSchedule.Process 1
  | "2" -> PrimeSchedule.Process 2
  | "3" -> PrimeSchedule.Process 3
  | "4" -> PrimeSchedule.Process 4
  | "5" -> PrimeSchedule.Process 5
  | "6" -> PrimeSchedule.Process 6
  | "7" -> PrimeSchedule.Process 7
  | "8" -> PrimeSchedule.Process 8
  | "9" -> PrimeSchedule.Process 9
  | s -> raise (Invalid_fiber_string s)

let fibers (s : string) : PrimeSchedule.fiber list =
  s |> String.trim |> Str.split (Str.regexp "") |> List.map fiber

let profile (s : string) : Profile.t =
  let aux part_s = Scanf.sscanf part_s "%d%@%d%!" Profile.p in
  match s with
  | "0" -> Profile.zero
  | s -> s |> Str.split (Str.regexp ",") |> List.map aux |> Profile.sum

let tree (s : string) : Tree.t =
  let rec aux s =
    try
      match Str.first_chars s 1 with
      | "e" -> Tree.E, 1
      | _ ->
         let a, len_a = aux (Str.string_after s 1) in
         let b, len_b = aux (Str.string_after s (1 + len_a + 1)) in
         Tree.T (a, b), 1 + len_a + 1 + len_b + 1
    with
    | Invalid_argument _ -> raise (String_does_not_start_with_tree s)
  in
  s |> String.trim |> aux |> fst
