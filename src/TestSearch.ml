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

open OUnit2
open UtilForTest

let test_predecessors _ =
  let max_processes = Unit.from_processes 4 in
  let max_resources = Unit.from_resources 7 in
  let state1 = Parse.prime_state ".OO" in
  let expected = [
    ".O.O";
    ".OO.";
    ".OOO";
    "O.O.";
    "O.OO";
  ] in
  let result_stateset = Search.predecessors max_processes max_resources state1 in
  let result = result_stateset |> Search.list |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let test_predecessors_constrained_resources _ =
  let max_processes = Unit.from_processes 4 in
  let max_resources = Unit.from_resources 6 in
  let state1 = Parse.prime_state ".OO" in
  let expected = [
    ".O.O";
    ".OO.";
    "O.O.";
  ] in
  let result_stateset = Search.predecessors max_processes max_resources state1 in
  let result = result_stateset |> Search.list |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let test_predecessors_constrained_processes _ =
  let max_processes = Unit.from_processes 3 in
  let max_resources = Unit.from_resources 6 in
  let state1 = Parse.prime_state ".OO" in
  let expected = [
    ".O.O";
    ".OO.";
  ] in
  let result_stateset = Search.predecessors max_processes max_resources state1 in
  let result = result_stateset |> Search.list |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let test_predecessors_bottom _ =
  let max_processes = Unit.from_processes 4 in
  let max_resources = Unit.from_resources 7 in
  let state1 = Parse.prime_state "OO" in
  let expected = [
    ".O.";
    ".OO";
    "O.O";
    "OO.";
    "OOO";
  ] in
  let result_stateset = Search.predecessors max_processes max_resources state1 in
  let result = result_stateset |> Search.list |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let test_counts expected state_string _ =
  let state = Parse.prime_state state_string in
  let result_stateset = Search.predecessors (Unit.from_processes 7) (Unit.from_resources 14) state in
  let real = List.length (Search.list result_stateset) in
  assert_equal_int expected real;
  ()

let test_search max_processes max_resources =
  let expected_filename = Printf.sprintf "TestSearchData/p%03d_s%04d.txt" max_processes max_resources in
  expected_filename >::
    fun _ ->
    Search.calc_l_max_result (Unit.from_processes max_processes) (Unit.from_resources max_resources)
    |> assert_equal_file expected_filename

let tests =
  "TestSearch" >::: [
    "test_predecessors" >:: test_predecessors;
    "test_predecessors_constrained_resources" >:: test_predecessors_constrained_resources;
    "test_predecessors_constrained_processes" >:: test_predecessors_constrained_processes;
    "test_predecessors_bottom" >:: test_predecessors_bottom;
    "test_counts_0" >:: test_counts 2 "";
    "test_counts_1" >:: test_counts 3 "O";
    "test_counts_2" >:: test_counts 5 "OO";
    "test_counts_3" >:: test_counts 8 "OOO";
    "test_counts_4" >:: test_counts 13 "OOOO";
    "test_counts_5" >:: test_counts 21 "OOOOO";
    "test_counts_6" >:: test_counts 34 "OOOOOO";
    "test_counts_7" >:: test_counts 55 "OOOOOOO";
    "test_counts_8" >:: test_counts 89 "OOOOOOOO";
    "test_counts_9" >:: test_counts 144 "OOOOOOOOO";
    "test_counts_composed_2_3" >:: test_counts 15 "OO.OO";
    "test_counts_composed_2_3_larger_gaps" >:: test_counts 15 ".OO..OO";
    test_search 2 4;
    test_search 2 5;
    test_search 2 6;
    test_search 2 7;
    test_search 2 8;
    test_search 2 9;
    test_search 3 4;
    test_search 3 5;
    test_search 3 6;
    test_search 3 7;
    test_search 3 8;
    test_search 4 4;
    test_search 4 5;
    test_search 4 6;
    test_search 4 7;
    test_search 4 8;
    test_search 5 4;
    test_search 5 5;
    test_search 5 6;
    test_search 5 7;
    test_search 6 4;
    test_search 6 5;
    test_search 6 6;
    test_search 6 7;
    test_search 7 4;
    test_search 7 5;
    test_search 7 6;
    test_search 7 7;
  ]
