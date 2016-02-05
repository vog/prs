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

let test_generate_failure expected_processes expected_resources =
  Printf.sprintf "test_generate_failure %d %d" expected_processes expected_resources >:: fun _ ->
    let processes = Unit.from_processes expected_processes in
    let resources = Unit.from_resources expected_resources in
    assert_raises Generate.Failure (fun () -> Generate.generate processes resources)

let test_generate_blind_anyreach expected_processes expected_resources =
  Printf.sprintf "test_generate_blind %d %d" expected_processes expected_resources >:: fun _ ->
    let tree = Generate.generate (Unit.from_processes expected_processes) (Unit.from_resources expected_resources) in
    let processes, resources, _ = tree |> TreeSchedule.from_tree |> TreeSchedule.summary in
    assert_bool "processes <= expected_processes" (Unit.processes processes <= expected_processes);
    assert_bool "resources <= expected_resources" (Unit.resources resources <= expected_resources)

let test_generate_blind expected_processes expected_resources expected_steps =
  Printf.sprintf "test_generate_blind %d %d" expected_processes expected_resources >:: fun _ ->
    let tree = Generate.generate (Unit.from_processes expected_processes) (Unit.from_resources expected_resources) in
    let processes, resources, steps = tree |> TreeSchedule.from_tree |> TreeSchedule.summary in
    assert_bool "processes <= expected_processes" (Unit.processes processes <= expected_processes);
    assert_bool "resources <= expected_resources" (Unit.resources resources <= expected_resources);
    assert_equal_int expected_steps (Unit.steps steps)

let test_generate expected_processes expected_resources expected_steps expected_tree_string =
  Printf.sprintf "test_generate %d %d" expected_processes expected_resources >:: fun _ ->
    let tree = Generate.generate (Unit.from_processes expected_processes) (Unit.from_resources expected_resources) in
    let processes, resources, steps = tree |> TreeSchedule.from_tree |> TreeSchedule.summary in
    assert_bool "processes <= expected_processes" (Unit.processes processes <= expected_processes);
    assert_bool "resources <= expected_resources" (Unit.resources resources <= expected_resources);
    assert_equal_int expected_steps (Unit.steps steps);
    assert_equal_string expected_tree_string (Draw.tree tree)

let tests =
  "TestGenerate" >::: [
    (* invalid *)
    test_generate_failure 0 (-1);
    test_generate_failure 0 0;
    test_generate_failure 0 1;
    test_generate_failure 0 2;
    test_generate_failure 1 (-1);
    test_generate_failure 1 0;
    test_generate_failure 2 (-1);
    test_generate_failure 2 0;
    test_generate_failure 2 1;
    test_generate_failure 3 (-1);
    test_generate_failure 3 0;
    test_generate_failure 3 1;
    test_generate_failure 3 2;
    test_generate_failure 4 (-1);
    test_generate_failure 4 0;
    test_generate_failure 4 1;
    test_generate_failure 4 2;
    test_generate_failure 4 3;
    (* p=k *)
    test_generate 1 1 1 "e";
    test_generate 2 2 2 "(e,e)";
    test_generate 3 3 3 "(e,(e,e))";
    test_generate 4 4 5 "((e,e),(e,(e,e)))";
    test_generate 5 5 8 "((e,(e,e)),((e,e),(e,(e,e))))";
    test_generate 6 6 13 "(((e,e),(e,(e,e))),((e,(e,e)),((e,e),(e,(e,e)))))";
    test_generate 7 7 21 "(((e,(e,e)),((e,e),(e,(e,e)))),(((e,e),(e,(e,e))),((e,(e,e)),((e,e),(e,(e,e))))))";
    test_generate_blind 8 8 34;
    (* p = 1 *)
    test_generate 1 1 1 "e";
    test_generate 1 2 1 "e";
    test_generate 1 3 1 "e";
    (* p = 2 *)
    test_generate_blind 2 4 4;
    test_generate_blind 2 5 5;
    test_generate_blind 2 6 6;
    test_generate_blind 2 7 7;
    test_generate_blind 2 8 8;
    test_generate_blind 2 9 9;
    test_generate_blind 2 10 10;
    test_generate_blind 2 11 11;
    test_generate_blind 2 12 12;
    (* p = 3 *)
    test_generate_blind 3 4 5;
    test_generate_blind 3 5 8;
    test_generate_blind 3 6 12;
    test_generate_blind 3 7 17;
    test_generate_blind 3 8 23;
    test_generate_blind 3 9 30;
    test_generate_blind 3 10 38;
    test_generate_blind 3 11 47;
    test_generate_blind 3 12 57;
    (* p = 4 *)
    test_generate_blind 4 5 8;
    test_generate_blind 4 6 13;
    test_generate_blind 4 7 21;
    test_generate_blind 4 8 32;
    test_generate_blind 4 9 46;
    test_generate_blind 4 10 63;
    test_generate_blind 4 11 83;
    test_generate_blind 4 12 106;
    test_generate_blind_anyreach 4 20;
    test_generate_blind_anyreach 4 40;
    test_generate_blind_anyreach 4 80;
    (* p = 5 *)
    test_generate_blind 5 6 12;
    test_generate_blind 5 7 19;
    test_generate_blind 5 8 31;
    test_generate_blind 5 9 48;
    test_generate_blind 5 10 70;
    test_generate_blind 5 11 97;
    test_generate_blind 5 12 129;
    test_generate_blind 5 13 166;
    test_generate_blind_anyreach 5 20;
    test_generate_blind_anyreach 5 40;
    test_generate_blind_anyreach 5 80;
    (* p = 6 *)
    test_generate_blind 6 7 16;
    test_generate_blind 6 8 25;
    test_generate_blind 6 9 41;
    test_generate_blind 6 10 64;
    test_generate_blind 6 11 94;
    test_generate_blind 6 12 131;
    test_generate_blind 6 13 175;
    test_generate_blind 6 14 226;
    test_generate_blind_anyreach 6 20;
    test_generate_blind_anyreach 6 40;
    test_generate_blind_anyreach 6 80;
    (* p = 7 *)
    test_generate_blind_anyreach 7 8;
    test_generate_blind_anyreach 7 9;
    test_generate_blind_anyreach 7 10;
    test_generate_blind_anyreach 7 11;
    test_generate_blind_anyreach 7 12;
    test_generate_blind_anyreach 7 13;
    test_generate_blind_anyreach 7 14;
    test_generate_blind_anyreach 7 15;
    test_generate_blind_anyreach 7 20;
    test_generate_blind_anyreach 7 40;
    test_generate_blind_anyreach 7 80;
    (* p >= 8 *)
    test_generate_blind_anyreach 8 30;
    test_generate_blind_anyreach 9 30;
    test_generate_blind_anyreach 10 30;
    test_generate_blind_anyreach 11 30;
  ]
