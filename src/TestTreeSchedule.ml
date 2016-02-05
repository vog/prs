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

let test_e =
  Printf.sprintf "test_e" >:: fun _ ->
    TreeSchedule.e
    |> TreeSchedule.to_tree
    |> Draw.tree
    |> assert_equal_string "e"

let test_from_to_tree s =
  Printf.sprintf "test_from_to_tree %s" s >:: fun _ ->
    s
    |> Parse.tree
    |> TreeSchedule.from_tree
    |> TreeSchedule.to_tree
    |> Draw.tree
    |> assert_equal_string s

let test_summary tree_string =
  Printf.sprintf "test_summary %s" tree_string >::
    fun _ ->
    let tree = Parse.tree tree_string in
    let expected_summary =
      tree
      |> Tree.to_schedule
      |> CompleteSchedule.summary in
    tree
    |> TreeSchedule.from_tree
    |> TreeSchedule.summary
    |> assert_equal expected_summary

let test_le f g expected =
  Printf.sprintf "test_le %s %s" f g >:: fun _ ->
    TreeSchedule.le (f |> Parse.tree |> TreeSchedule.from_tree) (g |> Parse.tree |> TreeSchedule.from_tree)
    |> assert_equal_bool expected

let test_within_bounds f max_processes max_resources expected =
  Printf.sprintf "test_within_bounds %s %d %d" f max_processes max_resources >:: fun _ ->
    f
    |> Parse.tree
    |> TreeSchedule.from_tree
    |> TreeSchedule.within_bounds max_processes max_resources
    |> assert_equal_bool expected

let test_combine f g expected =
  Printf.sprintf "test_combine %s %s" f g >:: fun _ ->
    TreeSchedule.combine (f |> Parse.tree |> TreeSchedule.from_tree) (g |> Parse.tree |> TreeSchedule.from_tree)
    |> TreeSchedule.to_tree
    |> Draw.tree
    |> assert_equal_string expected

let test_combine_makes_sense f g expected =
  Printf.sprintf "test_combine_makes_sense %s %s" f g >:: fun _ ->
    TreeSchedule.combine_makes_sense (f |> Parse.tree |> TreeSchedule.from_tree) (g |> Parse.tree |> TreeSchedule.from_tree)
    |> assert_equal_bool expected

let test_duration f expected =
  Printf.sprintf "test_duration %s" f >:: fun _ ->
    TreeSchedule.duration (f |> Parse.tree |> TreeSchedule.from_tree)
    |> assert_equal_int expected

let test_draw f expected =
  Printf.sprintf "test_draw %s" f >:: fun _ ->
    f
    |> Parse.tree
    |> TreeSchedule.from_tree
    |> Draw.tree_schedule
    |> assert_equal_string expected

let tests =
  "TestTreeSchedule" >::: [
    test_e;
    test_from_to_tree "e";
    test_from_to_tree "(e,e)";
    test_from_to_tree "((e,e),e)";
    test_from_to_tree "(e,(e,e))";
    test_from_to_tree "(((e,e),e),e)";
    test_from_to_tree "((e,(e,e)),e)";
    test_from_to_tree "((e,e),(e,e))";
    test_from_to_tree "(e,((e,e),e))";
    test_from_to_tree "(e,(e,(e,e)))";
    test_summary "e";
    test_summary "(e,e)";
    test_summary "(e,(e,e))";
    test_summary "((e,e),e)";
    test_summary "(e,(e,(e,e)))";
    test_summary "(e,((e,e),e))";
    test_summary "((e,e),(e,e))";
    test_summary "((e,(e,e)),e)";
    test_summary "(((e,e),e),e)";
    test_summary "(e,(e,(e,(e,e))))";
    test_summary "(e,(e,((e,e),e)))";
    test_summary "(e,((e,e),(e,e)))";
    test_summary "(e,((e,(e,e)),e))";
    test_summary "(e,(((e,e),e),e))";
    test_summary "((e,e),(e,(e,e)))";
    test_summary "((e,e),((e,e),e))";
    test_summary "((e,(e,e)),(e,e))";
    test_summary "(((e,e),e),(e,e))";
    test_summary "((e,(e,(e,e))),e)";
    test_summary "((e,((e,e),e)),e)";
    test_summary "(((e,e),(e,e)),e)";
    test_summary "(((e,(e,e)),e),e)";
    test_summary "((((e,e),e),e),e)";
    test_le "e" "e" true;
    test_le "e" "(e,e)" true;
    test_le "(e,e)" "e" false;
    test_le "(e,e)" "((e,e),e)" false;
    test_le "((e,e),e)" "(e,e)" false;
    test_le "(e,e)" "(e,(e,e))" true;
    test_le "(e,(e,e))" "(e,e)" false;
    test_le "((e,e),e)" "(e,(e,e))" false;
    test_le "(e,(e,e))" "((e,e),e)" false;
    test_within_bounds "e" 1 1 true;
    test_within_bounds "e" 1 2 true;
    test_within_bounds "e" 2 1 true;
    test_within_bounds "e" 2 2 true;
    test_within_bounds "e" 0 1 false;
    test_within_bounds "e" 1 0 false;
    test_within_bounds "e" 0 0 false;
    test_within_bounds "(e,e)" 2 2 true;
    test_within_bounds "(e,e)" 2 3 true;
    test_within_bounds "(e,e)" 2 1 false;
    test_within_bounds "(e,e)" 1 2 false;
    test_within_bounds "((e,e),e)" 2 3 true;
    test_within_bounds "((e,e),e)" 2 2 false;
    test_within_bounds "((e,e),e)" 1 3 false;
    test_within_bounds "(e,(e,e))" 2 3 true;
    test_within_bounds "(e,(e,e))" 2 2 false;
    test_within_bounds "(e,(e,e))" 1 3 false;
    test_combine "e" "e" "(e,e)";
    test_combine "e" "((e,e),e)" "(e,((e,e),e))";
    test_combine_makes_sense "e" "e" true;
    test_combine_makes_sense "e" "(e,e)" true;
    test_combine_makes_sense "(e,e)" "e" true;
    test_combine_makes_sense "(e,e)" "(e,e)" true;
    test_combine_makes_sense "e" "(e,(e,e))" false;
    test_combine_makes_sense "e" "((e,e),(e,e))" false;
    test_combine_makes_sense "e" "((e,(e,e)),(e,e))" false;
    test_combine_makes_sense "e" "(((e,e),e),(e,e))" false;
    test_combine_makes_sense "e" "(e,((e,e),e))" false;
    test_combine_makes_sense "(e,e)" "(e,(e,e))" true;
    test_combine_makes_sense "(e,e)" "((e,e),(e,e))" true;
    test_combine_makes_sense "(e,e)" "((e,(e,e)),(e,e))" true;
    test_combine_makes_sense "(e,e)" "(((e,e),e),(e,e))" true;
    test_combine_makes_sense "(e,e)" "(e,((e,e),e))" true;
    test_combine_makes_sense "(e,e)" "(e,((e,e),(e,e)))" false;
    test_duration "e" 2;
    test_duration "(e,e)" 4;
    test_duration "((e,e),e)" 6;
    test_duration "(e,(e,e))" 6;
    test_duration "(((e,e),e),e)" 8;
    test_duration "((e,(e,e)),e)" 8;
    test_duration "((e,e),(e,e))" 8;
    test_duration "(e,((e,e),e))" 8;
    test_duration "(e,(e,(e,e)))" 8;
    test_draw "e" "e [p=1, k=1, l=1]\n  p=+1@0,-1@2\n  r=+1@0,-1@2\n";
    test_draw "(e,e)" "(e,e) [p=2, k=2, l=2]\n  p=+1@0,+1@2,-1@3,-1@4\n  r=+2@0,-1@3,-1@4\n";
    test_draw "((e,e),e)" "((e,e),e) [p=2, k=3, l=3]\n  p=+1@0,+1@3,-1@5,-1@6\n  r=+2@0,+1@1,-1@4,-1@5,-1@6\n";
    test_draw "(e,(e,e))" "(e,(e,e)) [p=2, k=3, l=3]\n  p=+1@0,+1@2,-1@5,-1@6\n  r=+2@0,+1@2,-1@4,-1@5,-1@6\n";
  ]
