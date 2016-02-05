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

let test_complete_schedule prefix =
  let expected_filename = Printf.sprintf "TestTreeData/%s_ascii_letters.txt" prefix in
  expected_filename >:: fun _ ->
    Printf.sprintf "TestTreeData/%s_tree.txt" prefix
    |> Util.read_all
    |> Parse.tree
    |> Tree.to_schedule
    |> Draw.ascii_letters
    |> assert_equal_file expected_filename

let test_roundtrip tree_string =
  Printf.sprintf "test_roundtrip %s" tree_string >:: fun _ ->
    let tree = Parse.tree tree_string in
    assert_equal_string tree_string (Draw.tree tree)

let test_reach tree_string expected =
  Printf.sprintf "test_reach %s" tree_string >:: fun _ ->
    tree_string
    |> Parse.tree
    |> Tree.reach
    |> assert_equal_int expected

let tests =
  "TestTree" >:::
    [
      test_complete_schedule "01";
      test_complete_schedule "02";
      test_complete_schedule "03";
      test_complete_schedule "04";
      test_complete_schedule "05";
      test_complete_schedule "06";
      test_complete_schedule "07";
      test_complete_schedule "08";
      test_complete_schedule "09";
      test_complete_schedule "10";
      test_complete_schedule "11";
      test_complete_schedule "12";
      test_complete_schedule "13";
      test_complete_schedule "14";
      test_roundtrip "e";
      test_roundtrip "(e,e)";
      test_roundtrip "(e,(e,e))";
      test_roundtrip "((e,e),e)";
      test_roundtrip "(e,(e,(e,e)))";
      test_roundtrip "(e,((e,e),e))";
      test_roundtrip "((e,e),(e,e))";
      test_roundtrip "((e,(e,e)),e)";
      test_roundtrip "(((e,e),e),e)";
      test_roundtrip "(e,(e,(e,(e,e))))";
      test_roundtrip "(e,(e,((e,e),e)))";
      test_roundtrip "(e,((e,e),(e,e)))";
      test_roundtrip "(e,((e,(e,e)),e))";
      test_roundtrip "(e,(((e,e),e),e))";
      test_roundtrip "((e,e),(e,(e,e)))";
      test_roundtrip "((e,e),((e,e),e))";
      test_roundtrip "((e,(e,e)),(e,e))";
      test_roundtrip "(((e,e),e),(e,e))";
      test_roundtrip "((e,(e,(e,e))),e)";
      test_roundtrip "((e,((e,e),e)),e)";
      test_roundtrip "(((e,e),(e,e)),e)";
      test_roundtrip "(((e,(e,e)),e),e)";
      test_roundtrip "((((e,e),e),e),e)";
      test_reach "e" 1;
      test_reach "(e,e)" 2;
      test_reach "((e,e),e)" 3;
      test_reach "(e,(e,(e,e)))" 4;
      test_reach "(e,(e,(e,(e,e))))" 5;
    ]
