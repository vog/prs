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

let test_search max_processes max_resources max_reach expected =
  Printf.sprintf "test_search %d %d %d" max_processes max_resources max_reach >:: fun _ ->
    TreeSearch.search max_processes max_resources max_reach
    |> Util.collect
    |> List.map TreeSchedule.to_tree
    |> List.map Draw.tree
    |> Util.concat_lines
    |> assert_equal_string (Util.concat_lines expected)

let tests =
  "TestTreeSearch" >::: [
    test_search 1 1 999 [
      "e";
    ];
    test_search 2 999 4 [
      "e";
      "(e,e)";
      "(e,(e,e))";
      "((e,e),e)";
      "(e,((e,e),e))";
      "((e,(e,e)),e)";
      "(((e,e),e),e)";
    ];
    test_search 3 999 4 [
      "e";
      "(e,e)";
      "(e,(e,e))";
      "((e,e),e)";
      "(e,((e,e),e))";
      "((e,(e,e)),e)";
      "(((e,e),e),e)";
      "((e,e),(e,e))";
    ];
    test_search 999 999 4 [
      "e";
      "(e,e)";
      "(e,(e,e))";
      "((e,e),e)";
      "(e,((e,e),e))";
      "((e,(e,e)),e)";
      "(((e,e),e),e)";
      "((e,e),(e,e))";
    ];
    test_search 2 7 999 [
      "e";
      "(e,e)";
      "(e,(e,e))";
      "((e,e),e)";
      "(e,((e,e),e))";
      "((e,(e,e)),e)";
      "(((e,e),e),e)";
      "(e,(((e,e),e),e))";
      "((e,((e,e),e)),e)";
      "(((e,(e,e)),e),e)";
      "((((e,e),e),e),e)";
      "(e,((((e,e),e),e),e))";
      "((e,(((e,e),e),e)),e)";
      "(((e,((e,e),e)),e),e)";
      "((((e,(e,e)),e),e),e)";
      "(((((e,e),e),e),e),e)";
      "(e,(((((e,e),e),e),e),e))";
      "((e,((((e,e),e),e),e)),e)";
      "(((e,(((e,e),e),e)),e),e)";
      "((((e,((e,e),e)),e),e),e)";
      "(((((e,(e,e)),e),e),e),e)";
      "((((((e,e),e),e),e),e),e)";
    ];
    test_search 3 5 999 [
      "e";
      "(e,e)";
      "(e,(e,e))";
      "((e,e),e)";
      "(e,((e,e),e))";
      "((e,(e,e)),e)";
      "(((e,e),e),e)";
      "((e,e),(e,e))";
      "(e,(((e,e),e),e))";
      "((e,((e,e),e)),e)";
      "(((e,(e,e)),e),e)";
      "((e,e),((e,e),e))";
      "((e,e),(e,(e,e)))";
      "((e,(e,e)),(e,e))";
      "((((e,e),e),e),e)";
      "(((e,e),e),(e,e))";
      "(((e,e),(e,e)),e)";
      "(e,(((e,e),(e,e)),e))";
      "((e,e),((e,e),(e,e)))";
      "((e,e),((e,(e,e)),e))";
      "((e,((e,e),e)),(e,e))";
      "((e,(e,e)),((e,e),e))";
      "((e,(e,e)),(e,(e,e)))";
      "(((e,(e,e)),(e,e)),e)";
      "(((e,(e,e)),e),(e,e))";
      "((((e,e),e),(e,e)),e)";
      "((((e,e),e),e),(e,e))";
      "(((e,e),(e,e)),(e,e))";
      "(((e,e),e),((e,e),e))";
      "(((e,e),e),(e,(e,e)))";
      "((e,e),((e,e),((e,e),e)))";
      "((e,e),((e,e),(e,(e,e))))";
      "((e,((e,e),e)),((e,e),e))";
      "((e,((e,e),e)),(e,(e,e)))";
      "(((e,e),e),((e,e),(e,e)))";
      "(((e,(e,e)),(e,e)),(e,e))";
      "(((e,(e,e)),e),((e,e),e))";
      "(((e,(e,e)),e),(e,(e,e)))";
      "((((e,e),e),(e,e)),(e,e))";
      "((((e,e),e),e),((e,e),e))";
      "((((e,e),e),e),(e,(e,e)))";
      "((e,(e,e)),((e,e),(e,e)))";
      "(((e,e),(e,e)),((e,e),e))";
      "(((e,e),(e,e)),(e,(e,e)))";
      "((e,e),((e,(e,e)),((e,e),e)))";
      "((e,e),((e,(e,e)),(e,(e,e))))";
      "((e,(e,e)),((e,e),((e,e),e)))";
      "((e,(e,e)),((e,e),(e,(e,e))))";
      "(((e,e),e),((e,e),((e,e),e)))";
      "(((e,e),e),((e,e),(e,(e,e))))";
      "(((e,e),(e,(e,e))),((e,e),e))";
      "(((e,e),(e,(e,e))),(e,(e,e)))";
      "(((e,(e,e)),(e,e)),((e,e),e))";
      "(((e,(e,e)),(e,e)),(e,(e,e)))";
      "((((e,e),e),(e,e)),((e,e),e))";
      "((((e,e),e),(e,e)),(e,(e,e)))";
    ];
  ]
