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

let test_enumerate steps expected_tree_string_list =
  Printf.sprintf "test_enumerate_%d" (Unit.steps steps) >::
    fun _ ->
    OldTree.enumerate (Unit.from_processes 100) (Unit.from_resources 100) steps
    |> Util.collect
    |> List.map TreeSchedule.to_tree
    |> List.map Draw.tree
    |> Util.concat_lines
    |> assert_equal_string (Util.concat_lines expected_tree_string_list)

let test_search processes max_resources expected_tree_string_lists =
  Printf.sprintf "test_search_%d_%d" (Unit.processes processes) (Unit.resources max_resources) >::
    fun _ ->
    OldTree.search processes max_resources
    |> Util.collect
    |> List.map (List.map TreeSchedule.to_tree)
    |> List.map (List.map Draw.tree)
    |> assert_equal expected_tree_string_lists

let tests =
  "TestOldTree" >:::
    [
      (* test_enumerate *)
      test_enumerate
        (Unit.from_steps 1)
        [
          "e";
        ];
      test_enumerate
        (Unit.from_steps 2)
        [
          "(e,e)";
        ];
      test_enumerate
        (Unit.from_steps 3)
        [
          "(e,(e,e))";
          "((e,e),e)";
        ];
      test_enumerate
        (Unit.from_steps 4)
        [
          "(e,(e,(e,e)))";
          "(e,((e,e),e))";
          "((e,e),(e,e))";
          "((e,(e,e)),e)";
          "(((e,e),e),e)";
        ];
      test_enumerate
        (Unit.from_steps 5)
        [
          "(e,(e,(e,(e,e))))";
          "(e,(e,((e,e),e)))";
          "(e,((e,e),(e,e)))";
          "(e,((e,(e,e)),e))";
          "(e,(((e,e),e),e))";
          "((e,e),(e,(e,e)))";
          "((e,e),((e,e),e))";
          "((e,(e,e)),(e,e))";
          "(((e,e),e),(e,e))";
          "((e,(e,(e,e))),e)";
          "((e,((e,e),e)),e)";
          "(((e,e),(e,e)),e)";
          "(((e,(e,e)),e),e)";
          "((((e,e),e),e),e)";
        ];
      test_enumerate
        (Unit.from_steps 6)
        [
          "(e,(e,(e,(e,(e,e)))))";
          "(e,(e,(e,((e,e),e))))";
          "(e,(e,((e,e),(e,e))))";
          "(e,(e,((e,(e,e)),e)))";
          "(e,(e,(((e,e),e),e)))";
          "(e,((e,e),(e,(e,e))))";
          "(e,((e,e),((e,e),e)))";
          "(e,((e,(e,e)),(e,e)))";
          "(e,(((e,e),e),(e,e)))";
          "(e,((e,(e,(e,e))),e))";
          "(e,((e,((e,e),e)),e))";
          "(e,(((e,e),(e,e)),e))";
          "(e,(((e,(e,e)),e),e))";
          "(e,((((e,e),e),e),e))";
          "((e,e),(e,(e,(e,e))))";
          "((e,e),(e,((e,e),e)))";
          "((e,e),((e,e),(e,e)))";
          "((e,e),((e,(e,e)),e))";
          "((e,e),(((e,e),e),e))";
          "((e,(e,e)),(e,(e,e)))";
          "((e,(e,e)),((e,e),e))";
          "(((e,e),e),(e,(e,e)))";
          "(((e,e),e),((e,e),e))";
          "((e,(e,(e,e))),(e,e))";
          "((e,((e,e),e)),(e,e))";
          "(((e,e),(e,e)),(e,e))";
          "(((e,(e,e)),e),(e,e))";
          "((((e,e),e),e),(e,e))";
          "((e,(e,(e,(e,e)))),e)";
          "((e,(e,((e,e),e))),e)";
          "((e,((e,e),(e,e))),e)";
          "((e,((e,(e,e)),e)),e)";
          "((e,(((e,e),e),e)),e)";
          "(((e,e),(e,(e,e))),e)";
          "(((e,e),((e,e),e)),e)";
          "(((e,(e,e)),(e,e)),e)";
          "((((e,e),e),(e,e)),e)";
          "(((e,(e,(e,e))),e),e)";
          "(((e,((e,e),e)),e),e)";
          "((((e,e),(e,e)),e),e)";
          "((((e,(e,e)),e),e),e)";
          "(((((e,e),e),e),e),e)";
        ];
      (* test_search *)
      test_search
        (Unit.from_processes 1)
        (Unit.from_resources 1)
        [
          [
            (* p=1, k=1, l=1 *)
            "e";
          ];
        ];
      test_search
        (Unit.from_processes 2)
        (Unit.from_resources 7)
        [
          [
            (* p=2, k=2, l=2 *)
            "(e,e)";
          ];
          [
            (* p=2, k=3, l=3 *)
            "(e,(e,e))";
            "((e,e),e)";
          ];
          [
            (* p=2, k=4, l=4 *)
            "(e,((e,e),e))";
            "((e,(e,e)),e)";
            "(((e,e),e),e)";
          ];
          [
            (* p=2, k=5, l=5 *)
            "(e,(((e,e),e),e))";
            "((e,((e,e),e)),e)";
            "(((e,(e,e)),e),e)";
            "((((e,e),e),e),e)";
          ];
          [
            (* p=2, k=6, l=6 *)
            "(e,((((e,e),e),e),e))";
            "((e,(((e,e),e),e)),e)";
            "(((e,((e,e),e)),e),e)";
            "((((e,(e,e)),e),e),e)";
            "(((((e,e),e),e),e),e)";
          ];
          [
            (* p=2, k=7, l=7 *)
            "(e,(((((e,e),e),e),e),e))";
            "((e,((((e,e),e),e),e)),e)";
            "(((e,(((e,e),e),e)),e),e)";
            "((((e,((e,e),e)),e),e),e)";
            "(((((e,(e,e)),e),e),e),e)";
            "((((((e,e),e),e),e),e),e)";
          ];
        ];
      test_search
        (Unit.from_processes 3)
        (Unit.from_resources 5)
        [
          [
            (* p=3, k=4, l=5 *)
            "(e,(e,(e,(e,e))))";
            "(e,((e,e),(e,e)))";
            "((e,e),(e,(e,e)))";
            "((e,(e,e)),(e,e))";
            "(((e,e),e),(e,e))";
          ];
          [
            (* p=3, k=5, l=8 *)
            "(e,(e,((e,(e,e)),(e,(e,e)))))";
            "(e,(e,((e,(e,e)),((e,e),e))))";
            "(e,((e,e),((e,e),(e,(e,e)))))";
            "(e,((e,e),((e,e),((e,e),e))))";
            "(e,((e,(e,(e,e))),(e,(e,e))))";
            "(e,((e,(e,(e,e))),((e,e),e)))";
            "(e,(((e,e),(e,e)),(e,(e,e))))";
            "(e,(((e,e),(e,e)),((e,e),e)))";
            "((e,e),((e,(e,e)),(e,(e,e))))";
            "((e,e),((e,(e,e)),((e,e),e)))";
            "((e,(e,e)),((e,e),(e,(e,e))))";
            "((e,(e,e)),((e,e),((e,e),e)))";
            "(((e,e),e),((e,e),(e,(e,e))))";
            "(((e,e),e),((e,e),((e,e),e)))";
            "((e,(e,(e,(e,e)))),(e,(e,e)))";
            "((e,(e,(e,(e,e)))),((e,e),e))";
            "((e,((e,e),(e,e))),(e,(e,e)))";
            "((e,((e,e),(e,e))),((e,e),e))";
            "(((e,e),(e,(e,e))),(e,(e,e)))";
            "(((e,e),(e,(e,e))),((e,e),e))";
            "(((e,(e,e)),(e,e)),(e,(e,e)))";
            "(((e,(e,e)),(e,e)),((e,e),e))";
            "((((e,e),e),(e,e)),(e,(e,e)))";
            "((((e,e),e),(e,e)),((e,e),e))";
          ];
        ];
      test_search
        (Unit.from_processes 4)
        (Unit.from_resources 5)
        [
          [
            (* p=4, k=5, l=8 *)
            "(e,(e,(e,((e,e),(e,(e,e))))))";
            "(e,(e,(e,((e,e),((e,e),e)))))";
            "((e,e),(e,((e,e),(e,(e,e)))))";
            "((e,e),(e,((e,e),((e,e),e))))";
          ];
        ];
    ]
