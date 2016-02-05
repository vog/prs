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

let test_empty =
  Printf.sprintf "test_empty" >:: fun _ ->
    MinList.empty
    |> MinList.to_list
    |> List.map TreeSchedule.to_tree
    |> List.map Draw.tree
    |> String.concat " ; "
    |> assert_equal_string ""

let test_add schedules expected =
  Printf.sprintf "test_add %s" (String.concat " ; " schedules) >:: fun _ ->
    schedules
    |> List.map Parse.tree
    |> List.map TreeSchedule.from_tree
    |> List.fold_left MinList.add MinList.empty
    |> MinList.to_list
    |> List.map TreeSchedule.to_tree
    |> List.map Draw.tree
    |> String.concat " ; "
    |> assert_equal_string (String.concat " ; " expected)

let test_list schedules expected =
  Printf.sprintf "test_list %s" (String.concat " ; " schedules) >:: fun _ ->
    schedules
    |> List.map Parse.tree
    |> List.map TreeSchedule.from_tree
    |> MinList.from_list
    |> MinList.to_list
    |> List.map TreeSchedule.to_tree
    |> List.map Draw.tree
    |> String.concat " ; "
    |> assert_equal_string (String.concat " ; " expected)

let tests =
  "TestMinList" >::: [
    test_empty;
    test_add [] [];
    test_add ["e"] ["e"];
    test_add ["e"; "e"] ["e"];
    test_add ["e"; "e" ; "e"] ["e"];
    test_list [] [];
    test_list ["e"] ["e"];
    test_list ["e"; "e"] ["e"];
    test_list ["e"; "e" ; "e"] ["e"];
    test_list ["(e,e)"] ["(e,e)"];
    (* e <= (e,e) *)
    test_list ["e"; "(e,e)"] ["e"];
    test_list ["(e,e)"; "e"] ["e"];
    test_list ["(e,e)"; "e"; "(e,e)"] ["e"];
    (* ((e,e),(e,e)) <= (e,(e,(e,e))) *)
    test_list ["(e,(e,(e,e)))"; "((e,e),(e,e))"] ["((e,e),(e,e))"];
    test_list ["(e,(e,(e,e)))"; "(((e,e),e),e)"] ["(((e,e),e),e)"; "(e,(e,(e,e)))"];
    test_list ["(e,(e,(e,e)))"; "(((e,e),e),e)"; "((e,e),(e,e))"] ["((e,e),(e,e))"; "(((e,e),e),e)"];
  ]
