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

let test_tree tree expected =
  Printf.sprintf "test_tree %s" tree >:: fun _ ->
    tree
    |> Parse.tree
    |> DrawLatex.tree
    |> assert_equal_string expected

let test_profile profile expected =
  Printf.sprintf "test_profile %s" profile >:: fun _ ->
    profile
    |> Parse.profile
    |> DrawLatex.profile
    |> assert_equal_string expected

let test_schedule prefix =
  let expected_filename = Printf.sprintf "TestDrawLatexData/%s_latex.txt" prefix in
  Printf.sprintf "test_schedule %s" expected_filename >:: fun _ ->
    Printf.sprintf "TestDrawLatexData/%s_tree.txt" prefix
    |> Util.read_all
    |> Parse.tree
    |> Tree.to_schedule
    |> DrawLatex.schedule
    |> assert_equal_file expected_filename

let tests =
  "TestDraw" >::: [
    test_tree "e" "\\e";
    test_tree "(e,(e,e))" "(\\e,(\\e,\\e))";
    test_profile "0" "0";
    test_profile "+1@0" "\\p{0}";
    test_profile "+2@0" "2\\p{0}";
    test_profile "-1@0,-2@3" "-\\p{0}-2\\p{3}";
    test_profile "-2@0,+1@3" "-2\\p{0}+\\p{3}";
    test_profile "+1@0,+1@12,+2@13,-1@19,-2@23,-1@24" "\\p{0}+\\p{12}+2\\p{13}-\\p{19}-2\\p{23}-\\p{24}";
    test_schedule "01";
    test_schedule "02";
    test_schedule "03";
    test_schedule "04";
    test_schedule "05";
    test_schedule "06";
  ]
