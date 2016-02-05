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

let test_from_to_string s expected =
  Printf.sprintf "test_from_to_string %s" s >:: fun _ ->
    Parse.profile s
    |> Draw.profile
    |> assert_equal_string expected

let test_zero =
  Printf.sprintf "test_zero" >:: fun _ ->
    Profile.zero
    |> Draw.profile
    |> assert_equal_string "0"

let test_p a t expected =
  Printf.sprintf "test_p %d %d" a t >:: fun _ ->
    Profile.p a t
    |> Draw.profile
    |> assert_equal_string expected

let test_scalarmul a f expected =
  Printf.sprintf "test_p %d %s" a f >:: fun _ ->
    Profile.scalarmul a (Parse.profile f)
    |> Draw.profile
    |> assert_equal_string expected

let test_add f g expected =
  Printf.sprintf "test_add %s %s" f g >:: fun _ ->
    Profile.add (Parse.profile f) (Parse.profile g)
    |> Draw.profile
    |> assert_equal_string expected

let test_shift f t expected =
  Printf.sprintf "test_shift %s %d" f t >:: fun _ ->
    Profile.shift (Parse.profile f) t
    |> Draw.profile
    |> assert_equal_string expected

let test_values f expected =
  Printf.sprintf "test_values %s" f >:: fun _ ->
    f
    |> Parse.profile
    |> Profile.values
    |> List.map (Printf.sprintf "%d")
    |> String.concat ","
    |> assert_equal_string expected

let test_nonnegative f expected =
  Printf.sprintf "test_nonnegative %s" f >:: fun _ ->
    Profile.nonnegative (Parse.profile f)
    |> assert_equal_bool expected

let test_duration f expected =
  Printf.sprintf "test_duration %s" f >:: fun _ ->
    f
    |> Parse.profile
    |> Profile.duration
    |> assert_equal_int expected

let test_max f expected =
  Printf.sprintf "test_max %s" f >:: fun _ ->
    f
    |> Parse.profile
    |> Profile.max
    |> assert_equal_int expected

let test_negate f expected =
  Printf.sprintf "test_negate %s" f >:: fun _ ->
    Profile.negate (Parse.profile f)
    |> Draw.profile
    |> assert_equal_string expected

let test_sum l expected =
  Printf.sprintf "test_sum [ %s ]" (String.concat " ; " l) >:: fun _ ->
    List.map Parse.profile l
    |> Profile.sum
    |> Draw.profile
    |> assert_equal_string expected

let test_sub f g expected =
  Printf.sprintf "test_sub %s %s" f g >:: fun _ ->
    Profile.sub (Parse.profile f) (Parse.profile g)
    |> Draw.profile
    |> assert_equal_string expected

let test_le f g expected =
  Printf.sprintf "test_le %s %s" f g >:: fun _ ->
    Profile.le (Parse.profile f) (Parse.profile g)
    |> assert_equal_bool expected

let test_arrange f g expected =
  Printf.sprintf "test_arrange %s %s" f g >:: fun _ ->
    Profile.arrange (Parse.profile f) (Parse.profile g)
    |> Draw.profile
    |> assert_equal_string expected

let test_e =
  Printf.sprintf "test_e" >:: fun _ ->
    Profile.e
    |> Draw.profile
    |> assert_equal_string "+1@0,-1@2"

let test_combine_processes f g expected =
  Printf.sprintf "test_combine_processes %s %s" f g >:: fun _ ->
    Profile.combine_processes (Parse.profile f) (Parse.profile g)
    |> Draw.profile
    |> assert_equal_string expected

let test_combine_resources f g expected =
  Printf.sprintf "test_combine_resources %s %s" f g >:: fun _ ->
    Profile.combine_resources (Parse.profile f) (Parse.profile g)
    |> Draw.profile
    |> assert_equal_string expected

let tests =
  "TestProfile" >::: [
    test_from_to_string "1@1" "+1@1";
    test_from_to_string "+1@1" "+1@1";
    test_from_to_string "-1@0" "-1@0";
    test_from_to_string "1@0,-1@2" "+1@0,-1@2";
    test_from_to_string "+1@0,-1@2" "+1@0,-1@2";
    test_from_to_string "+1@2,+1@0,-1@2" "+1@0";
    test_from_to_string "+2@2,+1@0,-1@2" "+1@0,+1@2";
    test_from_to_string "" "0";
    test_from_to_string "0" "0";
    test_from_to_string "+2@0,+1@3,-2@0,-1@3" "0";
    test_from_to_string "+2@0,+1@3,-2@0,-2@3" "-1@3";
    test_zero;
    test_p 2 1 "+2@1";
    test_p (-3) 0 "-3@0";
    test_p 0 1 "0";
    test_scalarmul 1 "0" "0";
    test_scalarmul (-3) "0" "0";
    test_scalarmul 1 "+1@2" "+1@2";
    test_scalarmul (-2) "+3@1" "-6@1";
    test_scalarmul 3 "+2@0,-1@2" "+6@0,-3@2";
    test_scalarmul (-1) "-2@0,+1@2" "+2@0,-1@2";
    test_scalarmul 0 "+1@2" "0";
    test_scalarmul 0 "+1@2,-1@3" "0";
    test_add "0" "0" "0";
    test_add "0" "+1@0,-1@2" "+1@0,-1@2";
    test_add "+1@3,-1@4" "0" "+1@3,-1@4";
    test_add "+1@0,-1@2" "+1@0,-1@2" "+2@0,-2@2";
    test_add "+2@0,-1@2,-1@3" "+1@1,-1@3" "+2@0,+1@1,-1@2,-2@3";
    test_add "+2@0,-1@2,-1@3" "+1@1,+1@3" "+2@0,+1@1,-1@2";
    test_add "+2@0,-1@2,-1@3" "+1@1,+1@2,-1@3" "+2@0,+1@1,-2@3";
    test_add "+2@2,-1@6,-1@9" "+1@3,+1@6,+1@9" "+2@2,+1@3";
    test_add "+2@1,-1@3,-1@4" "-2@1,+1@3,+1@4" "0";
    test_shift "0" 0 "0";
    test_shift "0" 1 "0";
    test_shift "0" 2 "0";
    test_shift "+1@0" 0 "+1@0";
    test_shift "+1@0" 2 "+1@2";
    test_shift "+1@1" 0 "+1@1";
    test_shift "+1@1" 2 "+1@3";
    test_shift "+2@2,-2@4" 0 "+2@2,-2@4";
    test_shift "+2@2,-2@4" 2 "+2@4,-2@6";
    test_shift "+2@2,-1@3,-1@7" 0 "+2@2,-1@3,-1@7";
    test_shift "+2@2,-1@3,-1@7" 1 "+2@3,-1@4,-1@8";
    test_values "0" "0";
    test_values "+1@2,-1@2" "0";
    test_values "+1@2,+1@0,-1@2" "0,1";
    test_values "+2@0,-1@2,-1@3" "0,2,1,0";
    test_values "+1@0,+1@2,-3@3,-1@4" "0,1,2,-1,-2";
    test_nonnegative "0" true;
    test_nonnegative "+2@0,-1@2,-1@3" true;
    test_nonnegative "+2@1,-1@2" true;
    test_nonnegative "+2@0,-3@2" false;
    test_nonnegative "+2@0,-1@2,-2@3" false;
    test_nonnegative "-2@1,+3@2" false;
    test_nonnegative "+1@0,-2@1,+1@2" false;
    test_duration "0" 0;
    test_duration "+1@0" 0;
    test_duration "+1@0,-1@4" 4;
    test_duration "+1@0,+1@2,-3@4" 4;
    test_duration "-2@5" 5;
    test_duration "+1@5" 5;
    test_max "0" 0;
    test_max "+1@2,-1@2" 0;
    test_max "+1@2,+1@0,-1@2" 1;
    test_max "+2@0,-1@2,-1@3" 2;
    test_max "+1@0,+1@2,-3@3,-1@4" 2;
    test_negate "0" "0";
    test_negate "+1@0" "-1@0";
    test_negate "+3@5" "-3@5";
    test_negate "+3@1,-2@2,-1@4" "-3@1,+2@2,+1@4";
    test_sum [] "0";
    test_sum ["0"] "0";
    test_sum ["+2@0,-1@2,-1@3"] "+2@0,-1@2,-1@3";
    test_sum ["+2@0,-1@2,-1@3"; "+1@1,-1@3"] "+2@0,+1@1,-1@2,-2@3";
    test_sum ["+2@0,-1@2,-1@3"; "0"; "+1@1,-1@3"] "+2@0,+1@1,-1@2,-2@3";
    test_sum ["+2@0,-1@2,-1@3"; "0"; "+1@1,-1@3"; "+2@3"] "+2@0,+1@1,-1@2";
    test_sub "0" "0" "0";
    test_sub "0" "-1@0,+1@2" "+1@0,-1@2";
    test_sub "+1@3,-1@4" "0" "+1@3,-1@4";
    test_sub "+1@0,-1@2" "-1@0,+1@2" "+2@0,-2@2";
    test_sub "+2@0,-1@2,-1@3" "-1@1,+1@3" "+2@0,+1@1,-1@2,-2@3";
    test_sub "+2@0,-1@2,-1@3" "-1@1,-1@3" "+2@0,+1@1,-1@2";
    test_sub "+2@0,-1@2,-1@3" "-1@1,-1@2,+1@3" "+2@0,+1@1,-2@3";
    test_sub "+2@2,-1@6,-1@9" "-1@3,-1@6,-1@9" "+2@2,+1@3";
    test_sub "+2@1,-1@3,-1@4" "+2@1,-1@3,-1@4" "0";
    test_le "0" "0" true;
    test_le "0" "+2@0,-1@2,-1@3" true;
    test_le "0" "+2@1,-1@2" true;
    test_le "0" "+2@0,-3@2" false;
    test_le "0" "+2@0,-1@2,-2@3" false;
    test_le "0" "-2@1,+3@2" false;
    test_le "0" "+1@0,-2@1,+1@2" false;
    test_le "-2@0,+1@2,+1@3" "0" true;
    test_le "-2@1,+1@2" "0" true;
    test_le "+2@0,-3@2" "0" false;
    test_le "+2@0,-1@2,-2@3" "0" false;
    test_le "-2@1,+3@2" "0" false;
    test_le "+1@0,-2@1,+1@2" "0" false;
    test_le "+1@0,-1@2" "-1@0,+1@2" false;
    test_le "-1@0,+1@2" "+1@0,-1@2" true;
    test_le "+1@0,-1@2" "+1@0,-1@2" true;
    test_le "+1@0,-1@2" "+1@0,-1@3" true;
    test_le "+1@0,-1@3" "+1@0,-1@2" false;
    test_le "+1@0,-1@2,-1@4" "+1@0,-1@2" true;
    test_le "+1@0,-1@2,-1@4" "+1@0,-1@2,-1@4" true;
    test_le "+1@0,-1@2,-1@4" "+1@0" true;
    test_le "+1@0,-1@2,-1@4" "0" false;
    test_arrange "+1@0,-1@2" "+1@0,-1@2" "+1@1,+1@2,-1@3,-1@4";
    test_arrange "+1@0,-1@2" "+1@1,+1@2,-1@3,-1@4" "+1@2,+1@3,-1@5,-1@6";
    test_arrange "+1@1,+1@2,-1@3,-1@4" "+1@0,-1@2" "+1@2,+1@3,-1@5,-1@6";
    test_arrange "+1@0,-1@2" "+1@2,+1@3,-1@5,-1@6" "+1@3,+1@4,-1@7,-1@8";
    test_arrange "+1@0,-1@2" "+1@3,+1@4,-1@7,-1@8" "+1@4,+1@5,-1@9,-1@10";
    test_arrange "+1@1,+1@2,-1@3,-1@4" "+1@1,+1@2,-1@3,-1@4" "+1@3,+1@4,-1@7,-1@8";
    test_arrange "+1@1,+1@2,-1@3,-1@4" "+1@2,+1@3,-1@5,-1@6" "+1@4,+1@5,-1@9,-1@10";
    test_arrange "+1@2,+1@3,-1@5,-1@6" "+1@1,+1@2,-1@3,-1@4" "+1@4,+1@5,-1@9,-1@10";
    test_e;
    test_combine_processes "+1@0,-1@2" "+1@0,-1@2" "+1@0,+1@2,-1@3,-1@4";
    test_combine_processes "+1@0,+1@2,-1@3,-1@4" "+1@0,-1@2" "+1@0,+1@3,-1@5,-1@6";
    test_combine_processes "+1@0,-1@2" "+1@0,+1@2,-1@3,-1@4" "+1@0,+1@2,-1@5,-1@6";
    test_combine_resources "+1@0,-1@2" "+1@0,-1@2" "+2@0,-1@3,-1@4";
    test_combine_resources "+2@0,-1@3,-1@4" "+1@0,-1@2" "+2@0,+1@1,-1@4,-1@5,-1@6";
    test_combine_resources "+1@0,-1@2" "+2@0,-1@3,-1@4" "+2@0,+1@2,-1@4,-1@5,-1@6";
  ]
