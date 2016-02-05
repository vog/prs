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

let test_parse_prime_state_length _ =
  "" |>  Parse.prime_state |> List.length |> assert_equal_int 0;
  "O" |>  Parse.prime_state |> List.length |> assert_equal_int 1;
  ".O" |>  Parse.prime_state |> List.length |> assert_equal_int 2;
  "OO" |>  Parse.prime_state |> List.length |> assert_equal_int 2;
  ()

let test_parse_prime_state _ =
  "" |>  Parse.prime_state |> Draw.prime_state |> assert_equal_string "";
  ".O" |>  Parse.prime_state |> Draw.prime_state |> assert_equal_string ".O";
  "OO" |>  Parse.prime_state |> Draw.prime_state |> assert_equal_string "OO";
  ()

let test_parse_prime_schedule_length _ =
  "" |>  Parse.prime_schedule |> List.length |> assert_equal_int 0;
  "\n" |>  Parse.prime_schedule |> List.length |> assert_equal_int 1;
  ".O\n" |>  Parse.prime_schedule |> List.length |> assert_equal_int 1;
  "OO\n" |>  Parse.prime_schedule |> List.length |> assert_equal_int 1;
  ()

let test_parse_prime_schedule _ =
  "" |>  Parse.prime_schedule |> Draw.prime_schedule |> assert_equal_string "";
  "\n" |>  Parse.prime_schedule |> Draw.prime_schedule |> assert_equal_string "\n";
  ".O\n" |>  Parse.prime_schedule |> Draw.prime_schedule |> assert_equal_string ".O\n";
  "OO\n" |>  Parse.prime_schedule |> Draw.prime_schedule |> assert_equal_string "OO\n";
  "O\n.O\nOO\n\n.\n" |>  Parse.prime_schedule |> Draw.prime_schedule |> assert_equal_string "O\n.O\nOO\n\n.\n";
  ()

let tests =
  "TestParse" >::: [
    "test_parse_prime_state_length" >:: test_parse_prime_state_length;
    "test_parse_prime_state" >:: test_parse_prime_state;
    "test_parse_prime_schedule_length" >:: test_parse_prime_schedule_length;
    "test_parse_prime_schedule" >:: test_parse_prime_schedule;
  ]
