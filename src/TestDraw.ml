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

let test_draw_prime_state _ =
  assert_equal_string "" (Draw.prime_state []);
  assert_equal_string ".O" (Draw.prime_state [false; true]);
  assert_equal_string "OO" (Draw.prime_state [true; true]);
  ()

let test_draw_prime_schedule _ =
  assert_equal_string "" (Draw.prime_schedule []);
  assert_equal_string "\n" (Draw.prime_schedule [[]]);
  assert_equal_string ".O\n" (Draw.prime_schedule [[false; true]]);
  assert_equal_string "OO\n" (Draw.prime_schedule [[true; true]]);
  assert_equal_string "O\n.O\nOO\n\n.\n" (Draw.prime_schedule [[true]; [false; true]; [true; true]; []; [false]]);
  ()

let test_draw_complete_schedule_summary prefix =
  let expected_filename = Printf.sprintf "TestDrawData/%s_summary.txt" prefix in
  expected_filename >::
    fun _ ->
    Printf.sprintf "TestDrawData/%s.txt" prefix
    |> Util.read_all
    |> Parse.prime_schedule
    |> PrimeSchedule.to_complete_schedule
    |> CompleteSchedule.summary
    |> Draw.summary
    |> Printf.sprintf "%s\n"
    |> assert_equal_file expected_filename

let test_draw_complete_schedule suffix draw_function prefix =
  let expected_filename = Printf.sprintf "TestDrawData/%s_%s.txt" prefix suffix in
  expected_filename >::
    fun _ ->
    Printf.sprintf "TestDrawData/%s.txt" prefix
    |> Util.read_all
    |> Parse.prime_schedule
    |> PrimeSchedule.to_complete_schedule
    |> draw_function
    |> assert_equal_file expected_filename

let test_draw_complete_schedule_most prefix =
  "test_draw_complete_schedule" >::: [
    test_draw_complete_schedule_summary prefix;
    test_draw_complete_schedule "ascii" Draw.ascii prefix;
    test_draw_complete_schedule "ascii_small" Draw.ascii_small prefix;
  ]

let test_draw_complete_schedule_all prefix =
  "test_draw_complete_schedule" >::: [
    test_draw_complete_schedule_summary prefix;
    test_draw_complete_schedule "ascii" Draw.ascii prefix;
    test_draw_complete_schedule "ascii_small" Draw.ascii_small prefix;
    test_draw_complete_schedule "ascii_small_summary" Draw.ascii_small_summary "p04s08";
    test_draw_complete_schedule "ascii_letters" Draw.ascii_letters "p04s08";
    test_draw_complete_schedule "ascii_letters_summary" Draw.ascii_letters_summary "p04s08";
  ]

let tests =
  "TestDraw" >::: [
    "test_draw_prime_state" >:: test_draw_prime_state;
    "test_draw_prime_schedule" >:: test_draw_prime_schedule;
    test_draw_complete_schedule_most "p03s04";
    test_draw_complete_schedule_most "p03s05";
    test_draw_complete_schedule_most "p03s06";
    test_draw_complete_schedule_most "p03s07";
    test_draw_complete_schedule_most "p03s08";
    test_draw_complete_schedule_most "p04s06";
    test_draw_complete_schedule_most "p04s07";
    test_draw_complete_schedule_all "p04s08";
    test_draw_complete_schedule_most "p04s08v2";
  ]
