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

let test_all_checkpoints _ =
  let fibers_string =
    "cccc" in
  let expected = [
    "OOOO";
    "OOO";
    "OO";
    "O";
  ] in
  let result = fibers_string |> Parse.fibers |> PrimeSchedule.from_fibers |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let test_many_processes _ =
  let fibers_string =
    "00000ccc" in
  let expected = [
    "O....OOO";
    ".O...OO";
    "O.O..O";
    ".O.O.";
    "O.O.";
    ".O.";
    "O.";
    ".";
  ] in
  let result = fibers_string |> Parse.fibers |> PrimeSchedule.from_fibers |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let test_mixed _ =
  let fibers_string =
    "ccc12c000cc" in
  let expected = [
    ".....O...OO";
    ".....OO..O";
    ".....O.O.";
    "O....OO.";
    "OO...O.";
    "OOO..O";
    "OO.O.";
    "OOO.";
    "OOO";
    "OO";
    "O";
  ] in
  let result = fibers_string |> Parse.fibers |> PrimeSchedule.from_fibers |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let test_incomplete _ =
  let fibers_string =
    "0000c" in
  let expected = [
    ".O..O";
    "O.O.";
    ".O.";
    "O.";
    ".";
  ] in
  let result = fibers_string |> Parse.fibers |> PrimeSchedule.from_fibers |> Draw.prime_schedule in
  assert_equal_string (Util.concat_lines expected) result;
  ()

let tests =
  "TestPrimeSchedule" >::: [
    "test_all_checkpoints" >:: test_all_checkpoints;
    "test_many_processes" >:: test_many_processes;
    "test_mixed" >:: test_mixed;
    "test_incomplete" >:: test_incomplete;
  ]
