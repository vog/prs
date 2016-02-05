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

let create processes steps =
  let optimized = true in
  let prime_steps = Unit.prime_steps steps in
  let tpl = Tpl.create () in
  let o format = Tpl.line tpl format in
  o "Minimize";
  o "";
  o " objective: r'";
  o "";
  o "Subject To";
  o "";
  o " processes: p' <= %d" (Unit.prime_processes processes);
  o "";
  o " \\ processes(i): Step (i) uses at most the given number of processes";
  for i = 1 to prime_steps - 1 do
    o "";
    o " processes(%d):" i;
    for k = 1 to i + 1 do
      o "  + t(%d,%d)" i k;
    done;
    o "  - p'";
    o "  <= 0";
  done;
  o "";
  o " \\ resources(i): Step (i) uses at most the given number of resources";
  for i = 1 to prime_steps do
    o "";
    o " resources(%d):" i;
    for k = 1 to i do
      o "  + m(%d,%d)" i k;
    done;
    o "  - r'";
    o "  <= 0";
  done;
  o "";
  o " \\ consistency_m(i,k): Resource (i,k) can only be available if resource (i+1,k) is available, or a process at (i,k) happend";
  o "";
  for i = 1 to prime_steps - 1 do
    for k = 1 to i do
      o " consistency_m(%d,%d): m(%d,%d) - t(%d,%d) - m(%d,%d) <= 0"
        i k i k i k (i+1) k;
    done;
    for k = i + 1 to i + 1 do
      o " consistency_m(%d,%d):        - t(%d,%d) - m(%d,%d) <= -1"
        i k i k (i+1) k;
    done;
  done;
  o "";
  o " \\ consistency_t(i,k): Process (i,k) can only happen if resource (i+1,k-1) is available";
  o "";
  for i = 1 to prime_steps - 1 do
    for k = 2 to i + 1 do
      o " consistency_t(%d,%d): t(%d,%d) - m(%d,%d) <= 0"
        i k i k (i+1) (k-1);
    done;
  done;
  if optimized then begin
    o "";
    o " \\ optimization_a(i,k): Process (i,k) must not happen if resource (i+1,k) is available";
    o "";
    for i = 1 to prime_steps - 1 do
      for k = 1 to i + 1 do
        o " optimization_a(%d,%d): t(%d,%d) + m(%d,%d) <= 1"
          i k i k (i+1) k;
      done;
    done;
    o "";
    o " \\ optimization_b(i,k): If process (i,k) happens, it must be used by resource (i,k)";
    o "";
    for i = 1 to prime_steps - 1 do
      for k = 1 to i do
        o " optimization_b(%d,%d): t(%d,%d) - m(%d,%d) <= 0"
          i k i k i k;
      done;
    done;
    o "";
    o " \\ optimization_c(i,k): If process (i,k) happens, process (i,k+1) must not happen";
    o "";
    for i = 1 to prime_steps - 1 do
      for k = 1 to i do
        o " optimization_c(%d,%d): t(%d,%d) + t(%d,%d) <= 1"
          i k i k i (k+1);
      done;
    done;
    o "";
    o " \\ optimization_d(i,k): If resource (i,k) is available, process (i-1,k+1) must happen or resource (i-1,k) must be available";
    o "";
    for i = 2 to prime_steps do
      for k = 1 to i - 1 do
        o " optimization_d(%d,%d): m(%d,%d) - t(%d,%d) - m(%d,%d) <= 0"
          i k i k (i-1) (k+1) (i-1) k;
      done;
    done;
  end;
  o "";
  o "Bounds";
  o "";
  o " 0 <= p'";
  o " 0 <= r'";
  o "";
  o "Integer";
  o "";
  o " p'";
  o " r'";
  o "";
  o "Binary";
  o "";
  for i = 1 to prime_steps do
    for k = 1 to i do
      o " m(%d,%d)" i k;
    done;
  done;
  o "";
  for i = 1 to prime_steps - 1 do
    for k = 1 to i + 1 do
      o " t(%d,%d)" i k;
    done;
  done;
  o "";
  o "End";
  Tpl.result tpl

let solve processes steps =
  let threads = 1 in
  let lp = create processes steps in
  let () = Util.write_atomic "tmp_input.lp" lp in
  let prog = "cbc" in
  let args = [|
      "cbc";
      "tmp_input.lp";
      "-threads";
      string_of_int threads;
      "solve";
      "solution";
      "tmp_solution.txt";
      "gsolution";
      "tmp_gsolution.txt";
  |] in
  let pid = Unix.create_process prog args Unix.stdin Unix.stderr Unix.stderr in
  let _ = Unix.waitpid [] pid in
  let solution = Util.read_all "tmp_solution.txt" in
  solution
