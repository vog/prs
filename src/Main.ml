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

let usage () =
  let usage_lines = [
    "Usage:";
    "prs s PROCESSES RESOURCES";
    "prs sp PROCESSES RESOURCES";
    "prs gen PROCESSES RESOURCES";
    "prs tree TREE";
    "prs tree_letters TREE";
    "prs tree_search MAX_PROCESSES MAX_RESOURCES MAX_REACH";
    "prs old_tree_search PROCESSES MAX_RESOURCES";
    "prs ascii < PRIME_SCHEDULE";
    "prs ascii_small < PRIME_SCHEDULE";
    "prs fibers FIBERS";
    "prs lp PROCESSES REACH";
    "prs search MAX_PROCESSES MAX_RESOURCES";
    "prs latex_tree TREE";
    "prs latex_gen PROCESSES RESOURCES";
    "prs latex_ps < PRIME_SCHEDULE";
    "prs test";
  ] in
  List.iter print_endline usage_lines

let print_schedule draw_function schedule =
  Format.printf "\n";
  Format.printf "%s" (schedule |> TreeSchedule.to_tree |> Tree.to_schedule |> draw_function);
  Format.printf "\n";
  Format.printf "S     = %s\n" (schedule |> TreeSchedule.to_tree |> Draw.tree);
  Format.printf "procp = %s\n" (schedule |> TreeSchedule.to_proc_profile |> Draw.profile);
  Format.printf "resp  = %s\n" (schedule |> TreeSchedule.to_res_profile |> Draw.profile);
  Format.printf "reach = %d\n" (schedule |> TreeSchedule.reach);
  Format.printf "\n";
  ()

let run_s processes resources =
  Generate.generate processes resources
  |> Draw.tree
  |> Format.printf "%s\n"

let run_sp processes resources =
  let schedule = Generate.generate processes resources |> TreeSchedule.from_tree in
  Format.printf "%s\n" (schedule |> TreeSchedule.to_tree |> Draw.tree);
  Format.printf "%s\n" (schedule |> TreeSchedule.summary |> Draw.summary);
  ()

let run_gen draw_function processes resources =
  Generate.generate processes resources
  |> TreeSchedule.from_tree
  |> print_schedule draw_function

let run_tree draw_function tree_string =
  tree_string
  |> Parse.tree
  |> TreeSchedule.from_tree
  |> print_schedule draw_function

let run_tree_search max_processes max_resources max_reach =
  let print s =
    Format.printf "Time: %.01f seconds\n%s\n%!" (Sys.time ()) (s |> Draw.tree_schedule)
  in
  TreeSearch.search max_processes max_resources max_reach print;
  Format.printf "Done\n%!"

let run_old_tree_search processes max_resources =
  let print s =
    Format.printf
      "%s  %s\n%!"
      (s |> TreeSchedule.summary |> Draw.summary)
      (s |> TreeSchedule.to_tree |> Draw.tree)
  in
  OldTree.search processes max_resources (List.iter print)

let run_ascii () =
  let complete_schedule = stdin |> Util.read_all_channel |> Parse.prime_schedule |> PrimeSchedule.to_complete_schedule in
  Format.printf
    "\n%s\n%s\n\n"
    (complete_schedule |> Draw.ascii)
    (complete_schedule |> CompleteSchedule.summary |> Draw.summary)

let run_ascii_small () =
  let complete_schedule = stdin |> Util.read_all_channel |> Parse.prime_schedule |> PrimeSchedule.to_complete_schedule in
  Format.printf
    "\n%s\n%s\n\n"
    (complete_schedule |> Draw.ascii_small)
    (complete_schedule |> CompleteSchedule.summary |> Draw.summary)

let run_fibers fibers =
  let complete_schedule = fibers |> Parse.fibers |> PrimeSchedule.from_fibers |> PrimeSchedule.to_complete_schedule in
  Format.printf
    "\n%s\n%s\n\n"
    (complete_schedule |> Draw.ascii)
    (complete_schedule |> CompleteSchedule.summary |> Draw.summary)

let run_lp processes reach =
  let solution = Lp.solve processes reach in
  print_string solution

let run_search max_processes max_resources =
  let filename = Format.sprintf "p%03d_s%04d.txt" (Unit.processes max_processes) (Unit.resources max_resources) in
  let () = Format.printf "Writing to file %s\n@?" filename in
  let tmp_filename, tmp_file = Filename.open_temp_file ~temp_dir:"." (filename ^ ".") ".tmp" in
  let progress s =
    let () = output_string tmp_file s in
    flush tmp_file
  in
  let () = Search.calc_l_max max_processes max_resources progress in
  let () = close_out tmp_file in
  let () = Sys.rename tmp_filename filename in
  ()

let run_latex_tree tree_string =
  let schedule = tree_string |> Parse.tree |> TreeSchedule.from_tree in
  Format.printf "\n";
  Format.printf "%s" (schedule |> TreeSchedule.to_tree |> Tree.to_schedule |> DrawLatex.schedule);
  Format.printf "\\vspace{-5mm}\n";
  Format.printf "\\begin{align*}\n";
  Format.printf "S &= %s \\\\\n" (schedule |> TreeSchedule.to_tree |> DrawLatex.tree);
  Format.printf "\\procp S &= %s \\\\\n" (schedule |> TreeSchedule.to_proc_profile |> DrawLatex.profile);
  Format.printf "\\resp S &= %s \\\\\n" (schedule |> TreeSchedule.to_res_profile |> DrawLatex.profile);
  Format.printf "\\ell(S) &= %d\n" (schedule |> TreeSchedule.reach);
  Format.printf "\\end{align*}\n";
  Format.printf "\n";
  ()

let run_latex_gen processes resources =
  let p = Unit.processes processes in
  let k = Unit.resources resources in
  let schedule = Generate.generate processes resources |> TreeSchedule.from_tree in
  Format.printf "\n";
  Format.printf "%s" (schedule |> TreeSchedule.to_tree |> Tree.to_schedule |> DrawLatex.schedule);
  Format.printf "\\vspace{-5mm}\n";
  Format.printf "\\begin{align*}\n";
  Format.printf "S^{%d}_{%d} &= %s \\\\\n" p k (schedule |> TreeSchedule.to_tree |> DrawLatex.tree);
  Format.printf "\\procp S^{%d}_{%d} &= %s \\\\\n" p k (schedule |> TreeSchedule.to_proc_profile |> DrawLatex.profile);
  Format.printf "\\resp S^{%d}_{%d} &= %s \\\\\n" p k (schedule |> TreeSchedule.to_res_profile |> DrawLatex.profile);
  Format.printf "\\ell(S^{%d}_{%d}) &= %d\n" p k (schedule |> TreeSchedule.reach);
  Format.printf "\\end{align*}\n";
  Format.printf "\n";
  ()

let run_latex_ps () =
  let schedule = stdin |> Util.read_all_channel |> Parse.prime_schedule |> PrimeSchedule.to_complete_schedule in
  Format.printf "\\documentclass[tikz,border={1mm -4mm 1mm 1mm}]{standalone}\n";
  Format.printf "\\usetikzlibrary{matrix}\n";
  Format.printf "\\input{macros_tikz}\n";
  Format.printf "\\begin{document}\n";
  Format.printf "%s" (schedule |> DrawLatex.schedule);
  Format.printf "\\end{document}\n";
  ()

let run_test () =
  let tests = "Tests" >::: [
      TestDraw.tests;
      TestDrawLatex.tests;
      TestGenerate.tests;
      TestLp.tests;
      TestMinList.tests;
      TestOldTree.tests;
      TestParse.tests;
      TestPrimeSchedule.tests;
      TestProfile.tests;
      TestSearch.tests;
      TestTree.tests;
      TestTreeSchedule.tests;
      TestTreeSearch.tests;
    ] in
  let dummy_option = "-no-cache-filename" in
  let () = Sys.argv.(1) <- dummy_option in
  run_test_tt_main tests

let () =
  match Sys.argv with
  | [| _; "s"; processes; resources |]
    -> run_s (Unit.from_processes (int_of_string processes)) (Unit.from_resources (int_of_string resources))
  | [| _; "sp"; processes; resources |]
    -> run_sp (Unit.from_processes (int_of_string processes)) (Unit.from_resources (int_of_string resources))
  | [| _; "gen"; processes; resources |]
    -> run_gen Draw.ascii_small_summary (Unit.from_processes (int_of_string processes)) (Unit.from_resources (int_of_string resources))
  | [| _; "tree"; tree_string |]
    -> run_tree Draw.ascii_small_summary tree_string
  | [| _; "tree_letters"; tree_string |]
    -> run_tree Draw.ascii_letters_summary tree_string
  | [| _; "tree_search"; max_processes; max_resources; max_reach |]
    -> run_tree_search (int_of_string max_processes) (int_of_string max_resources) (int_of_string max_reach)
  | [| _; "old_tree_search"; processes; max_resources |]
    -> run_old_tree_search (Unit.from_processes (int_of_string processes)) (Unit.from_resources (int_of_string max_resources))
  | [| _; "ascii" |]
    -> run_ascii ()
  | [| _; "ascii_small" |]
    -> run_ascii_small ()
  | [| _; "fibers"; fibers |]
    -> run_fibers fibers
  | [| _; "lp"; processes; reach |]
    -> run_lp (Unit.from_processes (int_of_string processes)) (Unit.from_steps (int_of_string reach))
  | [| _; "search"; max_processes; max_resources |]
    -> run_search (Unit.from_processes (int_of_string max_processes)) (Unit.from_resources (int_of_string max_resources))
  | [| _; "latex_tree"; tree_string |]
    -> run_latex_tree tree_string
  | [| _; "latex_gen"; processes; resources |]
    -> run_latex_gen (Unit.from_processes (int_of_string processes)) (Unit.from_resources (int_of_string resources))
  | [| _; "latex_ps" |]
    -> run_latex_ps ()
  | [| _; "test" |]
    -> run_test ()
  | _
    -> usage ()
