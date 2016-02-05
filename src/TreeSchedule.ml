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

type t = Profile.t * Profile.t * Tree.t * int

let e : t =
  Profile.e, Profile.e, Tree.E, Profile.max Profile.e

let le (fp, fr, _, fres : t) (gp, gr, _, gres : t) : bool =
  fres <= gres
  && Profile.le fp gp
  && Profile.le fr gr

let combine (fp, fr, ft, _ : t) (gp, gr, gt, _ : t) : t =
  let hp = Profile.sum [
    Profile.arrange fp gp;
    Profile.p 1 0;
    Profile.p (-1) ((Profile.duration gp) / 2);
  ] in
  let hr = Profile.sum [
    Profile.arrange fr gr;
    Profile.p 2 0;
    Profile.p (-1) ((Profile.duration gr) / 2);
    Profile.p (-1) (Profile.duration fr);
  ] in
  let ht = Tree.T (ft, gt) in
  let hres = Profile.max hr in
  hp, hr, ht, hres

let combine_makes_sense (_, _, ft, _ : t) (_, _, gt, _ : t) : bool =
  match gt with
  | Tree.E -> true
  | Tree.T (_, gtr) -> 2 * Tree.reach ft > Tree.reach gtr

let duration (_, fr, _, _ : t) : int =
  Profile.duration fr

let processes (fp, _, _, _ : t) : int =
  Profile.max fp

let resources (_, _, _, fres : t) : int =
  fres

let reach (_, _, ft, _ : t) : int =
  Tree.reach ft

let summary (f : t) : Unit.summary =
  let p = processes f |> Unit.from_processes in
  let r = resources f |> Unit.from_resources in
  let l = reach f |> Unit.from_steps in
  p, r, l

let within_bounds (max_processes : int) (max_resources : int) (f : t) : bool =
  processes f <= max_processes && resources f <= max_resources

let from_tree (t : Tree.t) : t =
  Tree.map e combine t

let to_tree (_, _, ft, _ : t) : Tree.t =
  ft

let to_proc_profile (fp, _, _, _ : t) : Profile.t =
  fp

let to_res_profile (_, fr, _, _ : t) : Profile.t =
  fr
