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

let read_all_channel (in_channel : in_channel) : string =
  let length = in_channel_length in_channel in
  let result = Bytes.create length in
  let () = really_input in_channel result 0 length in
  let () = close_in in_channel in
  result

let read_all (filename : string) : string =
  open_in filename |> read_all_channel

let write_atomic (filename : string) (data : string) : unit =
  let tmp_filename, tmp_file = Filename.open_temp_file ~temp_dir:"." (filename ^ ".") ".tmp" in
  let () = output_string tmp_file data in
  let () = close_out tmp_file in
  let () = Sys.rename tmp_filename filename in
  ()

let sort_uniq (cmp : 'a -> 'a -> int) (list : 'a list) : 'a list =
  let rec uniq = function
    | a::b::rest when cmp a b = 0 -> uniq (a::rest)
    | a::rest -> a::(uniq rest)
    | [] -> [] in
  uniq (List.sort cmp list)

let safe_concat (listlist : 'a list list) : 'a list =
  List.rev (
    List.fold_left
      (fun acc list -> List.rev_append list acc)
      []
      listlist
  )

let safe_map (f : 'a -> 'b) (list : 'a list) : 'b list =
  List.rev (List.rev_map f list)

let prefixes (l : 'a list) : 'a list list =
  let rec aux p l =
    match l with
    | [] -> [p]
    | head::tail -> p :: (aux (p @ [head]) tail)
  in
  aux [] l

let list_trim_begin (a : 'a) (l : 'a list) : 'a list =
  let rec aux l =
    match l with
    | [] -> []
    | head::rest when head = a -> aux rest
    | _ -> l
  in
  aux l

let list_trim_end (a : 'a) (l : 'a list) : 'a list =
  l |> List.rev |> list_trim_begin a |> List.rev

let rec last (l : 'a list) : 'a option =
  match l with
  | [] -> None
  | [e] -> Some e
  | _::tail -> last tail

let without_last (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | _::_ -> l |> List.rev |> List.tl |> List.rev

let map_without_last (f : 'a -> 'b) (l : 'a list) : 'b list =
  l |> without_last |> List.map f

let map_pairs (f : 'a -> 'a -> 'b) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | _::tail -> List.map2 f (without_last l) tail

let max (default : 'a) (l : 'a list) : 'a =
  match l with
  | [] -> default
  | head::tail -> List.fold_left Pervasives.max head tail

let sum (l : int list) : int =
  List.fold_left (+) 0 l

let map_range (first : int) (last : int) (f : int -> 'a) : 'a list =
  let result = ref [] in
  for i = last downto first do
    result := f i :: !result
  done;
  !result

let rev_map_range (last : int) (first : int) (f : int -> 'a) : 'a list =
  let result = ref [] in
  for i = first to last do
    result := f i :: !result
  done;
  !result

let rev_collect_if (filter : 'a -> bool) (produce : ('a -> unit) -> unit) : 'a list =
  let result = ref [] in
  let () = produce (fun a -> if filter a then result := a :: !result) in
  !result

let rev_collect (produce : ('a -> unit) -> unit) : 'a list =
  rev_collect_if (fun _ -> true) produce

let collect_if (filter : 'a -> bool) (produce : ('a -> unit) -> unit) : 'a list =
  rev_collect_if filter produce |> List.rev

let collect (produce : ('a -> unit) -> unit) : 'a list =
  rev_collect produce |> List.rev

let concat_lines (lines : string list) : string =
  lines |> List.map (fun line -> line ^ "\n") |> String.concat ""

let trim_end_regexp = Str.regexp " +$"

let trim_end (s : string) : string =
  Str.replace_first trim_end_regexp "" s
