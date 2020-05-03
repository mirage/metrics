(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
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

(* The structure is as follows:
   - Every tagged source has its own file:
    "<src_name>-<tag1>-...<tagn>.data"
   - A data-point is a newline in that file.
   - There's a toplevel gnuplot script per graph, which plots all the source
   belonging to that same graph together.
   - There's a toplevel global script which plots all the graphs. *)

(* Unix++ *)

let safe_mkdir path =
  try
    if not (Sys.is_directory path) then Fmt.failwith "mkdir: %s: is a file" path
  with Sys_error _ -> Unix.mkdir path 0o755

let split_dirs path =
  let rec aux acc path =
    match Filename.dirname path with
    | "." -> Filename.basename path :: acc
    | "/" -> "" :: Filename.basename path :: acc
    | s -> aux (Filename.basename path :: acc) s
  in
  match Filename.basename path with "/" -> [ ""; "" ] | _ -> aux [] path

let mkdir path =
  let rec aux parent = function
    | [] -> ()
    | h :: t ->
      let path = Filename.concat parent h in
      safe_mkdir path;
      aux path t
  in
  match split_dirs path with "" :: xs -> aux "/" xs | xs -> aux "." xs

(* /Unix++ *)

open Metrics

let ( / ) = Filename.concat

type file = {
  name : string;
  ppf : Format.formatter;
  data_fields : field list;
  close : unit -> unit;
}

let escape s =
  let b = Buffer.create (String.length s) in
  let e = ref false in
  String.iter
    (function
      | '\n' | '\t' | '\r' | ':' | '(' | ')' -> ()
      | ('a' .. 'z' | '0' .. '9' | 'A' .. 'Z' | '.') as x ->
        if !e then Buffer.add_char b '_';
        e := false;
        Buffer.add_char b x
      | _ -> e := true)
    s;
  Buffer.contents b

let filename (src, tags) =
  let pp_tags = Fmt.(list ~sep:(unit "-") pp_value) in
  let name = Src.name src in
  let file = Fmt.strf "%a" pp_tags (string "" name :: tags) in
  escape file ^ ".data"

module Raw = struct
  type t = Src.t * tags

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let hash t = Hashtbl.hash (filename t)

    let equal a b = filename a = filename b
  end)
end

module Lonely = struct
  type t = Src.t * string

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let hash (s, n) = Hashtbl.hash (Src.name s ^ n)

    let equal (a, b) (c, d) = Src.equal a c && String.equal b d
  end)
end

type t = { dir : string; raw : file Raw.Tbl.t; lly : Graph.t Lonely.Tbl.t }

let uuid = Uuidm.v `V4

let default_dir = Unix.getcwd () / "_metrics" / Uuidm.to_string uuid

let empty ?(dir = default_dir) () =
  { dir; raw = Raw.Tbl.create 8; lly = Lonely.Tbl.create 17 }

let register_lonely_fields t src data_fields =
  List.iter
    (fun f ->
      let k = key f in
      if graphs f = None && k <> Key.duration && k <> Key.status then
        match Lonely.Tbl.find t.lly (src, k) with
        | _ -> ()
        | exception Not_found ->
          let g = Graph.v ~title:(key f) ~ylabel:(key f) ?yunit:(unit f) () in
          Lonely.Tbl.add t.lly (src, k) g;
          Graph.add_field g src f)
    data_fields

let file t src ~data_fields ~tags =
  try Raw.Tbl.find t.raw (src, tags)
  with Not_found ->
    mkdir t.dir;
    let file = t.dir / filename (src, tags) in
    let oc = open_out file in
    let ppf = Format.formatter_of_out_channel oc in
    let close () =
      Format.fprintf ppf "%!";
      close_out oc
    in
    let file = { name = file; ppf; close; data_fields } in
    Raw.Tbl.add t.raw (src, tags) file;
    file

let write t src ~data_fields ~tags fmt =
  let f = file t src ~data_fields ~tags in
  Fmt.pf f.ppf fmt

let pp_tags =
  let e pp ppf x = Fmt.string ppf (escape (Fmt.to_to_string pp x)) in
  let pp_tag ppf t = Fmt.pf ppf "%a=%a" (e pp_key) t (e pp_value) t in
  Fmt.(list ~sep:(unit ", ") pp_tag)

let read_file file =
  let ic = open_in file in
  let r = ref [] in
  try
    while true do
      r := input_line ic :: !r
    done;
    assert false
  with End_of_file ->
    close_in ic;
    String.concat "\n" (List.rev !r)

let read_output cmd =
  let temp_file = Filename.temp_file "metrics" "gnuplot" in
  let fd = Unix.openfile temp_file [ O_WRONLY; O_TRUNC ] 0 in
  let pid = Unix.create_process "sh" [| "sh"; "-c"; cmd |] Unix.stdin fd fd in
  Unix.close fd;
  let read () = read_file temp_file in
  match snd (Unix.waitpid [] pid) with
  | exception Unix.(Unix_error (EINTR, _, _)) -> Ok (read ())
  | WEXITED 0 -> Ok (read ())
  | _ -> Error (read ())

let plots_of_field t xlabel acc (src, field) =
  Raw.Tbl.fold
    (fun (src_r, tags) file acc ->
      if Src.equal src src_r then
        let fields = Src.data src in
        let i = index ~fields field + 2 in
        let label =
          match tags with
          | [] -> key field
          | _ -> Fmt.strf "%a (%s)" pp_tags tags (key field)
        in
        match xlabel with
        | `Timestamp -> (file.name, 1, i, label) :: acc
        | `Duration -> (
          let duration =
            try Some (index_key ~fields Key.duration + 2)
            with Not_found -> None
          in
          match duration with
          | None -> acc
          | Some d -> (file.name, d, i, label) :: acc)
      else acc)
    t.raw acc

let scatter_plot oc ~plots ~title ~xlabel ~ylabel ~yunit ~output =
  let ppf = Format.formatter_of_out_channel oc in
  let xlabel =
    match xlabel with `Timestamp -> "Time (ns)" | `Duration -> "Duration (ns)"
  in
  let pp_plots ppf (file, i, j, label) =
    Fmt.pf ppf "'%s' using %d:%d t \"%s\"" file i j label
  in
  match plots with
  | [] -> ()
  | _ ->
    Fmt.pf ppf
      {|
set title '%s'
set xlabel '%s'
set ylabel "%s%s"
set datafile separator ","
set grid
set term png
set output '%s'
plot %a
|}
      title xlabel ylabel yunit output
      Fmt.(list ~sep:(unit ", ") pp_plots)
      plots

let render_graph ~dir ~out ~script_file =
  let out_dir = dir / out in
  if not (Sys.file_exists out_dir) then Unix.mkdir out_dir 0o755;
  Fmt.strf "cd %s && gnuplot %s" dir script_file |> read_output

let plot_graph ~output_format ~xlabel t g =
  let fields = Graph.fields g in
  let plots = List.fold_left (plots_of_field t xlabel) [] fields in
  let ylabel =
    match Metrics.Graph.ylabel g with
    | Some t -> t
    | None -> match fields with [] -> "" | h :: _ -> key (snd h)
  in
  let yunit =
    match Metrics.Graph.yunit g with
    | Some u -> Fmt.strf " (%s)" u
    | None ->
    match fields with
    | [] -> ""
    | h :: _ ->
    match unit (snd h) with None -> "" | Some u -> Fmt.strf " (%s)" u
  in
  let title = match Metrics.Graph.title g with Some t -> t | None -> ylabel in
  let suffix = match xlabel with `Timestamp -> "" | `Duration -> ".d" in
  let basename = Fmt.strf "%s-%d%s" (escape title) (Graph.id g) suffix in
  let output = ("out" / basename) ^ ".png" in
  let file = (t.dir / basename) ^ ".gp" in
  mkdir (Filename.dirname file);
  let oc = open_out file in
  scatter_plot oc ~plots ~title ~xlabel ~ylabel ~yunit ~output;
  flush oc;
  close_out oc;
  match output_format with
  | `Script -> Fmt.pr "%s has been created.\n%!" file
  | `Image -> (
    render_graph ~dir:t.dir ~out:"out" ~script_file:file |> function
    | Ok _ -> Fmt.pr "%s has been created.\n%!" (t.dir / output)
    | Error e -> Fmt.failwith "Cannot generate %s: %s" output e)

let set_reporter ?dir ?(output = `Image) () =
  let t = empty ?dir () in
  let report ~tags ~data ~over src k =
    let data_fields = Data.fields data in
    (* TODO: quote values *)
    let pp = Fmt.(list ~sep:(unit ", ") pp_value) in
    let timestamp =
      match Data.timestamp data with
      | Some ts -> ts
      | None -> Int64.to_string (Mtime_clock.elapsed_ns ())
    in
    let pp_timestamp ppf () =
      match data_fields with
      | [] -> Fmt.string ppf timestamp
      | _ -> Fmt.pf ppf "%s, " timestamp
    in
    write ~data_fields ~tags t src "%a%a\n" pp_timestamp () pp data_fields;
    register_lonely_fields t src data_fields;
    over ();
    k ()
  in
  let now () = Mtime_clock.now () |> Mtime.to_uint64_ns in
  let at_exit () =
    let graphs = Graph.list () in
    (* Close all the raw data files *)
    Raw.Tbl.iter (fun _ f -> f.close ()) t.raw;
    match output with
    | `Datafile ->
      Raw.Tbl.iter (fun _ f -> Fmt.pr "%s has been created.\n%!" f.name) t.raw
    | `Script ->
      List.iter (plot_graph t ~output_format:`Script ~xlabel:`Timestamp) graphs;
      List.iter (plot_graph t ~output_format:`Script ~xlabel:`Duration) graphs
    | `Image ->
      List.iter (plot_graph t ~output_format:`Image ~xlabel:`Timestamp) graphs;
      List.iter (plot_graph t ~output_format:`Image ~xlabel:`Duration) graphs
  in
  Metrics.set_reporter { Metrics.report; now; at_exit }
