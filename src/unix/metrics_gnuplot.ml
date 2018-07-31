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
      "<src-name>-<tag1>-...<tagn>.data"
   - A data-point is a newline in that file.
   - There's a toplevel gnuplot script per graph,
     which plots all the source beloging to that same graph
     together.
   - There's a toplevel global script which plots all
     the graphs.
*)

(* Unix++ *)

let safe_mkdir path =
  try
    if not (Sys.is_directory path) then Fmt.failwith "mkdir: %s: is a file" path
  with Sys_error _ ->
    Unix.mkdir path 0o755

let split_dirs path =
  let rec aux acc path =
    match Filename.dirname path with
    | "." -> Filename.basename path :: acc
    | "/" -> "" :: Filename.basename path :: acc
    | s   -> aux (Filename.basename path :: acc) s
  in
  match Filename.basename path with
  | "/" -> [""; ""]
  | _   -> aux [] path

let mkdir path =
  let rec aux parent = function
    | []   -> ()
    | h::t ->
      let path = Filename.concat parent h in
      safe_mkdir path;
      aux path t
  in
  match split_dirs path with
  | "" ::xs -> aux "/" xs
  | xs      -> aux "." xs

(* /Unix++ *)

open Metrics
let (/) = Filename.concat

type file = {
  name       : string;
  ppf        : Format.formatter;
  data_fields: field list;
  close      : unit -> unit;
}

let filename (src, tags) =
  (* TODO: quote names *)
  let pp_tags = Fmt.(list ~sep:(unit "-") pp_value) in
  let name = Src.name src in
  Fmt.strf "%a.data" pp_tags (string "" name :: tags)

module Plot = struct

  type t = Src.t * tags

  module Tbl = Hashtbl.Make(struct
    type nonrec t = t
    let hash t = Hashtbl.hash (filename t)
    let equal a b = filename a = filename b
  end)

end

module Graph = struct

  type t = Graph.t

  module Tbl = Hashtbl.Make(struct
      type nonrec t = t
      let hash t = Hashtbl.hash (Graph.id t)
      let equal a b = Graph.id a = Graph.id b
    end)

end

module Fields = Set.Make(struct
    type t = Plot.t * int
    let compare (a, b) (c, d) =
      match compare (filename a) (filename c) with
      | 0 -> compare b d
      | i -> i
  end)

type t = {
  dir   : string;
  plots : file Plot.Tbl.t;
  graphs: Fields.t Graph.Tbl.t;
}

let uuid = Uuidm.v `V4
let default_dir = Unix.getcwd () / "_metrics" / Uuidm.to_string uuid
let empty ?(dir=default_dir) () =
  { dir; plots = Plot.Tbl.create 8; graphs = Graph.Tbl.create 8 }

let file t src ~data_fields ~tags =
  try Plot.Tbl.find t.plots (src, tags)
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
    Plot.Tbl.add t.plots (src, tags) file;
    file

let write t src ~data_fields ~tags fmt =
  let f = file t src ~data_fields ~tags in
  List.iteri (fun i field ->
      let g = graph field in
      let gs =
        try Graph.Tbl.find t.graphs g
        with Not_found -> Fields.empty
      in
      let gs = Fields.add ((src, tags), i+2) gs in
      Graph.Tbl.replace t.graphs g gs
    ) data_fields;
  Fmt.pf f.ppf fmt

let pp_tags =
  let pp_tag ppf t = Fmt.pf ppf "%a=%a" pp_key t pp_value t in
  Fmt.(list ~sep:(unit ", ") pp_tag)

let read_file file =
  let ic = open_in file in
  let r = ref [] in
  try while true do r := input_line ic :: !r done; assert false
  with End_of_file ->
    close_in ic;
    String.concat "\n" (List.rev !r)

let read_output cmd =
  let temp_file = Filename.temp_file "metrics" "gnuplot" in
  let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
  let pid = Unix.create_process "sh" [| "sh"; "-c"; cmd |] Unix.stdin fd fd in
  Unix.close fd;
  let read () = read_file temp_file in
  match snd (Unix.waitpid [] pid) with
  | exception Unix.(Unix_error (EINTR, _, _))
              -> Ok (read ())
  | WEXITED 0 -> Ok (read ())
  | _         -> Error (read ())

let set_reporter ?dir () =
  let t = empty ?dir () in
  let report ~tags ~data ~over src k =
    let data_fields = Data.fields data in
    (* TODO: quote values *)
    let pp = Fmt.(list ~sep:(unit ", ") pp_value) in
    let timestamp = match Data.timestamp data with
      | Some ts -> ts
      | None    -> Int64.to_string (Mtime_clock.elapsed_ns ())
    in
    let pp_timestamp ppf () = match data_fields with
      | [] -> Fmt.string ppf timestamp
      | _  -> Fmt.pf ppf "%s, " timestamp
    in
    write ~data_fields ~tags t src "%a%a\n" pp_timestamp () pp data_fields;
    over ();
    k ()
  in
  let now () = Mtime_clock.now () |> Mtime.to_uint64_ns in
  let at_exit () =
    (* Close all the data files *)
    Plot.Tbl.iter (fun _ f -> f.close ()) t.plots;
    Graph.Tbl.iter (fun g fields ->
        let label = Metrics.Graph.label g in
        let title = match Metrics.Graph.title g with
          | Some t -> t
          | None   -> label
        in
        let unit = match Metrics.Graph.unit g with
          | None   -> ""
          | Some u -> Fmt.strf " (%s)" u
        in
        let output = Fmt.strf "%s.png" label in
        let file = t.dir / label ^ ".gp" in
        mkdir (Filename.dirname file);
        let oc = open_out file in
        let ppf = Format.formatter_of_out_channel oc in
        let plots = Fields.fold (fun ((_, tags as plot), i) acc ->
            let file = filename plot in
            let label = Fmt.to_to_string pp_tags tags in
            (file, i, label) :: acc
          ) fields []
        in
        let pp_plots ppf (file, i, label) =
          Fmt.pf ppf "'%s' using 1:%d t \"%s\"" file i label
        in
        Fmt.pf ppf {|
set title '%s'
set xlabel "Time (ns)"
set ylabel "%s%s"
set datafile separator ","
set grid
set term png
set output '%s'
plot %a
|} title label unit output Fmt.(list ~sep:(unit ", ") pp_plots) plots;
        flush oc;
        close_out oc;
        let cmd = Fmt.strf "cd %s && gnuplot %s" t.dir file in
        match read_output cmd with
        | Ok _    -> Fmt.pr "%s has been created.\n%!" (t.dir / output)
        | Error e -> Fmt.failwith "Cannot generate %s: %s" output e
      ) t.graphs
  in
  Metrics.set_reporter { Metrics.report; now; at_exit }
