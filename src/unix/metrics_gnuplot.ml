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
   - There's a toplevel gnuplot script per source,
     which plots all the graphs together.
   - There's a toplevel global script which plots all
     the source graph.
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
  ppf  : Format.formatter;
  keys : string list;
  close: unit -> unit;
}

let filename name ~tags =
  (* TODO: quote names *)
  let pp_tags = Fmt.(list ~sep:(unit "-") pp_value) in
  Fmt.strf "%a.data" pp_tags (string "" name :: tags)

module Tbl = Hashtbl.Make(struct
    type t = Src.t * tags
    let hash (src, tags) = Hashtbl.hash (filename (Src.name src) ~tags)
    let equal (a, b) (c, d) =
      Src.name a = Src.name c &&
      filename (Src.name a) ~tags:b = filename (Src.name c) ~tags:d
  end)

type t = {
  dir : string;
  srcs: file Tbl.t;
}

let uuid = Uuidm.v `V4
let default_dir = Unix.getcwd () / "_metrics" / Uuidm.to_string uuid
let empty ?(dir=default_dir) () = { dir; srcs = Tbl.create 8 }

let file t src ~keys ~tags =
  try Tbl.find t.srcs (src, tags)
  with Not_found ->
    mkdir t.dir;
    let file = t.dir / filename (Src.name src) ~tags in
    let oc = open_out file in
    let ppf = Format.formatter_of_out_channel oc in
    let close () = close_out oc in
    let file = { ppf; close; keys } in
    Tbl.add t.srcs (src, tags) file;
    file

let write t src ~keys ~tags fmt =
  let f = file t src ~keys ~tags in
  Fmt.pf f.ppf fmt

let set_reporter ?dir () =
  let t = empty ?dir () in
  let report ~tags ~data ~over src k =
    let data_fields = Data.fields data in
    let keys = List.map key data_fields in
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
    write ~keys ~tags t src "%a%a\n" pp_timestamp () pp data_fields;
    over ();
    k ()
  in
  let now () = Mtime_clock.now () |> Mtime.to_uint64_ns in
  let at_exit () =
    (* Close all the data files *)
    Tbl.iter (fun _ f -> f.close ()) t.srcs;
    (* Create a gnuplot script per source (if it's active) *)
    let srcs = List.fold_left (fun acc src ->
        if Src.is_active src then Src.name src :: acc else acc
      ) [] (Src.list ())
    in
    List.iter (fun name ->
        let file = t.dir / name ^ ".gp" in
        let oc = open_out file in
        let ppf = Format.formatter_of_out_channel oc in
        let plots = Hashtbl.create 8 in
        let find_plots () =
          Tbl.iter (fun (src, tags) f ->
              if Src.name src = name then (
                let file = filename name ~tags in
                let pp_tags =
                  let pp_tag ppf t = Fmt.pf ppf "%a=%a" pp_key t pp_value t in
                  Fmt.(list ~sep:(unit ", ") pp_tag)
                in
                let tags = Fmt.strf "%a" pp_tags tags in
                List.iteri (fun i k ->
                    let kplots =
                      try Hashtbl.find plots k
                      with Not_found -> []
                    in
                    Hashtbl.replace plots k ((file, i + 2, tags) :: kplots)
                  ) f.keys
              )
            ) t.srcs
        in
        find_plots ();
        Hashtbl.iter (fun k plots ->
            let pp_plots ppf (file, i, label) =
              Fmt.pf ppf "'%s' using 1:%d t \"%s\"" file i label
            in
            let output = Fmt.strf "%s-%s.png" name k in
            Fmt.pf ppf {|
set title '%s (%s)'
set xlabel "Time (ns)"
set ylabel "TODO"
set datafile separator ","
set grid
set term png
set output '%s'
plot %a
%!|} name k output Fmt.(list ~sep:(unit ", ") pp_plots) plots;
            let i = Fmt.kstrf Sys.command "cd %s && gnuplot %s" t.dir file in
            if i <> 0 then Fmt.failwith "Cannot generate graph for %s" name
            else (
              Fmt.pr "%s has been created.\n%!" (t.dir / output);
            )
          ) plots;
      ) srcs;
  in
  Metrics.set_reporter { Metrics.report; now; at_exit }
