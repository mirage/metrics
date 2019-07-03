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

module Field = Field
module Tags = Tags
module Data = Data
module Src = Src
module Graph = Graph
module Key = Key

type data = Data.t
type tags = Field.t list
type ('a, 'b) src = ('a, 'b) Src.src

let init t data =
  let t = Src.Src t in
  match Src.data t with
  | [] ->
    let df = List.map Field.key data.Data.fields in
    Src.set_data_fields t df;
    List.iter
      (fun f ->
        match (Field.graphs f) with
        | None -> ()
        | Some gs -> List.iter (fun g -> Graph.add_field g t f) gs )
      data.Data.fields
  | _ -> ()

(* Reporters *)

type reporter =
  { now : unit -> int64
  ; at_exit : unit -> unit
  ; report :
      'a.    tags:tags -> data:Data.t -> over:(unit -> unit) -> Src.t
      -> (unit -> 'a) -> 'a }

let nop_reporter =
  { at_exit = (fun () -> ())
  ; now = (fun () -> 0L)
  ; report = (fun ~tags:_ ~data:_ ~over _ k -> over (); k ()) }

let _reporter = ref nop_reporter
let set_reporter r = _reporter := r
let reporter () = !_reporter
let () = at_exit (fun () -> !_reporter.at_exit ())
let now () = !_reporter.now ()

let report (src: ('a, 'b) Src.src) ~over ~k (tags: 'a -> tags) (f: 'b -> (data -> 'c) -> 'd): 'd =
  let tags = tags (Src.tag src) in
  f src.Src.data (fun data -> !_reporter.report ~tags ~data ~over (Src src) k)

let over () = ()
let kunit _ = ()

let add_no_check (src: ('a, 'b) Src.src) ?duration ?status (tags: 'a -> tags) (f: 'b -> data): unit =
  report src ~over ~k:kunit tags (fun data k ->
      let data = f data in
      let data =
        match duration, status with
        | None, None -> data
        | Some d, None | None, Some d -> Data.cons d data
        | Some x, Some y -> Data.cons x (Data.cons y data)
      in
      init src data; k data )

let is_active src = Src.is_active (Src src)

let add (src: ('a, 'b) Src.src) (tags: 'a -> tags) (data: 'b -> data) =
  if is_active src then add_no_check src tags data

let mk t f v = if t then Some (f v) else None

let run (src: ('a, ('b, exn) result -> data) Src.src) (tags: 'a -> tags) (g: unit -> 'b): 'b =
  if not (is_active src) then g ()
  else
    let d0 = now () in
    let r = try Ok (g ()) with e -> Error e in
    let duration = mk (Src.duration (Src src)) Field.duration (Int64.sub (now ()) d0) in
    let status x = mk (Src.duration (Src src)) Field.status x in
    match r with
    | Ok x ->
      add_no_check src ?duration ?status:(status `Ok) tags (fun f -> f r);
      x
    | Error e ->
      add_no_check src ?duration ?status:(status `Error) tags (fun f -> f r);
      raise e

type ('a, 'b) rresult = ('a, [`Exn of exn | `Error of 'b]) result

let rrun src tags g =
  if not (is_active src) then g ()
  else
    let d0 = now () in
    let r = try Ok (g ()) with e -> Error (`Exn e) in
    let duration = mk src.duration Field.duration (Int64.sub (now ()) d0) in
    let status x = mk src.status Field.status x in
    match r with
    | Ok (Ok _ as x) ->
      add_no_check src ?duration ?status:(status `Ok) tags (fun f -> f x);
      x
    | Ok (Error e as x) ->
      add_no_check src tags ?duration
        ?status:(status `Error)
        (fun f -> f (Error (`Error e)));
      x
    | Error (`Exn e as x) ->
      add_no_check src tags ?duration
        ?status:(status `Error)
        (fun f -> f (Error x));
      raise e

let enable_tag t =
  Src._tags.tags <- Keys.add t Src._tags.tags;
  Src.update ()

let disable_tag t =
  Src._tags.tags <- Keys.remove t Src._tags.tags;
  Src.update ()

let enable_all () =
  Src._tags.all <- true;
  Src.update ()

let disable_all () =
  Src._tags.all <- false;
  Src._tags.tags <- Keys.empty;
  Src.update ()

let gc_quick_stat ~tags =
  let doc = "OCaml memory management counters (quick)" in
  let graph = Graph.v ~title:doc ~ylabel:"words" () in
  let data () =
    let stat = Gc.quick_stat () in
    Data.v
      [ Field.float Key.minor_words ~graph stat.Gc.minor_words
      ; Field.float Key.promoted_words ~graph stat.Gc.promoted_words
      ; Field.float Key.major_words ~graph stat.Gc.major_words
      ; Field.uint Key.minor_collections ~graph stat.Gc.minor_collections
      ; Field.uint Key.major_collections ~graph stat.Gc.major_collections
      ; Field.uint Key.heap_words ~graph stat.Gc.heap_words
      ; Field.uint Key.heap_chunks ~graph stat.Gc.heap_chunks
      ; Field.uint Key.compactions ~graph stat.Gc.compactions
      ; Field.uint Key.top_heap_words ~graph stat.Gc.top_heap_words
      ; Field.uint Key.stack_size ~graph stat.Gc.stack_size ]
  in
  Src.v ~doc ~tags ~data "gc quick"

let gc_stat ~tags =
  let doc = "OCaml memory management counters" in
  let graph = Graph.v ~title:doc ~ylabel:"words" () in
  let data () =
    let stat = Gc.stat () in
    Data.v
      [ Field.float Key.minor_words ~graph stat.Gc.minor_words
      ; Field.float Key.promoted_words ~graph stat.Gc.promoted_words
      ; Field.float Key.major_words ~graph stat.Gc.major_words
      ; Field.uint Key.minor_collections ~graph stat.Gc.minor_collections
      ; Field.uint Key.major_collections ~graph stat.Gc.major_collections
      ; Field.uint Key.heap_words ~graph stat.Gc.heap_words
      ; Field.uint Key.heap_chunks ~graph stat.Gc.heap_chunks
      ; Field.uint Key.compactions ~graph stat.Gc.compactions
      ; Field.uint Key.live_words ~graph stat.Gc.live_words
      ; Field.uint Key.live_blocks ~graph stat.Gc.live_blocks
      ; Field.uint Key.free_words ~graph stat.Gc.free_words
      ; Field.uint Key.free_blocks ~graph stat.Gc.free_blocks
      ; Field.uint Key.largest_free ~graph stat.Gc.largest_free
      ; Field.uint Key.fragments ~graph stat.Gc.fragments
      ; Field.uint Key.top_heap_words ~graph stat.Gc.top_heap_words
      ; Field.uint Key.stack_size ~graph stat.Gc.stack_size ]
  in
  Src.v ~doc ~tags ~data "gc"
