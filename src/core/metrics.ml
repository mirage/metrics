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

module Keys = Set.Make (String)

type key = string

type 'a ty =
  | String : string ty
  | Bool : bool ty
  | Float : float ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Uint : int ty
  | Uint32 : int32 ty
  | Uint64 : int64 ty
  | Other : 'a Fmt.t -> 'a ty

type 'a v = { ty : 'a ty; v : 'a }

type graph = int

type field =
  | F : {
      key : string;
      unit : string option;
      doc : string option;
      graphs : int list option;
      v : 'a v;
    }
      -> field

module Tags = struct
  type 'a v = { k : string; pp : Format.formatter -> 'a -> unit }

  let v pp k = { k; pp }

  let string = v Fmt.string

  let float = v Fmt.float

  let int = v Fmt.int

  let uint = v Fmt.uint

  let int32 = v Fmt.int32

  let uint32 = v Fmt.uint32

  let int64 = v Fmt.int64

  let uint64 = v Fmt.uint64

  let bool = v Fmt.bool

  type 'a t = [] : field list t | ( :: ) : 'a v * 'b t -> ('a -> 'b) t

  let rec domain : type a. a t -> Keys.t = function
    | [] -> Keys.empty
    | h :: t -> Keys.add h.k (domain t)
end

let key (F { key; _ }) = key

let doc (F { doc; _ }) = doc

let unit (F { unit; _ }) = unit

let graphs (F { graphs; _ }) = graphs

module Data = struct
  type timestamp = string

  type t = { timestamp : string option; fields : field list }

  let keys t = List.map key t.fields

  let timestamp t = t.timestamp

  let fields t = t.fields

  let cons h t = { t with fields = h :: t.fields }

  let v ?timestamp fields = { timestamp; fields }
end

let index_key ~fields f =
  let rec aux n = function
    | [] -> raise Not_found
    | h :: t -> if h = f then n else aux (n + 1) t
  in
  aux 0 fields

let index ~fields (F f) = index_key ~fields f.key

type tags = field list

type data = Data.t

module Src = struct
  (* inspiration from From logs/Src *)

  type predicate = { mutable all : bool; mutable tags : Keys.t }

  let _tags = { all = false; tags = Keys.empty }

  type ('a, 'b) src = {
    uid : int;
    name : string;
    doc : string;
    dom : Keys.t;
    tags : 'a Tags.t;
    data : 'b;
    mutable active : bool;
    duration : bool;
    status : bool;
    mutable data_fields : string list option;
  }

  type t = Src : ('a, 'b) src -> t

  let uid =
    let id = ref (-1) in
    fun () ->
      incr id;
      !id

  let list = ref []

  let active tags =
    if _tags.all then true else not (Keys.is_empty (Keys.inter _tags.tags tags))

  let v ?(doc = "undocumented") ?(duration = false) ?(status = false) ~tags
      ~data name =
    let dom = Tags.domain tags in
    let active = active dom in
    let src =
      {
        duration;
        status;
        dom;
        uid = uid ();
        name;
        doc;
        tags;
        data;
        active;
        data_fields = None;
      }
    in
    list := Src src :: !list;
    src

  let is_active (Src s) = s.active

  let enable (Src s) = s.active <- true

  let disable (Src s) = s.active <- false

  let name (Src s) = s.name

  let doc (Src s) = s.doc

  let tags (Src s) = Keys.elements s.dom

  let equal (Src src0) (Src src1) = src0.uid = src1.uid

  let compare (Src src0) (Src src1) = compare src0.uid src1.uid

  let duration (Src s) = s.duration

  let status (Src s) = s.status

  let data (Src s) = match s.data_fields with None -> [] | Some l -> l

  let pp_strings ppf l =
    Fmt.pf ppf "@[<1>(%a)@]" Fmt.(list ~sep:(unit " ") string) l

  let pp ppf (Src src) =
    let tags = Keys.elements (Tags.domain src.tags) in
    let data = match src.data_fields with None -> [] | Some l -> l in
    Format.fprintf ppf
      "@[<1>(src@   @[<1>(name %S)@]@   @[<1>(uid %d)@]   @[<1>(doc %S)@])   \
       @[<1>(tags (%a))@]   @[<1>(data (%a))@] @]"
      src.name src.uid src.doc pp_strings tags pp_strings data

  let list () = !list

  let update () = List.iter (fun (Src s) -> s.active <- active s.dom) (list ())
end

module Fields = Set.Make (struct
  type t = Src.t * field

  let compare (a, F x) (b, F y) =
    match Src.compare a b with 0 -> String.compare x.key y.key | i -> i
end)

type ('a, 'b) src = ('a, 'b) Src.src

module Graph = struct
  type t = int

  type v = {
    title : string option;
    ylabel : string option;
    yunit : string option;
    id : int;
    mutable active : bool;
    mutable fields : Fields.t;
  }

  let tbl = Hashtbl.create 27

  let v ?title ?ylabel ?yunit () =
    let id = Oo.id (object end) in
    let t =
      { id; yunit; title; ylabel; active = false; fields = Fields.empty }
    in
    Hashtbl.add tbl id t;
    id

  let get id = Hashtbl.find tbl id

  let title t = (get t).title

  let ylabel t = (get t).ylabel

  let yunit t = (get t).yunit

  let id t = (get t).id

  let enable t = (get t).active <- true

  let disable t = (get t).active <- false

  let is_active t = (get t).active

  let list () = Hashtbl.fold (fun x _ acc -> x :: acc) tbl []

  let fields g = Fields.fold (fun f acc -> f :: acc) (get g).fields []

  let add_field g src f =
    let g = get g in
    g.fields <- Fields.add (src, f) g.fields

  let remove_field g src f =
    let g = get g in
    g.fields <-
      Fields.filter
        (fun (x, y) -> not (Src.equal x src && String.equal f (key y)))
        g.fields
end

let init t data =
  match t.Src.data_fields with
  | Some _ -> ()
  | None ->
    let df = List.map key data.Data.fields in
    t.data_fields <- Some df;
    List.iter
      (fun (F f) ->
        match f.graphs with
        | None -> ()
        | Some gs -> List.iter (fun g -> Graph.add_field g (Src t) (F f)) gs)
      data.Data.fields

type 'a field_f =
  ?doc:string ->
  ?unit:string ->
  ?graph:graph ->
  ?graphs:graph list ->
  key ->
  'a ->
  field

let field ?doc ?unit ?graph ?graphs key ty v =
  let graphs =
    match (graph, graphs) with
    | None, None -> None
    | Some g, None -> Some [ g ]
    | None, Some gs -> Some gs
    | Some g, Some gs -> Some (g :: gs)
  in
  F { key; doc; unit; v = { ty; v }; graphs }

let ff ty ?doc ?unit ?graph ?graphs k v = field ?doc ?unit ?graph ?graphs k ty v

let string = ff String

let bool = ff Bool

let float = ff Float

let int = ff Int

let int32 = ff Int32

let int64 = ff Int64

let uint = ff Uint

let uint32 = ff Uint32

let uint64 = ff Uint64

type status = [ `Ok | `Error ]

let string_of_status = function `Ok -> "ok" | `Error -> "error"

module Key = struct
  let duration = "duration"

  let status = "status"

  let minor_words = "minor words"

  let promoted_words = "promoted words"

  let major_words = "major words"

  let minor_collections = "minor collections"

  let major_collections = "major collections"

  let heap_words = "heap words"

  let heap_chunks = "heap chunks"

  let compactions = "compactions"

  let live_words = "live words"

  let live_blocks = "live blocks"

  let free_words = "free words"

  let free_blocks = "free blocks"

  let largest_free = "largest free"

  let fragments = "fragments"

  let top_heap_words = "top heap words"

  let stack_size = "stack size"
end

let status v = field Key.status (Other (Fmt.of_to_string string_of_status)) v

let duration i = int64 Key.duration i

let pp : type a. a ty -> a Fmt.t =
 fun ty ppf v ->
  match ty with
  | String -> Fmt.string ppf v
  | Bool -> Fmt.bool ppf v
  | Int -> Fmt.int ppf v
  | Int32 -> Fmt.int32 ppf v
  | Int64 -> Fmt.int64 ppf v
  | Float -> Fmt.float ppf v
  | Uint -> Fmt.uint ppf v
  | Uint32 -> Fmt.uint32 ppf v
  | Uint64 -> Fmt.uint64 ppf v
  | Other pp -> pp ppf v

type value = V : 'a ty * 'a -> value

let pp_key ppf f = Fmt.string ppf (key f)

let pp_value ppf (F { v = { ty; v }; _ }) = pp ty ppf v

let value (F { v = { ty; v }; _ }) = V (ty, v)

let tag : type a b. (a, b) Src.src -> a =
 fun src ->
  let rec aux : type a. tags -> a Tags.t -> a =
   fun tags -> function
    | Tags.[] -> List.rev tags
    | Tags.(h :: t) ->
      fun a ->
        let tags = field h.k (Other h.pp) a :: tags in
        aux tags t
  in
  aux [] src.Src.tags

(* Reporters *)

type reporter = {
  now : unit -> int64;
  at_exit : unit -> unit;
  report :
    'a. tags:tags -> data:data -> over:(unit -> unit) -> Src.t ->
    (unit -> 'a) -> 'a;
}

let nop_reporter =
  {
    at_exit = (fun () -> ());
    now = (fun () -> 0L);
    report =
      (fun ~tags:_ ~data:_ ~over _ k ->
        over ();
        k ());
  }

let _reporter = ref nop_reporter

let set_reporter r = _reporter := r

let reporter () = !_reporter

let () = at_exit (fun () -> !_reporter.at_exit ())

let now () = !_reporter.now ()

module SM = Map.Make (Src)

let cache_reporter () =
  let m = ref SM.empty in
  let report ~tags ~data ~over src k =
    m := SM.add src (tags, data) !m;
    over ();
    k ()
  in
  ((fun () -> !m), { report; now; at_exit = (fun () -> ()) })

let report src ~over ~k tags f =
  let tags = tags (tag src) in
  f src.Src.data (fun data -> !_reporter.report ~tags ~data ~over (Src src) k)

let over () = ()

let kunit _ = ()

let add_no_check src ?duration ?status tags f =
  report src ~over ~k:kunit tags (fun data k ->
      let data = f data in
      let data =
        match (duration, status) with
        | None, None -> data
        | Some d, None | None, Some d -> Data.cons d data
        | Some x, Some y -> Data.cons x (Data.cons y data)
      in
      init src data;
      k data)

let is_active src = src.Src.active

let add src tags data = if is_active src then add_no_check src tags data

let mk t f v = if t then Some (f v) else None

let run src tags g =
  if not (is_active src) then g ()
  else
    let d0 = now () in
    let r = try Ok (g ()) with e -> Error e in
    let duration = mk src.duration duration (Int64.sub (now ()) d0) in
    let status x = mk src.status status x in
    match r with
    | Ok x ->
      add_no_check src tags ?duration ?status:(status `Ok) (fun f -> f r);
      x
    | Error e ->
      add_no_check src tags ?duration ?status:(status `Error) (fun f -> f r);
      raise e

type ('a, 'b) rresult = ('a, [ `Exn of exn | `Error of 'b ]) result

let rrun src tags g =
  if not (is_active src) then g ()
  else
    let d0 = now () in
    let r = try Ok (g ()) with e -> Error (`Exn e) in
    let duration = mk src.duration duration (Int64.sub (now ()) d0) in
    let status x = mk src.status status x in
    match r with
    | Ok (Ok _ as x) ->
      add_no_check src tags ?duration ?status:(status `Ok) (fun f -> f x);
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
      [
        float Key.minor_words ~graph stat.Gc.minor_words;
        float Key.promoted_words ~graph stat.Gc.promoted_words;
        float Key.major_words ~graph stat.Gc.major_words;
        uint Key.minor_collections ~graph stat.Gc.minor_collections;
        uint Key.major_collections ~graph stat.Gc.major_collections;
        uint Key.heap_words ~graph stat.Gc.heap_words;
        uint Key.heap_chunks ~graph stat.Gc.heap_chunks;
        uint Key.compactions ~graph stat.Gc.compactions;
        uint Key.top_heap_words ~graph stat.Gc.top_heap_words;
        uint Key.stack_size ~graph stat.Gc.stack_size;
      ]
  in
  Src.v ~doc ~tags ~data "gc quick"

let gc_stat ~tags =
  let doc = "OCaml memory management counters" in
  let graph = Graph.v ~title:doc ~ylabel:"words" () in
  let data () =
    let stat = Gc.stat () in
    Data.v
      [
        float Key.minor_words ~graph stat.Gc.minor_words;
        float Key.promoted_words ~graph stat.Gc.promoted_words;
        float Key.major_words ~graph stat.Gc.major_words;
        uint Key.minor_collections ~graph stat.Gc.minor_collections;
        uint Key.major_collections ~graph stat.Gc.major_collections;
        uint Key.heap_words ~graph stat.Gc.heap_words;
        uint Key.heap_chunks ~graph stat.Gc.heap_chunks;
        uint Key.compactions ~graph stat.Gc.compactions;
        uint Key.live_words ~graph stat.Gc.live_words;
        uint Key.live_blocks ~graph stat.Gc.live_blocks;
        uint Key.free_words ~graph stat.Gc.free_words;
        uint Key.free_blocks ~graph stat.Gc.free_blocks;
        uint Key.largest_free ~graph stat.Gc.largest_free;
        uint Key.fragments ~graph stat.Gc.fragments;
        uint Key.top_heap_words ~graph stat.Gc.top_heap_words;
        uint Key.stack_size ~graph stat.Gc.stack_size;
      ]
  in
  Src.v ~doc ~tags ~data "gc"
