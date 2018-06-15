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

module Keys = Set.Make(String)

module Tags = struct

  type 'a v = { k: string; pp: Format.formatter -> 'a -> unit }

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

  type ('a, 'b) t =
    | []  : ('b, 'b) t
    | (::): 'a v * ('b, 'c) t -> ('a -> 'b, 'c) t

  let rec domain: type a b. (a, b) t -> Keys.t = function
    | []    -> Keys.empty
    | h :: t -> Keys.add h.k (domain t)

end

type key = string

type 'a ty =
  | String: string ty
  | Bool: bool ty
  | Float: float ty
  | Int: int ty
  | Int32: int32 ty
  | Int64: int64 ty
  | Uint: int ty
  | Uint32: int32 ty
  | Uint64: int64 ty
  | Other: 'a Fmt.t -> 'a ty

type 'a v = { ty: 'a ty; v: 'a }

type field = F: string * 'a v -> field

let key (F (k, _)) = k

let field k ty v = F (k, {ty; v})
let string k v = F (k, {ty=String; v})
let bool k v = F (k, {ty=Bool; v})
let float k v = F (k, {ty=Float; v})
let int k v = F (k, {ty=Int; v})
let int32 k v = F (k, {ty=Int32; v})
let int64 k v = F (k, {ty=Int64; v})
let uint k v = F (k, {ty=Uint; v})
let uint32 k v = F (k, {ty=Uint32; v})
let uint64 k v = F (k, {ty=Uint64; v})

let pp: type a. a ty -> a Fmt.t = fun ty ppf v -> match ty with
  | String -> Fmt.string ppf v
  | Bool   -> Fmt.bool ppf v
  | Int    -> Fmt.int ppf v
  | Int32  -> Fmt.int32 ppf v
  | Int64  -> Fmt.int64 ppf v
  | Float  -> Fmt.float ppf v
  | Uint   -> Fmt.uint ppf v
  | Uint32 -> Fmt.uint32 ppf v
  | Uint64 -> Fmt.uint64 ppf v
  | Other pp -> pp ppf v

type value = V: 'a ty * 'a -> value
let pp_key ppf f = Fmt.string ppf (key f)
let pp_value ppf (F (_, {ty; v})) = pp ty ppf v
let value (F (_, {ty; v})) = V (ty, v)

module Data = struct
  type timestamp = string
  type t = {
    timestamp: string option;
    fields   : field list;
  }
  let keys t = List.map key t.fields
  let timestamp t = t.timestamp
  let fields t = t.fields
  let v ?timestamp fields = { timestamp; fields }
end

type tags = field list
type data = Data.t

module Src = struct
  (* inspiration from From logs/Src *)

  type predicate = {
    mutable all : bool;
    mutable tags: Keys.t;
  }

  let _tags = { all=false; tags=Keys.empty }

  type kind = [`Any | `Timer | `Status]

  type ('a, 'b, 'c) src = {
    kind: 'c;
    uid : int;
    name: string;
    doc : string;
    dom : Keys.t;
    tags: ('a, ('b, 'c) inst) Tags.t;
    data: 'b;
    mutable active: bool;
  }

  and ('b, 'c) inst = Inst: {
    src : ('a, 'b, 'c) src;
    tags: tags;
  } -> ('b, 'c) inst

  type t = Src: ('a, 'b, [< kind]) src -> t

  let uid =
    let id = ref (-1) in
    fun () -> incr id; !id

  let list = ref []

  let active tags =
    if _tags.all then true
    else not (Keys.is_empty (Keys.inter _tags.tags tags))

  let create kind ?(doc = "undocumented") ~tags ~data name =
    let dom = Tags.domain tags in
    let active = active dom in
    let src = { kind; dom; uid = uid (); name; doc; tags; data; active } in
    list := Src src :: !list;
    src

  let v ?doc ~tags ~data name = create `Any ?doc ~tags ~data name

  let string_of_result = function `Ok -> "ok" | `Error -> "error"
  let status_f v = field "status" (Other (Fmt.of_to_string string_of_result)) v
  let duration_f i = int64 "duration"  i

  type result = [`Ok | `Error]
  type 'a status_src = ('a, result -> Data.t, [`Status]) src
  type status = (result -> Data.t, [`Status]) inst
  type 'a timer_src = ('a, int64 -> result -> Data.t, [`Timer]) src
  type timer = (int64 -> result -> Data.t, [`Timer]) inst

  let status ?doc ~tags name: 'a status_src =
    let data s = Data.v [ status_f s ] in
    create `Status ?doc ~tags ~data name

  let timer ?doc ~tags name: 'a timer_src =
    let data i s = Data.v [ duration_f i; status_f s ] in
    create `Timer ?doc ~tags ~data name

  let update (Src s) = s.active <- active s.dom
  let enable (Src s) = s.active <- true
  let disable (Src s) = s.active <- false
  let kind (Src s) = (s.kind :> kind)
  let name (Src s) = s.name
  let doc (Src s) = s.doc
  let tags (Src s) = Keys.elements s.dom
  let equal (Src src0) (Src src1) = src0.uid = src1.uid
  let compare (Src src0) (Src src1) =
    (Pervasives.compare : int -> int -> int) src0.uid src1.uid

  let pp ppf (Src src) = Format.fprintf ppf
      "@[<1>(src@ @[<1>(name %S)@]@ @[<1>(uid %d)@] @[<1>(doc %S)@])@]"
      src.name src.uid src.doc

  let list () = !list

end

type kind = Src.kind
type ('a, 'b, 'c) src = ('a, 'b, 'c) Src.src constraint 'c = [< kind]
type ('a, 'b) t = ('a, 'b) Src.inst constraint 'b = [< kind]

let v: type a b c. (a, b, c) Src.src -> a = fun src ->
  let rec aux: type a. tags -> (a, (b, c) Src.inst) Tags.t -> a =
    fun tags -> function
      | Tags.[]       -> Src.Inst { src; tags }
      | Tags.(h :: t) -> (fun a ->
          let tags = field h.k (Other h.pp) a :: tags in
          aux tags t)
  in
  aux [] src.Src.tags

(* Reporters *)

type reporter = {
  now: unit -> int64;
  report :
    'a.  tags:tags -> data:data -> over:(unit -> unit) -> Src.t ->
    (unit -> 'a) -> 'a
}

let nop_reporter =
  { now = (fun () -> 0L);
    report = fun ~tags:_ ~data:_ ~over _ k -> over (); k () }

let _reporter = ref nop_reporter
let set_reporter r = _reporter := r
let reporter () = !_reporter
let report ~tags ~data ~over src k = !_reporter.report ~tags ~data ~over src k

let now () = !_reporter.now ()

let over () = ()
let kunit _ = ()

let is_active (Src.Inst src) = src.src.Src.active

let add_no_check (Src.Inst src) f =
  let tags = src.tags in
  let data = f src.src.data in
  report ~tags ~data ~over (Src src.src) kunit

let add src f = if is_active src then add_no_check src f

let run src g =
  if not (is_active src) then g ()
  else (
    let d0 = now () in
    let r =
      try Ok (g ())
      with e -> Error (`Exn e)
    in
    let dt = Int64.sub (now ()) d0 in
    match r with
    | Ok x ->
      add_no_check src (fun m -> m dt `Ok);
      x
    | Error (`Exn e) ->
      add_no_check src (fun m -> m dt `Error);
      raise e
  )

let run_with_result src g =
  if not (is_active src) then g ()
  else (
    let d0 = now () in
    let r =
      try Ok (g ())
      with e -> Error (`Exn e)
    in
    let dt = Int64.sub (now ()) d0 in
    match r with
    | Ok (Ok _ as x) ->
      add_no_check src (fun m -> m dt `Ok);
      x
    | Ok (Error _ as x) ->
      add_no_check src (fun m -> m dt `Error);
      x
    | Error (`Exn e) ->
      add_no_check src (fun m -> m dt `Error);
      raise e
  )

let check src t =
  if is_active src then match t with
    | Ok _    -> add_no_check src (fun m -> m `Ok)
    | Error _ -> add_no_check src (fun m -> m `Error)

let enable_tag t =
  Src._tags.tags <- Keys.add t Src._tags.tags;
  List.iter Src.update (Src.list ())

let disable_tag t =
  Src._tags.tags <- Keys.remove t Src._tags.tags;
  List.iter Src.update (Src.list ())

let enable_all () =
  Src._tags.all <- true;
  List.iter Src.update (Src.list ())

let disable_all () =
  Src._tags.all <- false;
  Src._tags.tags <- Keys.empty;
  List.iter Src.update (Src.list ())
