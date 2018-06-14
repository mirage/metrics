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

  type 'a ty = { pp: Format.formatter -> 'a -> unit }
  let ty pp = { pp }
  let string = { pp = Fmt.string }
  let float = { pp = Fmt.float }
  let int = { pp = Fmt.int }
  let uint = { pp = Fmt.uint }
  let int32 = { pp = Fmt.int32 }
  let uint32 = { pp = Fmt.uint32 }
  let int64 = { pp = Fmt.int64 }
  let uint64 = { pp = Fmt.uint64 }
  let bool = { pp = Fmt.bool }

  type ('a, 'b) t =
    | []  : ('b, 'b) t
    | (::): (string * 'a ty) * ('b, 'c) t -> ('a -> 'b, 'c) t

  let rec domain: type a b. (a, b) t -> Keys.t = function
    | [] -> Keys.empty
    | (h, _) :: t -> Keys.add h (domain t)

end

module Data = struct

  type t = {
    timestamp: string option;
    fields   : (string * string) list;
  }

  let keys t = List.map fst t.fields
  type timestamp = string
  type key = string
  type value = string
  let string x = x
  let int = string_of_int
  let uint = Printf.sprintf "%u"
  let int32 = Int32.to_string
  let uint32 = Printf.sprintf "%lu"
  let int64 = Int64.to_string
  let uint64 = Printf.sprintf "%Lu"
  let float = string_of_float
  let bool = string_of_bool
  let timestamp t = t.timestamp
  let fields t = t.fields
  let v ?timestamp fields = { timestamp; fields }
end

type tags = (Data.key * Data.value) list
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

  type result = [`Ok | `Error]
  type 'a status_src = ('a, result -> Data.t, [`Status]) src
  type status = (result -> Data.t, [`Status]) inst
  type 'a timer_src = ('a, int64 -> result -> Data.t, [`Timer]) src
  type timer = (int64 -> result -> Data.t, [`Timer]) inst

  let status ?doc ~tags name: 'a status_src =
    let data s =
      Data.v [ ("status", string_of_result s) ]
    in
    create `Status ?doc ~tags ~data name

  let timer ?doc ~tags name: 'a timer_src =
    let data i s =
      Data.v [ ("duration", Data.int64 i); ("status", string_of_result s) ]
    in
    create `Timer ?doc ~tags ~data name

  let update (Src s) = s.active <- active s.dom
  let enable (Src s) = s.active <- true
  let disable (Src s) = s.active <- false
  let kind (Src s) = (s.kind :> kind)
  let name (Src s) = s.name
  let doc (Src s) = s.doc
  let domain (Src s) = Keys.elements s.dom
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
      | Tags.[] -> Src.Inst { src; tags }
      | Tags.((k, ty):: t) -> (fun a ->
          let tags = (k, Fmt.to_to_string ty.pp a) :: tags in
          aux tags t)
  in
  aux [] src.Src.tags

(* Reporters *)

type reporter = {
  now: unit -> int64;
  report :
    'a 'b 'c 'd.
    tags:(string * string) list ->
    fields:(string * string) list ->
    ?timestamp:string ->
    over:(unit -> unit) ->
    ('a, 'b, 'd) src -> (unit -> 'c) -> 'c
}

let nop_reporter =
  { now = (fun () -> 0L);
    report = fun ~tags:_ ~fields:_ ?timestamp:_ ~over _ k -> over (); k () }

let _reporter = ref nop_reporter
let set_reporter r = _reporter := r
let reporter () = !_reporter
let report ~tags ~fields ?timestamp ~over src k =
  !_reporter.report ~tags ~fields ?timestamp ~over src k

let now () = !_reporter.now ()

let over () = ()
let kunit _ = ()

let is_active (Src.Inst src) = src.src.Src.active

let add_no_check (Src.Inst src) f =
  let tags = src.tags in
  let d = f src.src.data in
  let { Data.fields; timestamp } = d in
  report ~tags ~fields ?timestamp ~over src.src kunit

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
