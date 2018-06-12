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

module Chars = Set.Make(struct
    type t = char
    let compare = compare
  end)

let valid_chars =
  (* "^[a-zA-Z0-9_]+$" *)
  let s = "abcdefghijklmnopqerstuwzyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_." in
  let cs = ref Chars.empty in
  String.iter (fun c -> cs := Chars.add c !cs) s;
  !cs

exception Break

let is_valid name =
  try
    String.iter (fun c ->
        if not (Chars.mem c valid_chars) then raise Break
      ) name;
    true
  with Break -> false

module Tags = Set.Make(String)

module Data = struct

  type t = {
    timestamp: string option;
    fields   : (string * string) list;
  }

  let domain t = Tags.of_list (List.map fst t.fields)
  type timestamp = string
  type value = string
  let string x = x
  let int = string_of_int
  let int32 = Int32.to_string
  let int64 = Int64.to_string
  let float = string_of_float
  let bool = string_of_bool
  let v ?timestamp fields = { timestamp; fields }
  let timestamp t = t.timestamp
  let fields t = t.fields
end

type tags = (string * string) list
type fields = Data.t

module Src = struct
  (* inspiration from From logs/Src *)

  type tags = {
    mutable all : bool;
    mutable tags: Tags.t;
  }

  let _tags = { all=false; tags=Tags.empty }

  type status = [`Ok | `Error]
  type kind = [`Push | `Timer]

  type ('a, 'b, 'c) src = {
    kind  : 'c;
    uid   : int;
    name  : string;
    doc   : string;
    dom   : Tags.t;
    tags  : 'a;
    fields: 'b;
    mutable active: bool;
  }

  type t = Src: ('a, 'b, 'c) src -> t

  let uid =
    let id = ref (-1) in
    fun () -> incr id; !id

  let list = ref []

  let active tags =
    if _tags.all then true
    else not (Tags.is_empty (Tags.inter _tags.tags tags))

  let v kind ?(doc = "undocumented") ~tags ~fields name =
    let dom = Data.domain tags in
    let active = active dom in
    let src = { kind; dom; uid = uid (); name; doc; tags; fields; active } in
    list := Src src :: !list;
    src

  let push ?doc ~tags ~fields name = v `Push ?doc ~tags ~fields name

  let string_of_status = function `Ok -> "ok" | `Error -> "error"

  type 'a timer = ('a, int64 -> status -> Data.t, [`Timer]) src

  let timer ?doc ~tags name: 'a timer =
    let fields i s =
      Data.v [ ("duration", Data.int64 i); ("status", string_of_status s) ]
    in
    v `Timer ?doc ~tags ~fields name

  let update (Src s) = s.active <- active s.dom
  let enable (Src s) = s.active <- true
  let disable (Src s) = s.active <- false
  let name (Src s) = s.name
  let doc (Src s) = s.doc
  let domain (Src s) = Tags.elements s.dom
  let equal (Src src0) (Src src1) = src0.uid = src1.uid
  let compare (Src src0) (Src src1) =
    (Pervasives.compare : int -> int -> int) src0.uid src1.uid

  let pp ppf (Src src) = Format.fprintf ppf
      "@[<1>(src@ @[<1>(name %S)@]@ @[<1>(uid %d)@] @[<1>(doc %S)@])@]"
      src.name src.uid src.doc

  let list () = !list

  let with_tags src f fields =
    let tags, _ = f src.tags fields in { src with tags }

end

(* Reporters *)

type ('a, 'b, 'c) src = ('a, 'b, 'c) Src.src

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

let push src f =
  if src.Src.active then
    let tags, d = f src.tags src.fields in
    let { Data.fields; timestamp } = d in
    report ~tags ~fields ?timestamp ~over src kunit

let with_timer src g =
  if not src.Src.active then g ()
  else (
    let d0 = now () in
    let r =
      try Ok (g ())
      with e -> Error (`Exn e)
    in
    let dt = Int64.sub (now ()) d0 in
    match r with
    | Ok (Ok _ as x) ->
      push src (fun t m -> t, m (dt, `Ok));
      x
    | Ok (Error _ as x) ->
      push src (fun t m -> t, m (dt, `Error));
      x
    | Error (`Exn e) ->
      push src (fun t m -> t, m (dt, `Error));
      raise e
  )

let enable_tag t =
  Src._tags.tags <- Src.Tags.add t Src._tags.tags;
  List.iter Src.update (Src.list ())

let disable_tag t =
  Src._tags.tags <- Src.Tags.remove t Src._tags.tags;
  List.iter Src.update (Src.list ())

let enable_all () =
  Src._tags.all <- true;
  List.iter Src.update (Src.list ())

let disable_all () =
  Src._tags.all <- false;
  Src._tags.tags <- Src.Tags.empty;
  List.iter Src.update (Src.list ())
