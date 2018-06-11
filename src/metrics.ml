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

type 'a ty = {
  pp: Format.formatter -> 'a -> unit;
}

let string = { pp = Fmt.string }
let float = { pp = Fmt.float }
let int = { pp = Fmt.int }
let uint = { pp = Fmt.uint }
let int32 = { pp = Fmt.int32 }
let int64 = { pp = Fmt.int64 }
let bool = { pp = Fmt.bool }
let uint32 = { pp = Fmt.uint32 }
let uint64 = { pp = Fmt.uint64 }

module Data = struct

  type t = {
    timestamp: string option;
    fields   : (string * string) list;
  }

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

module Frame = struct

  type ('a, 'b) t =
    | []  : ('b, 'b) t
    | (::): (string * 'a ty) * ('b, 'c) t -> ('a -> 'b, 'c) t

  exception Empty
  let empty = []

  let cons s a b = (s, a) :: b

  let hd: type a b c. (a -> b, c) t -> string * a ty = function
    | h::_ -> h
    | [] -> raise Empty

  let tl: type a b c. (a -> b, c) t -> (b, c) t = function
    | _::t -> t
    | [] -> raise Empty

end

module Src = struct
  (* inspiration from From logs/Src *)

  module Tags = Set.Make(String)

  type tags = {
    mutable all : bool;
    mutable tags: Tags.t;
  }

  let _tags = { all=false; tags=Tags.empty }

  type ('a, 'b) src = {
    uid : int;
    name: string;
    doc : string;
    tags: Tags.t;
    args: ('a, 'b -> unit) Frame.t;
    ty  : 'b ty;
    f   : 'b -> Data.t;
    mutable active: bool;
  }

  type t = Src: ('a, 'b) src -> t

  let uid =
    let id = ref (-1) in
    fun () -> incr id; !id

  let list = ref []

  let active tags =
    if _tags.all then true
    else not (Tags.is_empty (Tags.inter _tags.tags tags))

  let create ?(doc = "undocumented") name args ty f =
    let rec tags: type a b. (a, b) Frame.t -> Tags.t = function
      | Frame.([])          -> Tags.empty
      | Frame.((n, _) :: t) ->
        if not (is_valid n) then failwith "invalid field name";
        Tags.add n (tags t)
    in
    let tags = tags args in
    let active = active tags in
    let src = { uid = uid (); name; doc; tags; active; args; ty; f } in
    list := Src src :: !list;
    src

  let update (Src s) = s.active <- active s.tags
  let enable (Src s) = s.active <- true
  let disable (Src s) = s.active <- false
  let name (Src s) = s.name
  let doc (Src s) = s.doc
  let tags (Src s) = Tags.elements s.tags
  let equal (Src src0) (Src src1) = src0.uid = src1.uid
  let compare (Src src0) (Src src1) =
    (Pervasives.compare : int -> int -> int) src0.uid src1.uid

  let pp ppf (Src src) = Format.fprintf ppf
      "@[<1>(src@ @[<1>(name %S)@]@ @[<1>(uid %d)@] @[<1>(doc %S)@])@]"
      src.name src.uid src.doc

  let list () = !list
end

(* Reporters *)

type ('a, 'b) src = ('a, 'b) Src.src

type reporter = {
  report :
    'a 'b 'c.
    tags:(string * string) list ->
    fields:(string * string) list ->
    ?timestamp:string ->
    over:(unit -> unit) ->
    ('a, 'b) src -> (unit -> 'c) -> 'c
}

let nop_reporter =
  { report = fun ~tags:_ ~fields:_ ?timestamp:_ ~over _ k -> over (); k () }

let _reporter = ref nop_reporter
let set_reporter r = _reporter := r
let reporter () = !_reporter
let report ~tags ~fields ?timestamp ~over src k =
  !_reporter.report ~tags ~fields ?timestamp ~over src k

type ('a, 'b) metric =  ('a, 'b) src -> ('a -> unit) -> unit

let over () = ()
let kunit _ = ()

let v: type a b. (a, b) metric = fun src f->
  let rec aux: type a b. _ -> (b -> Data.t) -> (a, b -> unit) Frame.t -> a =
    fun tags f -> function
      | Frame.[] ->
        (fun b ->
           let tags = List.rev tags in
           let data = f b in
           let fields = Data.fields data in
           let timestamp = Data.timestamp data in
           report ~tags ~fields ?timestamp ~over src kunit)
      | Frame.((n, t) :: r) ->
        fun a ->
          let tags = ((n, Fmt.to_to_string t.pp a) :: tags) in
          aux tags f r
  in
  if src.active then f (aux [] src.Src.f src.Src.args)
  else ()

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
