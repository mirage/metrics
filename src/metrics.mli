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

(** Metrics monitoring. *)

type ('a, 'b) src
(** The type for metric sources. A source defines a named unit for a
   metric. The ['a] parameter is the type of the metric tags. *)

type 'a ty
(** The type for data types. *)

val string: string ty
val float: float ty
val int: int ty
val int32: int32 ty
val int64: int64 ty
val bool: bool ty


(** Data points.

    Data points are the contents of the collected metrics; they are a
   dataframe associated with a timestamp. *)
module Data: sig

  type t
  (** The type for field data. *)

  type timestamp = string
  (** the timestamp shows the date and time, in RFC3339 UTC,
     associated with particular data. *)

  type value = private string
  (** The type for individual metric values. *)

  val string: string -> value
  val int: int -> value
  val int32: int32 -> value
  val int64: int64 -> value
  val float: float -> value
  val bool: bool -> value

  val v: ?timestamp:timestamp -> (string * value) list -> t
  (** [v ?timestamp f] is the measurement [f], a the list of pairs:
     metric name and metric value and the timestamp [timestamp]. If
     [timestamp] is [None], it will be set be the reporter to the
     current time.

      TODO: examples
  *)

end

(** Typed dataframe. *)
module Frame: sig

  (** The type for a typed dictionary of tags. *)
  type ('a, 'b) t =
    | []  : ('b, 'b) t
    | (::): (string * 'a ty) * ('b, 'c) t -> ('a -> 'b, 'c) t

  val hd: ('a -> 'b, 'c) t -> string * 'a ty
  (** [hd f] is [f]'s first element. *)

  val tl: ('a -> 'b, 'c) t -> ('b, 'c) t
  (** [tl f] is [f] without its firs
t element. *)

  val empty: ('a -> unit, 'a -> unit) t
  (** [empty] is the empty frame. *)

  val cons: string -> 'a ty -> ('b, 'c) t -> ('a -> 'b, 'c) t
  (** [cons n ty f] is the new frame [t] such that [hd t = (n, ty)]
      and [tl t = f]. *)

end

(** Data sources. *)
module Src : sig

  (** {1 Sources} *)

  type t = Src: ('a, 'b) src -> t
  (** The type for metric sources. *)

  val create : ?doc:string -> string -> ('a, 'b -> unit) Frame.t ->
    'b ty -> ('b -> Data.t) -> ('a, 'b) src
  (** [create ?doc name tags] is a new metric source. [name] is the
     name of the source; it doesn't need to be unique but it is good
     practice to prefix the name with the name of your package or
     library (e.g. ["mypkg.network"]). [doc] is a documentation string
     describing the source, defaults to ["undocumented"]. [tags] is
     the collection if (typed) tags which will be used to index the
     metric and are used identify the various metric. The source will
     be enabled on creation iff one of tag in [tags] has been enabled
     with {!enable_tag}.

      For instance, to create a metric to collect CPU usage on various
     machines, indexed by hostname and core ID, use:

      {[
let src =
  create ~doc:"CPU usage" "cpu"  Frame.["hostname", string; "core", int]
    (fun () -> Data.v Frame.["percent", int] @@ Cpu.usage ())
]} *)

  val name : t -> string
  (** [name] is [src]'s name. *)

  val doc : t -> string
  (** [doc src] is [src]'s documentation string. *)

  val tags : t -> string list
  (** [tags src] is the list of tags of [src] (if any).  *)

  val equal : t -> t -> bool
  (** [equal src src'] is [true] iff [src] and [src'] are the same source. *)

  val compare : t -> t -> int
  (** [compare src src'] is a total order on sources. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf src] prints an unspecified representation of [src] on
      [ppf]. *)

  val enable: t -> unit
  (** [enable src] enables the metric source [src]. *)

  val disable: t -> unit
  (** [disable src] disables the metric source [src]. *)

  val list : unit -> t list
  (** [list ()] is the current exisiting source list. *)

end

val enable_tag: string -> unit
(** [enable_tag t] enables all the registered metric sources having
   the tag [t]. *)

val disable_tag: string -> unit
(** [disable_tag t] disables all the registered metric sources having
   the tag [t]. *)

val enable_all: unit -> unit
(** [enable_all ()] enables all registered metric sources. *)

val disable_all: unit -> unit
(** [disable_all ()] disables all registered metric sources. *)

type ('a, 'b) metric =  ('a, 'b) src -> ('a -> unit) -> unit

val v: ('a, 'b) metric

(** {Reporters} *)

type reporter = {
  report :
    'a 'b 'c.
    tags:(string * string) list ->
    fields:(string * string) list ->
    ?timestamp:string ->
    over:(unit -> unit) ->
    ('a, 'b) src -> (unit -> 'c) -> 'c
}

val nop_reporter: reporter
(** [nop_reporter] is the initial reporter returned by {!reporter}, it
    does nothing if a metric gets reported. *)

val reporter: unit -> reporter
(** [reporter ()] is the current repporter. *)

val set_reporter: reporter -> unit
(** [set_reporter r] sets the current reporter to [r]. *)
