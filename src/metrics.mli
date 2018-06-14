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

(** Metrics Monitoring.

   [Metrics] provides a basic infrastructure to monitor metrics using
   time series. {{!func}Monitoring} is performed on {{!srcs}sources}.
   Source are indexed by {{!tags}tags}, allowing users to enable or
   disable at runtime the gathering of {{!data}data points} for specific
   sources. Metric reporting is decoupled from monitoring and is
   handled by a {{!reporter}reporter}.

    [Metrics] is heavily inspired by
   {{:http://erratique.ch/software/logs}Logs}.

   {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1:data Data points} *)

(** [Data] defines what is stored in the time series. *)
module Data: sig

  (** {1 Data}

      [Metric]'s data points are a list of untyped fields with an
      optional timestamp. They are created with the {!v} and
      {{!values}values} constructors.

      For instance, to create a data point with two values ["%CPU"] and
      ["MEM"], respectively of type [float] and [int]:

      {[
let x = Data.v [
  "%CPU", Data.float 0.42;
  "MEM" , Data.int 27_000;
]
     ]}
  *)

  type t
  (** The type for data points. *)

  type timestamp = string
  (** The type for timestamp. A timestamp shows the date and time, in
     RFC3339 UTC, associated with particular data. *)

  type key = string
  (** The type for data keys. *)

  type value = private string
  (** The type for individual metric values. *)

  val keys: t -> key list
  (** [keys t] is [t]'s keys. *)

  val fields: t -> (key * value) list
  (** [fields t] is [t]'s fields. *)

  val timestamp: t -> timestamp option
  (** [timestamp t] is [t]'s timestamp (if any). If it is [None], then
     the reporter will add a new timestamp automatically. *)

  val v: ?timestamp:timestamp -> (key * value) list -> t
  (** [v ?timestamp f] is the measure [f], as a the list metric name
     and value, and the timestamp [timestamp]. If [timestamp] is not
     provided, it will be set be the reporter. Raise
     [Invalid_argument] is a key or a value contains an invalid
     character.  *)

  (** {2:values Values} *)

  val string: string -> value
  (** [string s] is [s]. *)

  val int: int -> value
  (** [int i] is [string_of_int i]. *)

  val uint: int -> value
  (** [uint i] is [Printf.printf "%u" i]. *)

  val int32: int32 -> value
  (** [int32 i] is [Printf.printf "%l" i]. *)

  val uint32: int32 -> value
  (** [uint32 i] is [Printf.printf "%ul" i]. *)

  val int64: int64 -> value
  (** [int64 i] is [Printf.printf "%L" i]. *)

  val uint64: int64 -> value
  (** [uint64 i] is [Printf.printf "%uL" i]. *)

  val float: float -> value
  (** [uint f] is [string_of_float f]. *)

  val bool: bool -> value
  (** [uint b] is [string_of_bool b]. *)

end

type data = Data.t
(** The type for data points. *)

(** {1:tags Tags} *)

(** [Tags] indexes metric sources, and allow to enable/disable data
    collection at runtime. *)
module Tags: sig

  (** {1 Tags}

      [Tags] are heterogeneous {{!t}lists} of key names and type of values,
     which are associated to data sources. Filters on key names allow
     to select which data sources is {{!enabling}enabled} at runtime. Disabled data
     sources have a very low cost -- only allocating a closure.

      For instance, to define the tags "PID", "IP" and "host",
      respectively of type [int], [Ipaddr.t].
{[
let ipaddr = Tags.ty Ipaddr.pp_hum
let t = Tags.[
  "PID" , int;
  "IP"  , ipaddr;
  "host", string;
]}
*)

  type 'a ty
  (** The type for tag values. *)

  (** The type tags: an heterogeneous list of names and types. *)
  type ('a, 'b) t =
    | []  : ('b, 'b) t
    | (::): (string * 'a ty) * ('b, 'c) t -> ('a -> 'b, 'c) t


  (** {2 Types} *)

  val ty: 'a Fmt.t -> 'a ty
  (** [ty pp] is a new typed tag. *)

  val string: string ty
  val float: float ty
  val int: int ty
  val uint: int ty
  val int32: int32 ty
  val uint32: int32 ty
  val int64: int64 ty
  val uint64: int64 ty
  val bool: bool ty

end

type tags = (string * Data.value) list
(** The type for metric tags. Used to distinguish the various entities
   that are being measured. *)

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

(** {1:srcs Metric Sources} *)

type kind = [`Any | `Timer | `Status]
(** The kind for metric sources. Arbitrary data points can be added to
   [`Any] sources. [`Timer] sources can gather data points containing
   duration information. [`Status] sources can only gather
   information about success/errors. *)

type ('a, 'b, 'c) src constraint 'c = [< kind]
(** The type for metric sources. A source defines a named unit for a
   time series. ['a] is the type of the function used to create new
   {{!data}data points}. ['b] is the type of the function used to
   create new {!tags}. ['c] is the kind of metrics (See {!kind}). *)

type ('a, 'b) t constraint 'b = [< kind]
(** The type for metric sources, whose tags have been fully
   resolved. *)

(** Metric sources. *)
module Src : sig

  (** {1 Sources} *)

  val v:
    ?doc:string ->
    tags:('a, ('b, [`Any]) t) Tags.t ->
    data:'b ->
    string -> ('a, 'b, [`Any]) src
  (** [v ?doc ~tags name] is a new source, accepting arbitrary data points.
      [name] is the name
      of the source; it doesn't need to be unique but it is good
      practice to prefix the name with the name of your package or
      library (e.g. ["mypkg.network"]). [doc] is a documentation string
      describing the source, defaults to ["undocumented"]. [tags] is
      the collection if (typed) tags which will be used to tag and
      index the measure and are used identify the various metrics. The
      source will be enabled on creation iff one of tag in [tags] has
      been enabled with {!enable_tag}.

      For instance, to create a metric to collect CPU and memory usage on
      various machines, indexed by PID, hostname and IP, use:

      {[
let src =
  let ipaddr = Tags.ty Ipaddr.pp_hum in
  let tags = Frame.[ ("hostname", string); ("IP", ipaddr); ("PID", int) ] in
  let data () = Data.v [
     "%CPU", Data.float (...);
     "MEM" , Data.int   (...);
   ] in
  Src.push "top" ~tags ~data ~doc:"Information about processess"
]} *)

  (** {1 Status} *)

  type result = [`Ok | `Error]
  (** The type for result events. *)

  type 'a status_src = ('a, result -> Data.t, [`Status]) src
  (** The type for status sources. *)

  type status = (result -> Data.t, [`Status]) t
  (** The type for status sources whose tags have been fully resolved. *)

  val status: ?doc:string -> tags:('a, status) Tags.t -> string -> 'a status_src
  (** Same as {!v} but create a new status source. *)

  (** {1 Timers} *)

  type 'a timer_src = ('a, int64 -> result -> Data.t, [`Timer]) src
  (** The type for timer sources. The callback takes the duration of
      the event in milliseconds (an [int64]) and the status: [Error] or
      [Success]. *)

  type timer = (int64 -> result -> Data.t, [`Timer]) t
  (** The type for timer source, whose tags have been fully resolved. *)

  val timer: ?doc:string -> tags:('a, timer) Tags.t -> string -> 'a timer_src
  (** Same as {!v} but create a new timer source. *)

  (** {1 Listing Sources} *)

  type t = Src: ('a, 'b, 'c) src -> t
  (** The type for metric sources. *)

  val list : unit -> t list
  (** [list ()] is the current exisiting source list. *)

  val name : t -> string
  (** [name src] is [src]'s name. *)

  val kind : t -> kind
  (** [kind src] is [src]'s kind. *)

  val doc : t -> string
  (** [doc src] is [src]'s documentation string. *)

  val domain : t -> string list
  (** [domain src] is the list of tags of [src] (if any).  *)

  val equal : t -> t -> bool
  (** [equal src src'] is [true] iff [src] and [src'] are the same source. *)

  val compare : t -> t -> int
  (** [compare src src'] is a total order on sources. *)

  val pp : t Fmt.t
  (** [pp ppf src] prints an unspecified representation of [src] on
      [ppf]. *)

  val enable: t -> unit
  (** [enable src] enables the metric source [src]. *)

  val disable: t -> unit
  (** [disable src] disables the metric source [src]. *)

end

(** {1:func Metric functions} *)

val v: ('a, 'b, 'c) src -> 'a
(** [v src t1 ... tn] resolves [src]'s tags with the values [t1], ... [tn]. *)

val add: ('a, [`Any]) t -> ('a -> data) -> unit
(** [add src f] adds a new data point to [src]. *)

val run: Src.timer -> (unit -> 'a) -> 'a
(** [run src f] runs [f ()] and records in a new data point the time
   it took. [run] will also record the status of the computation,
   e.g. whether an exception has been raised. *)

val run_with_result: Src.timer -> (unit -> ('c, 'd) result) -> ('c, 'd) result
(** Same as {!run} but also record if the result is [Ok] or [Error]. *)

(** {1:reporter Reporters}

    TODO: explain and give an example
*)

(** The type for reporters. *)
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

val nop_reporter: reporter
(** [nop_reporter] is the initial reporter returned by {!reporter}, it
    does nothing if a metric gets reported. *)

val reporter: unit -> reporter
(** [reporter ()] is the current reporter. *)

val set_reporter: reporter -> unit
(** [set_reporter r] sets the current reporter to [r]. *)

(** TODO *)
