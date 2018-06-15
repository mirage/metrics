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
   time series. {{!func}Monitoring} is performed on {{!srcs}sources},
   indexed by {{!tags}tags}. Tags allow users to select at runtime
   which metric sources is producing data points. Disabled
   data-sources have a low runtime cost (only a closure allocation)
   which make [Metrics] suitable to instrument production systems.

    Both sources tags and data-points are built using dictionaries of
   typed entries called {{!fields}fields}.

    [Metrics] is heavily inspired by
   {{:http://erratique.ch/software/logs}Logs} as it decouples metric
   reporting is from metric monitoring. This is handled by custom
   {{!reporter}reporters}.

   {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {2:fields Fields} *)

type field
(** The type for metric fields. *)

type key = string
(** The type for field keys. *)

val string: key -> string -> field
(** [string k v] is the field whose key is [k] and value is [v]. *)

val int: key -> int -> field
(** [int k i] is the field whose key is [k] and value is [i]. *)

val uint: key -> int -> field
(** [uint k i] is the field whose key is [k] and value is [i]. *)

val int32: key -> int32 -> field
(** [int32 k i] is the field whose key is [k] and value is [i]. *)

val uint32: key -> int32 -> field
(** [uint32 k i] is the field whose key is [k] and value is [i]. *)

val int64: key -> int64 -> field
(** [int64 k i] is the field whose key is [k] and value is [i]. *)

val uint64: key -> int64 -> field
(** [uint64 k i] is the field whose key is [k] and value is [i]. *)

val float: key -> float -> field
(** [uint k f] is the field whose key is [k] and value is [i]. *)

val bool: key -> bool -> field
(** [uint k b] is the field whose key is [k] and value is [i]. *)

(** {3 Custom fields} *)

(** The type of supported values in metric fields. *)
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

val field: string -> 'a ty -> 'a -> field
(** [field k ty v] is the field whose key is [k], value type is [ty] and
    value is [v]. *)

(** {3 Reading Fields} *)

val key: field -> string
(** [key f] is [f]'s key. *)

type value = V: 'a ty * 'a -> value
(** Type for values. *)

val value: field -> value
(** [value f] is [f]'s value. *)

(** {3 Pretty-printing Fields} *)

val pp_key: field Fmt.t
(** [pp_key] is the pretty-printer for field keys. *)

val pp_value: field Fmt.t
(** [pp_value] is the pretty-printer for field values, using
     sensible default. *)

(** {2:data Data points} *)

(** [Data] defines what is stored in the time series. *)
module Data: sig

  (** {2 Data}

      [Metric]'s data points are a list of typed fields with an
      optional timestamp. They are created with the {!v} and
      {{!fields}field} constructors.

      For instance, to create a data point with two values ["%CPU"] and
      ["MEM"], respectively of type [float] and [int]:

      {[
let x = Data.v [
  float "%CPU" 0.42;
  int   "MEM"  27_000;
]
     ]}
  *)

  type t
  (** The type for data points. *)

  type timestamp = string
  (** The type for timestamp. A timestamp shows the date and time, in
     RFC3339 UTC, associated with particular data. *)

  val timestamp: t -> timestamp option
  (** [timestamp t] is [t]'s timestamp (if any). If it is [None], then
     the reporter will add a new timestamp automatically. *)

  val v: ?timestamp:timestamp -> field list -> t
  (** [v ?timestamp f] is the measure [f], as a the list metric name
     and value, and the timestamp [timestamp]. If [timestamp] is not
     provided, it will be set be the reporter. Raise
     [Invalid_argument] is a key or a value contains an invalid
     character.  *)

  val keys: t -> key list
  (** [keys t] is [t]'s keys. *)

  val fields: t -> field list
  (** [fields t] is [t]'s fields. *)

end

type data = Data.t
(** The type for data points. *)

(** {2:tags Tags} *)

(** [Tags] indexes metric sources, and allow to enable/disable data
    collection at runtime. *)
module Tags: sig

  (** {2 Tags}

      [Tags] are heterogeneous {{!t}lists} of key names and type of values,
     which are associated to data sources. Filters on key names allow
     to select which data sources is {{!enabling}enabled} at runtime. Disabled data
     sources have a very low cost -- only allocating a closure.

      For instance, to define the tags "PID", "IP" and "host",
      respectively of type [int], [Ipaddr.t]:
{[
let ipaddr = Tags.v Ipaddr.pp_hum
let t = Tags.[
  int    "PID" ;
  ipaddr "IP"  ;
  string "host";
]
]}
*)

  type 'a v
  (** The type for tag values. *)

  (** The type tags: an heterogeneous list of names and types. *)
  type ('a, 'b) t =
    | []  : ('b, 'b) t
    | (::): ('a v) * ('b, 'c) t -> ('a -> 'b, 'c) t


  (** {3 Tag Values} *)

  val v: 'a Fmt.t -> string -> 'a v
  (** [ty pp] is a new typed tag. *)

  val string: string -> string v
  val float: string -> float v
  val int: string -> int v
  val uint: string -> int v
  val int32: string -> int32 v
  val uint32: string -> int32 v
  val int64: string -> int64 v
  val uint64: string -> int64 v
  val bool: string -> bool v

end

type tags = field list
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

(** {2:srcs Sources} *)

type kind = [`Any | `Timer | `Status]
(** The kind for metric sources. Arbitrary data points can be added to
   [`Any] sources. [`Timer] sources can gather data points containing
   duration information. [`Status] sources can only gather
   information about success/errors. *)

type ('a, 'b, 'c) src constraint 'c = [< kind]
(** The type for metric sources. A source defines a named unit for a
   time series. ['a] is the type of the function used to create new
   {{!data}data points}. ['b] is the type of the function used to
   create new {!tags}. ['c] is the kind of metrics (See {!kind}).

    A source needs to be {{!v}tagged} before being available to
   produce data-points.  *)

type ('a, 'b) t constraint 'b = [< kind]
(** The type for tagged metric sources. Such sources can be used to
   produce data-points. *)

(** Metric sources. *)
module Src : sig

  (** {2 Sources} *)

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
      various machines, indexed by [PID], [host] name and [IP] address:

      {[
let src =
  let ipaddr = Tags.v Ipaddr.pp_hum in
  let tags = Tags.[
      string "host";
      ipaddr "IP"  ;
      int    "PID" ;
    ] in
  let data () = Data.v [
      float "%CPU" (...);
      int   "MEM"  (...);
    ] in
  Src.v "top" ~tags ~data ~doc:"Information about processess"
]} *)

  (** {3 Status} *)

  type result = [`Ok | `Error]
  (** The type for result events. *)

  type 'a status_src = ('a, result -> Data.t, [`Status]) src
  (** The type for status sources. *)

  type status = (result -> Data.t, [`Status]) t
  (** The type for tagged status sources. *)

  val status: ?doc:string -> tags:('a, status) Tags.t -> string -> 'a status_src
  (** Same as {!v} but create a new status source. *)

  (** {3 Timers} *)

  type 'a timer_src = ('a, int64 -> result -> Data.t, [`Timer]) src
  (** The type for timer sources. The callback takes the duration of
      the event in milliseconds (an [int64]) and the status: [Error] or
      [Success]. *)

  type timer = (int64 -> result -> Data.t, [`Timer]) t
  (** The type for tagged timer source. *)

  val timer: ?doc:string -> tags:('a, timer) Tags.t -> string -> 'a timer_src
  (** Same as {!v} but create a new timer source. *)

  (** {3 Listing Sources} *)

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

  val tags : t -> string list
  (** [tags src] is the list of [src]'s tag names.  *)

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

(** {2:func Monitoring} *)

val v: ('a, 'b, 'c) src -> 'a
(** [v src t1 ... tn] resolves [src]'s tags with the values [t1],
   ... [tn]. Once all the tag values are provided, the metric source
   become {{!t}tagged}. *)

val add: ('a, [`Any]) t -> ('a -> data) -> unit
(** [add src f] adds a new data point to [src]. *)

val run: Src.timer -> (unit -> 'a) -> 'a
(** [run src f] runs [f ()] and records in a new data point the time
   it took. [run] will also record the status of the computation,
   e.g. whether an exception has been raised. *)

val run_with_result: Src.timer -> (unit -> ('c, 'd) result) -> ('c, 'd) result
(** Same as {!run} but also record if the result is [Ok] or [Error]. *)

val check: Src.status -> ('a, 'b) result -> unit
(** [check s] records if [s] is a failure or a success. *)

(** {2:reporter Reporters}

    TODO: explain and give an example
*)

(** The type for reporters. *)
type reporter = {
  now   : unit -> int64;
  report:
    'a. tags:tags -> data:data -> over:(unit -> unit) ->
    Src.t -> (unit -> 'a) -> 'a
}

val nop_reporter: reporter
(** [nop_reporter] is the initial reporter returned by {!reporter}, it
    does nothing if a metric gets reported. *)

val reporter: unit -> reporter
(** [reporter ()] is the current reporter. *)

val set_reporter: reporter -> unit
(** [set_reporter r] sets the current reporter to [r]. *)

(** TODO *)
