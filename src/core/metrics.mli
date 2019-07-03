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

    [Metrics] provides a basic infrastructure to monitor metrics using time
    series. {{!func}Monitoring} is performed on {{!srcs}sources}, indexed by
    {{!tags}tags}. Tags allow users to select at runtime which metric sources
    is producing data points. Disabled data-sources have a low runtime cost
    (only a closure allocation) which make [Metrics] suitable to instrument
    production systems.

    Both sources tags and data-points are built using dictionaries of typed
    entries called {{!fields}fields}.

    [Metrics] is heavily inspired by {{:http://erratique.ch/software/logs}Logs}
    as it decouples metric reporting is from metric monitoring. This is handled
    by custom {{!reporter}reporters}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

module Graph : module type of Graph
module Field : module type of Field
module Key : module type of Key

(** Metric sources. *)
module Src : module type of Src

(** {2:data Data points} *)

(** [Data] defines what is stored in the time series. *)
module Data : sig
  (** {2 Data}

      [Metric]'s data points are a list of typed fields with an optional
      timestamp. They are created with the {!v} and {{!fields}field}
      constructors.

      For instance, to create a data point with two values ["%CPU"] and
      ["MEM"], respectively of type [float] and [int]:

      {[ let x = Data.v [ float "%CPU" 0.42; int "MEM" 27_000; ] ]} *)

  (** The type for data points. *)
  type t

  (** The type for timestamp. A timestamp shows the date and time, in RFC3339
      UTC, associated with particular data. *)
  type timestamp = string

  val timestamp : t -> timestamp option
  (** [timestamp t] is [t]'s timestamp (if any). If it is [None], then the
      reporter will add a new timestamp automatically. *)

  val v : ?timestamp:timestamp -> Field.t list -> t
  (** [v ?timestamp f] is the measure [f], as a the list metric name and value,
      and the timestamp [timestamp]. If [timestamp] is not provided, it will be
      set be the reporter. Raise [Invalid_argument] is a key or a value
      contains an invalid character. *)

  val keys : t -> Field.key list
  (** [keys t] is [t]'s keys. *)

  val fields : t -> Field.t list
  (** [fields t] is [t]'s fields. *)

  val cons : Field.t -> t -> t
  (** [cons f t] is the new data having the same timestamp as [t] and the
      fields [f :: fields t]. *)
end

(** The type for data points. *)
type data = Data.t

(** {2:tags Tags} *)

(** [Tags] indexes metric sources, and allow to enable/disable data collection
    at runtime. *)
module Tags : module type of Tags

(* TODO: restrict the interface of Tags *)
(* sig
 *   (\** {2 Tags}
 *
 *       [Tags] are heterogeneous {{!t}lists} of key names and type of values,
 *       which are associated to data sources. Filters on key names allow to
 *       select which data sources is {{!enabling}enabled} at runtime. Disabled
 *       data sources have a very low cost -- only allocating a closure.
 *
 *       For instance, to define the tags "PID", "IP" and "host", respectively of
 *       type [int], [Ipaddr.t]: {[ let ipaddr = Tags.v Ipaddr.pp_hum let t =
 *       Tags.[ int "PID" ; ipaddr "IP" ; string "host"; ] ]} *\)
 *
 *   (\** The type for tag values. *\)
 *   type 'a v
 *
 *   (\** The type tags: an heterogeneous list of names and types. *\)
 *   type 'a t = [] : Field.t list t | ( :: ) : 'a v * 'b t -> ('a -> 'b) t
 *
 *   (\** {3 Tag Values} *\)
 *
 *   val v : 'a Fmt.t -> string -> 'a v
 *   (\** [ty pp] is a new typed tag. *\)
 *
 *   val string : string -> string v
 *   val float : string -> float v
 *   val int : string -> int v
 *   val uint : string -> int v
 *   val int32 : string -> int32 v
 *   val uint32 : string -> int32 v
 *   val int64 : string -> int64 v
 *   val uint64 : string -> int64 v
 *   val bool : string -> bool v
 * end *)

(** The type for metric tags. Used to distinguish the various entities that are
    being measured. *)
type tags = Field.t list

val enable_tag : Field.key -> unit
(** [enable_tag t] enables all the registered metric sources having the tag
    [t]. *)

val disable_tag : Field.key -> unit
(** [disable_tag t] disables all the registered metric sources having the tag
    [t]. *)

val enable_all : unit -> unit
(** [enable_all ()] enables all registered metric sources. *)

val disable_all : unit -> unit
(** [disable_all ()] disables all registered metric sources. *)

type ('a, 'b) src = ('a, 'b) Src.src

(** {2:func Monitoring} *)

val is_active : ('a, 'b) src -> bool
(** [is_active src] is true iff [src] monitoring is enabled. *)

val add : ('a, 'b) src -> ('a -> tags) -> ('b -> Data.t) -> unit
(** [add src t f] adds a new data point to [src] for the tags [t]. *)

val run :
  ('a, ('b, exn) result -> Data.t) src -> ('a -> tags) -> (unit -> 'b) -> 'b
(** [run src t f] runs [f ()] and add a new data points.

    Depending on [src] configuration, new data points might have duration
    information (e.g. how long [g ()] took, in nano-seconds) and status
    information (e.g. to check if an exception has been raised). *)

(** The type for extended results. *)
type ('a, 'b) rresult = ('a, [`Exn of exn | `Error of 'b]) result

val rrun :
     ('a, ('b, 'c) rresult -> Data.t) src
  -> ('a -> tags)
  -> (unit -> ('b, 'c) result)
  -> ('b, 'c) result
(** Same as {!run} but also record if the result is [Ok] or [Error]. *)

(** {2:reporter Reporters}

    TODO: explain and give an example *)

(** The type for reporters. *)
type reporter =
  { now : unit -> int64
  ; at_exit : unit -> unit
  ; report :
      'a.    tags:tags -> data:data -> over:(unit -> unit) -> Src.t
      -> (unit -> 'a) -> 'a }

val nop_reporter : reporter
(** [nop_reporter] is the initial reporter returned by {!reporter}, it does
    nothing if a metric gets reported. *)

val reporter : unit -> reporter
(** [reporter ()] is the current reporter. *)

val set_reporter : reporter -> unit
(** [set_reporter r] sets the current reporter to [r]. *)

(** {2:runtime OCaml Gc sources}

    The {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html}Gc} module
    of the OCaml system provides
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html#TYPEstat}counters}
    of the memory management via
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html#VALquick_stat}Gc.quick_stat}
    and
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html#VALstat}Gc.stat}
    function. Both are provided here. *)

val gc_stat : tags:'a Tags.t -> ('a, unit -> data) src
(** [gc_stat ~tags] is the source of OCaml's [Gc.stat ()] memory management
    counters. *)

val gc_quick_stat : tags:'a Tags.t -> ('a, unit -> data) src
(** [gc_quick_stat ~tags] is the source of OCaml's [Gc.quick_stat ()] memory
    management counters. *)

val report :
     ('a, 'b) src
  -> over:(unit -> unit)
  -> k:(unit -> 'c)
  -> ('a -> tags)
  -> ('b -> (data -> 'c) -> 'd)
  -> 'd
(**/*)

val init : ('a, 'b) src -> data -> unit
val now : unit -> int64
