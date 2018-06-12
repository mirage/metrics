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

type tags = Data.t
(** The type for metric tags. Used to distinguish the various entities
   that are being measured. *)

type fields = Data.t
(** The type for metric fields. Is supposed to vary every time a new
   data point is collected. *)

type ('a, 'b, 'c) src
(** The type for metric sources. A source defines a named unit for a
   metric. ['a] is the type of the function used to create new
   {!fields}. ['b] is the type of the function used to create new
   {!tags}. ['c] is the kind of metrics (See {!Src.kind}). *)

(** Data sources. *)
module Src : sig

  (** {1 Sources} *)

  type t = Src: ('a, 'b, 'c) src -> t
  (** The type for metric sources. *)

  val name : t -> string
  (** [name] is [src]'s name. *)

  val doc : t -> string
  (** [doc src] is [src]'s documentation string. *)

  val domain : t -> string list
  (** [domain src] is the list of tags of [src] (if any).  *)

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

  (** {2 Creating sources} *)

  type kind = [`Push | `Timer]
  (** The type for source kind. [Push] sources can only be pushed
     individual data points. [Timer] sources are used to gather events
     with durations and status. *)

  type status = [`Ok | `Error]
  (** The type for event status. *)

  type 'a timer = ('a, int64 -> status -> Data.t, [`Timer]) src
  (** The type for timers. The callback takes the duration of the
     event in milliseconds (an [int64]) and the status: [Error] or
     [Success]. *)

  val push: ?doc:string -> ?tags:((Data.t -> unit) -> 'a) -> fields:'b ->
    string -> ('a, 'b, [`Push]) src
  (** [push ?doc ?tags name ty f] is a new push source. [name] is the
      name of the source; it doesn't need to be unique but it is good
      practice to prefix the name with the name of your package or
      library (e.g. ["mypkg.network"]). [doc] is a documentation string
      describing the source, defaults to ["undocumented"]. [tags] is
      the collection if (typed) tags which will be used to index the
      metric and are used identify the various metric. [ty] is the type
      of the input value of the metric callback [f]. The source will be
      enabled on creation iff one of tag in [tags] has been enabled
      with {!enable_tag}.

      For instance, to create a metric to collect CPU usage on various
      machines, indexed by hostname and core ID, use:

      {[ let src =
           let tags = Frame.[ ("hostname", string); ("core", int)] in
           let data () = Data.v Frame.[
               ("percent", Data.int @@ Cpu.usage ());
               ...
             ] in
           Src.v ~doc:"CPU usage" ~tags "cpu" unit data
      ]} *)

  val timer: ?doc:string -> ?tags:'a -> string -> 'a timer
  (** Same as {!push} but create a new timer. *)

  (** {2 Specializing tags} *)

  val with_tags: ('a, 'b, 'c) src -> 'a

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

val push: ('a, 'b, [`Push]) src -> ('a -> unit) -> unit
(** [push src f] pushes a new stream event. *)

val with_timer: 'a Src.timer -> (unit -> ('c, 'd) result) -> ('c, 'd) result
(** [with_timer src f] pushed a new stream event. The duration of [f]
   is logged as well as the kind of return (success or failure). *)

(** {Reporters} *)

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
(** [reporter ()] is the current repporter. *)

val set_reporter: reporter -> unit
(** [set_reporter r] sets the current reporter to [r]. *)
