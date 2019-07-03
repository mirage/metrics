(* TODO: hide this? *)
type predicate = {mutable all : bool; mutable tags : Keys.t}
val _tags: predicate

(* TODO: abstract this type? *)
(** The type for metric sources. A source defines a named unit for a time
    series. ['a] is the type of the function used to create new {{!data}data
    points}. ['b] is the type for {!tags}. *)
type ('a, 'b) src =
  { uid : int
  ; name : string
  ; doc : string
  ; dom : Keys.t
  ; tags : 'a Tags.t
  ; data : 'b
  ; dmap : Data.t -> Data.t
  ; mutable active : bool
  ; duration : bool
  ; status : bool
  ; mutable data_fields : string list option }

val v :
  ?doc:string
  -> ?duration:bool
  -> ?status:bool
  -> tags:'a Tags.t
  -> data:'b
  -> string
  -> ('a, 'b) src
(** [v ?doc ~tags name] is a new source, accepting arbitrary data points.
    [name] is the name of the source; it doesn't need to be unique but it is
    good practice to prefix the name with the name of your package or library
    (e.g. ["mypkg.network"]). [doc] is a documentation string describing the
    source, defaults to ["undocumented"]. [tags] is the collection if (typed)
    tags which will be used to tag and index the measure and are used
    identify the various metrics. The source will be enabled on creation iff
    one of tag in [tags] has been enabled with {!enable_tag}.

    For instance, to create a metric to collect CPU and memory usage on
    various machines, indexed by [PID], [host] name and [IP] address:

    {[ let src =
         let ipaddr = Tags.v Ipaddr.pp_hum in
         let tags = Tags.[string "host"; ipaddr "IP" ; int "PID" ;] in
         let data () = Data.v [float "%CPU" (...); int "MEM" (...); ] in
         Src.v "top" ~tags ~data ~doc:"Information about processess" ]}
*)

val tag : ('a, 'b) src -> 'a

(** {3 Listing Sources} *)

type t = Src : ('a, 'b) src -> t  (** The type for metric sources. *)

val list : unit -> t list
(** [list ()] is the current exisiting source list. *)

val update : unit -> unit (* TODO: hide this? *)

val name : t -> string
(** [name src] is [src]'s name. *)

val doc : t -> string
(** [doc src] is [src]'s documentation string. *)

val tags : t -> string list
(** [tags src] is the list of [src]'s tag names. *)

val data : t -> string list
(** [fields src] is the list of [src]'s data field names. Note that these are
    updated dynamically, so a monitoring function has to be called first. *)

val set_data_fields : t -> string list -> unit
(** [set_data_fields t fields] sets the data fields of [t] to be [fields]. *)

val equal : t -> t -> bool
(** [equal src src'] is [true] iff [src] and [src'] are the same source. *)

val compare : t -> t -> int
(** [compare src src'] is a total order on sources. *)

val duration : t -> bool
(** [duration t] is true iff [t] is a {!fn} source and [t] requires automatic
    duration recording. *)

val status : t -> bool
(** [status t] is true iff [t] is a {!fn} source and [t] requires automatic
    duration recording. *)

val pp : t Fmt.t
(** [pp ppf src] prints an unspecified representation of [src] on [ppf]. *)

val is_active : t -> bool
(** [is_active t] is true iff [t] is enabled. *)

val enable : t -> unit
(** [enable src] enables the metric source [src]. *)

val disable : t -> unit
(** [disable src] disables the metric source [src]. *)
