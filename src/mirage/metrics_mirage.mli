(*
 * Copyright (c) 2018 Hannes Mehnert <hannes@mehnert.org>
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
(** Metrics reporters using MirageOS *)

(** Influx reporter *)
module Influx (CLOCK : Mirage_clock.MCLOCK) (STACK : Mirage_stack.V4) : sig
  val vmname : string -> Metrics.field
  (** [vmname name] creates a [tag] with the virtual machine name. *)

  val create :
    STACK.t ->
    ?interval:int ->
    ?hostname:string ->
    STACK.TCPV4.ipaddr ->
    ?port:int ->
    unit ->
    (Metrics.reporter, unit) result Lwt.t
  (** [create mclock stack ~interval ~hostname ip ~port ()] is [reporter], which
      sends measurements (prefixed by [vmname hosttname] if provided), to
      [ip:port] (defaults to 8094). If [interval] is provided, measurements from
      each source are at most reported once during [interval] milliseconds.
      Fails if the initial connection attempt fails. Reconnects TCP session on
      failure. *)
end
