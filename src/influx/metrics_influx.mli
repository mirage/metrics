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

val encode_line_protocol : Metrics.tags -> Metrics.data -> string -> string

val lwt_reporter :
  ?tags:Metrics.tags ->
  ?interval:int ->
  (string -> unit Lwt.t) ->
  (unit -> int64) ->
  Metrics.reporter
(** [lwt_reporter ~tags ~interval send clock] is a metrics reporter that encodes
    a measurement in
    {{:https://docs.influxdata.com/influxdb/v1.5/write_protocols/line_protocol_reference/}
    influxdb line protocol} and reports it via [send]. A measurement can be
    prefixed by an optional list of [tags]. If [~interval] is specified and a
    positive amount of milliseconds, each source measurement is reported only
    once within this interval. *)
