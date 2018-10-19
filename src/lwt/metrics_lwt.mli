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

(** {!Lwt} monitoring.

    The monitoring functions of this module return [Lwt] threads that proceed
    only when the metric operation is over, as defined by the current
    {!Metrics.reporter}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Metric Monitoring} *)

open Metrics

val add : ('a, 'b) src -> ('a -> tags) -> ('b -> data Lwt.t) -> unit Lwt.t
(** [add src t f] adds a new data point to [src]. *)

val run :
     ('a, ('b, exn) result -> Data.t) src
  -> ('a -> tags)
  -> (unit -> 'b Lwt.t)
  -> 'b Lwt.t
(** [run src f] runs [f ()] and records in a new data point the time it took.
    [run] will also record the status of the computation, e.g. whether an
    exception has been raised. *)

val rrun :
     ('a, ('b, [`Exn of exn | `Error of 'c]) result -> Data.t) src
  -> ('a -> tags)
  -> (unit -> ('b, 'c) result Lwt.t)
  -> ('b, 'c) result Lwt.t
(** Same as {!run} but also record if the result is [Ok] or [Error]. *)
