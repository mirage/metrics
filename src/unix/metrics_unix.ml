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

open Metrics
open Lwt.Infix

let gc_quick_stat = gc_quick_stat ~tags:Tags.[]

let gc_stat = gc_stat ~tags:Tags.[]

let monitor_gc ?(quick = true) delay =
  let id x = x in
  let f () =
    if quick then add gc_quick_stat id (fun d -> d ())
    else add gc_stat id (fun d -> d ())
  in
  let rec loop () =
    f ();
    Lwt_unix.sleep delay >>= fun () -> loop ()
  in
  Lwt.async loop

let disable_gc_stat () =
  Src.(disable (Src gc_quick_stat));
  Src.(disable (Src gc_stat))

let enable_gc_stat () =
  Src.(enable (Src gc_quick_stat));
  Src.(enable (Src gc_stat))
