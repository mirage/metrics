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

let add_no_check_lwt src ?duration ?status tags f =
  let (ret, unblock) = Lwt.wait () in
  let k () = ret in
  let over () = Lwt.wakeup unblock () in
  report src ~over ~k tags (fun data k ->
      f data >>= fun data ->
      let data = match duration, status with
        | None  , None   -> data
        | Some d, None
        | None  , Some d -> Data.cons d data
        | Some x, Some y -> Data.cons x (Data.cons y data)
      in
      k data
    )

let add src tags f =
  if is_active src then add_no_check_lwt src tags f
  else Lwt.return ()

let mk t f v = if t then Some (f v) else None

let run src tags data g =
  let src = Metrics.Src.src src in
  if not (is_active src) then g ()
  else (
    let d0 = now () in
    Lwt.catch
      (fun () -> g () >|= fun x -> Ok x)
      (fun e  -> Lwt.return (Error (`Exn e)))
    >>= fun r ->
    let duration = mk (Src.duration (Src src)) duration (Int64.sub (now ()) d0) in
    let status x = mk (Src.status (Src src)) status x in
    match r with
    | Ok x ->
      add_no_check_lwt src tags data ?duration ?status:(status `Ok)
      >|= fun () ->
      x
    | Error (`Exn e) ->
      add_no_check_lwt src tags data ?duration ?status:(status `Error)
      >|= fun () ->
      raise e
  )

let rrun src tags data g =
  let src = Metrics.Src.src src in
  if not (is_active src) then g ()
  else (
    let d0 = now () in
    Lwt.catch
      (fun () -> g () >|= fun x -> Ok x)
      (fun e  -> Lwt.return (Error (`Exn e)))
    >>= fun r ->
    let duration = mk (Src.duration (Src src)) duration (Int64.sub (now ()) d0) in
    let status x = mk (Src.status (Src src)) status x in
    match r with
    | Ok (Ok _ as x) ->
      add_no_check_lwt src tags data ?duration ?status:(status `Ok)
      >|= fun () ->
      x
    | Ok (Error _ as x) ->
      add_no_check_lwt src tags data ?duration ?status:(status `Error)
      >|= fun () ->
      x
    | Error (`Exn e) ->
      add_no_check_lwt src tags data ?duration ?status:(status `Error)
      >|= fun () ->
      raise e
  )
