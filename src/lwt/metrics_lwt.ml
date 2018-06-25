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

(* same a Metrics.add_no_check *)
let add_no_check src tags f =
  let over () = () in
  let k _ = () in
  report src ~over  ~k tags (fun data k -> k (f data))

let add_no_check_lwt src tags f =
  let (ret, unblock) = Lwt.wait () in
  let k () = ret in
  let over () = Lwt.wakeup unblock () in
  report ~over src ~k tags (fun data k -> f data >>= k)

let add src tags f =
  if is_active src then add_no_check_lwt src tags f
  else Lwt.return ()

let run src tags g =
  if not (is_active src) then g ()
  else (
    let d0 = now () in
    Lwt.catch
      (fun () -> g () >|= fun x -> Ok x)
      (fun e  -> Lwt.return (Error (`Exn e)))
    >|= fun r ->
    let dt = Int64.sub (now ()) d0 in
    match r with
    | Ok x ->
      add_no_check src tags (fun m -> m dt `Ok);
      x
    | Error (`Exn e) ->
      add_no_check src tags (fun m -> m dt `Error);
      raise e
  )

let run_with_result src tags g =
  if not (is_active src) then g ()
  else (
    let d0 = now () in
    Lwt.catch
      (fun () -> g () >|= fun x -> Ok x)
      (fun e  -> Lwt.return (Error (`Exn e)))
    >|= fun r ->
    let dt = Int64.sub (now ()) d0 in
    match r with
    | Ok (Ok _ as x) ->
      add_no_check src tags (fun m -> m dt `Ok);
      x
    | Ok (Error _ as x) ->
      add_no_check src tags (fun m -> m dt `Error);
      x
    | Error (`Exn e) ->
      add_no_check src tags (fun m -> m dt `Error);
      raise e
  )
