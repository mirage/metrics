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

open Lwt.Infix

let src = Logs.Src.create "influx" ~doc:"influx metrics reporter"

module Log = (val Logs.src_log src : Logs.LOG)

module Influx (CLOCK : Mirage_clock.MCLOCK) (STACK : Mirage_stack.V4) = struct
  module TCP = STACK.TCPV4

  let vmname =
    Metrics.field ~doc:"name of the virtual machine" "vm" Metrics.String

  let create stack ?interval ?hostname dst ?(port = 8094) () =
    let tcp = STACK.tcpv4 stack in
    let f = ref None in
    let m = Lwt_mutex.create () in
    let connect () =
      TCP.create_connection tcp (dst, port) >|= function
      | Ok flow ->
        f := Some flow;
        Ok ()
      | Error e ->
        Log.err (fun m ->
            m "error %a while creating connection to influx" TCP.pp_error e);
        Error ()
    in
    let reconnect k msg =
      Lwt_mutex.lock m >>= fun () ->
      (match !f with None -> connect () | Some _ -> Lwt.return (Ok ()))
      >>= function
      | Ok () ->
        Lwt_mutex.unlock m;
        k msg
      | Error () ->
        Lwt_mutex.unlock m;
        Lwt.return_unit
    in
    let rec send msg =
      match !f with
      | None -> reconnect send msg
      | Some flow -> (
        TCP.write flow (Cstruct.of_string msg) >>= function
        | Ok () -> Lwt.return_unit
        | Error e ->
          f := None;
          Log.err (fun m ->
              m "error %a while writing to influx reporter, reconnecting"
                TCP.pp_write_error e);
          reconnect send msg)
    in
    connect () >|= function
    | Ok () ->
      let now () = CLOCK.elapsed_ns ()
      and tags =
        match hostname with None -> [] | Some host -> [ vmname host ]
      in
      Ok (Metrics_influx.lwt_reporter ~tags ?interval send now)
    | Error () -> Error ()
end
