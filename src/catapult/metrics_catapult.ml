(*
 * Copyright (c) 2019 Craig Ferguson <me@craigfe.io>
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

type data_series = (string * string) list [@@deriving to_yojson]

let report_instant ~ts name tags data =
  Catapult.Event.to_yojson
  @@ Catapult.Event.instant ~name ~ts
       ~args:[ ("tags", tags); ("data", data) ]
       ()

let report_duration ~ts name tags data duration =
  Catapult.Event.to_yojson
  @@ Catapult.Event.complete ~name ~ts
       ~dur:(duration |> Int64.of_string |> Mtime.Span.of_uint64_ns)
       ~args:[ ("tags", tags); ("data", data) ]
       ()

let set_reporter ?(pretty_print = true) ~output =
  let now () = Mtime_clock.now () |> Mtime.to_uint64_ns in
  let file = Unix.openfile output [ Unix.O_WRONLY ] 0o600 in
  let out_channel = Unix.out_channel_of_descr file in
  let () =
    output_string out_channel "{\"displayTimeUnit\":\"ns\",\"traceEvents\":["
  in
  let is_first = ref true in
  let report ~tags ~data ~over src k =
    let field f =
      (Fmt.to_to_string Metrics.pp_key f, Fmt.to_to_string Metrics.pp_value f)
    in
    let data_fields = Metrics.Data.fields data in
    let name = Metrics.Src.name src in
    let tags = data_series_to_yojson @@ List.map field tags in
    let ts =
      match Metrics.Data.timestamp data with
      | Some t -> t |> Int64.of_string |> Mtime.of_uint64_ns
      | None -> Mtime_clock.now ()
    in
    let d =
      if Metrics.Src.duration src then
        let duration, data =
          data_fields |> List.map field |> function
          (* Extract the "duration" field from the data *)
          | ("duration", dur) :: data -> (dur, data)
          | t ->
            (* This is possible if a source with duration:true is used
               with the regular [add] function. *)
            Fmt.epr "Unexpected duration format %a\n"
              Fmt.(
                pair ~sep:comma string string
                |> parens
                |> list ~sep:(const string "; ")
                |> brackets)
              t;
            assert false
        in
        let data = data_series_to_yojson data in
        report_duration ~ts name tags data duration
      else
        let data = List.map field data_fields in
        let data = data_series_to_yojson data in
        report_instant ~ts name tags data
    in
    let to_channel =
      if pretty_print then Yojson.Safe.pretty_to_channel
      else Yojson.Safe.to_channel ?buf:None ?len:None
    in
    if !is_first then is_first := false else output_char out_channel ',';
    to_channel out_channel d;
    over ();
    k ()
  in
  let at_exit () =
    output_string out_channel "]}\n";
    flush out_channel;
    Unix.close file
  in
  Metrics.set_reporter { Metrics.report; now; at_exit }
