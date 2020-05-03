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

(*************)
(* influxdb line protocol reporter *)
(* from https://docs.influxdata.com/influxdb/v1.5/write_protocols/line_protocol_reference/ *)
(* example line: weather,location=us-midwest temperature=82 1465839830100400200 *)
(*************)

open Astring

let avoid_keyword =
  let keywords =
    String.Set.of_list
      [
        "ALL";
        "ALTER";
        "ANY";
        "AS";
        "ASC";
        "BEGIN";
        "BY";
        "CREATE";
        "CONTINUOUS";
        "DATABASE";
        "DATABASES";
        "DEFAULT";
        "DELETE";
        "DESC";
        "DESTINATIONS";
        "DIAGNOSTICS";
        "DISTINCT";
        "DROP";
        "DURATION";
        "END";
        "EVERY";
        "EXPLAIN";
        "FIELD";
        "FOR";
        "FROM";
        "GRANT";
        "GRANTS";
        "GROUP";
        "GROUPS";
        "IN";
        "INF";
        "INSERT";
        "INTO";
        "KEY";
        "KEYS";
        "KILL";
        "LIMIT";
        "SHOW";
        "MEASUREMENT";
        "MEASUREMENTS";
        "NAME";
        "OFFSET";
        "ON";
        "ORDER";
        "PASSWORD";
        "POLICY";
        "POLICIES";
        "PRIVILEGES";
        "QUERIES";
        "QUERY";
        "READ";
        "REPLICATION";
        "RESAMPLE";
        "RETENTION";
        "REVOKE";
        "SELECT";
        "SERIES";
        "SET";
        "SHARD";
        "SHARDS";
        "SLIMIT";
        "SOFFSET";
        "STATS";
        "SUBSCRIPTION";
        "SUBSCRIPTIONS";
        "TAG";
        "TO";
        "USER";
        "USERS";
        "VALUES";
        "WHERE";
        "WITH";
        "WRITE";
      ]
  in
  fun m -> if String.(Set.mem (Ascii.uppercase m) keywords) then "o" ^ m else m

let escape =
  List.fold_right (fun e m' -> String.(concat ~sep:("\\" ^ e) (cuts ~sep:e m')))

let escape_measurement m = escape [ ","; " " ] (avoid_keyword m)

let escape_name m = escape [ ","; " "; "=" ] (avoid_keyword m)

let pp_value (str : string Fmt.t) ppf f =
  let open Metrics in
  match value f with
  | V (String, s) -> str ppf s
  | V (Int, i) -> Fmt.pf ppf "%di" i
  | V (Int32, i32) -> Fmt.pf ppf "%ldi" i32
  | V (Int64, i64) -> Fmt.pf ppf "%Ldi" i64
  | V (Uint, u) -> Fmt.pf ppf "%ui" u
  | V (Uint32, u32) -> Fmt.pf ppf "%lui" u32
  | V (Uint64, u64) -> Fmt.pf ppf "%Lui" u64
  | _ -> pp_value ppf f

(* we need to:
  - avoid keywords
  - escape comma and space in measurement name
  - escape comma, space and equal in tag key, tag value, field key of type string
  - double-quote field value of type string
  - data type number is a float, suffix i for integers *)
let encode_line_protocol tags data name =
  let data_fields = Metrics.Data.fields data in
  let pp_field_str ppf s = Fmt.pf ppf "%S" s in
  let pp_field ppf f =
    Fmt.(pair ~sep:(unit "=") string (pp_value pp_field_str))
      ppf
      (escape_name (Metrics.key f), f)
  in
  let pp_fields = Fmt.(list ~sep:(unit ",") pp_field) in
  let pp_tag_str ppf s = Fmt.string ppf (escape_name s) in
  let pp_tag ppf f =
    Fmt.(pair ~sep:(unit "=") string (pp_value pp_tag_str))
      ppf
      (escape_name (Metrics.key f), f)
  in
  let pp_tags = Fmt.(list ~sep:(unit ",") pp_tag) in
  Fmt.strf "%s,%a %a\n" (escape_measurement name) pp_tags tags pp_fields
    data_fields

module SM = Map.Make (Metrics.Src)

let lwt_reporter ?tags:(more_tags = []) ?interval send now =
  let m = ref SM.empty in
  let i = match interval with None -> 0L | Some s -> Duration.of_ms s in
  let report ~tags ~data ~over src k =
    let send () =
      m := SM.add src (now ()) !m;
      let str =
        encode_line_protocol (more_tags @ tags) data (Metrics.Src.name src)
      in
      let unblock () =
        over ();
        Lwt.return_unit
      in
      Lwt.finalize (fun () -> send str) unblock |> Lwt.ignore_result;
      k ()
    in
    match SM.find_opt src !m with
    | None -> send ()
    | Some last ->
      if now () > Int64.add last i then send ()
      else (
        over ();
        k ())
  in
  { Metrics.report; now; at_exit = (fun () -> ()) }
