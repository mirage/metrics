type rusage = {
  utime : int64 * int;
  stime : int64 * int;
  maxrss : int64;
  ixrss : int64;
  idrss : int64;
  isrss : int64;
  minflt : int64;
  majflt : int64;
  nswap : int64;
  inblock : int64;
  outblock : int64;
  msgsnd : int64;
  msgrcv : int64;
  nsignals : int64;
  nvcsw : int64;
  nivcsw : int64;
}

type kinfo_mem = {
  vsize : int64;
  rss : int64;
  tsize : int64;
  dsize : int64;
  ssize : int64;
  runtime : int64;
  cow : int;
  start : int64 * int;
}

let ( let* ) = Result.bind

external sysconf_clock_tick : unit -> int = "metrics_sysconf_clock_tick"
external sysctl_kinfo_proc : int -> kinfo_mem = "metrics_sysctl_kinfo_proc"
external getrusage : unit -> rusage = "metrics_rusage"
external uname : unit -> string = "metrics_uname"

let wrap f arg = try Ok (f arg) with e -> Error (`Msg (Printexc.to_string e))

let string_of_file filename =
  try
    let fh = open_in filename in
    let content = input_line fh in
    close_in_noerr fh;
    Ok content
  with _ -> Error (`Msg (Fmt.str "Error reading file %S" filename))

let parse_proc_stat s =
  match String.rindex_opt s ')' with
  | None -> Error (`Msg "unable to parse /proc/self/stat")
  | Some idx ->
    let rest = String.sub s idx (String.length s - idx) in
    Ok (String.split_on_char ' ' rest)

let linux_kinfo () =
  let* start =
    match Unix.stat "/proc/self" with
    | { Unix.st_ctime = start; _ } ->
      let frac = Float.rem start 1. in
      Ok (Int64.of_float start, int_of_float (frac *. 1_000_000.))
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Error (`Msg "failed to stat process")
  in
  (* reading /proc/self/stat - since it may disappear mid-time,
     best to have it in memory *)
  let* data = string_of_file "/proc/self/stat" in
  let* stat_vals = parse_proc_stat data in
  let* data = string_of_file "/proc/self/statm" in
  let statm_vals = String.split_on_char ' ' data in
  let i64 s =
    try Ok (Int64.of_string s)
    with Failure _ -> Error (`Msg "couldn't parse integer")
  in
  let us_of_int64 t =
    let clock_tick = Int64.of_int (sysconf_clock_tick ()) in
    let ( * ) = Int64.mul and ( / ) = Int64.div in
    t * 1_000_000L / clock_tick
  in
  if List.length stat_vals >= 50 && List.length statm_vals >= 7 then
    let* utime = i64 (List.nth stat_vals 11) in
    (* divide by sysconf(_SC_CLK_TCK) *)
    let* stime = i64 (List.nth stat_vals 12) in
    (* divide by sysconf(_SC_CLK_TCK) *)
    let runtime = us_of_int64 Int64.(add utime stime) in
    let* vsize = i64 (List.nth stat_vals 20) in
    (* in bytes *)
    let* rss = i64 (List.nth stat_vals 21) in
    (* in pages *)
    let* tsize = i64 (List.nth statm_vals 3) in
    let* dsize = i64 (List.nth statm_vals 5) in
    (* data + stack *)
    let* ssize = i64 (List.nth statm_vals 5) in
    (* data + stack *)
    Ok { vsize; rss; tsize; dsize; ssize; runtime; cow = 0; start }
  else Error (`Msg "couldn't read /proc/self/stat")

let tv (sec, usec) = Int64.to_float sec +. (float_of_int usec /. 1_000_000.)

open Metrics

let rusage_src ~tags =
  let doc = "System rusage counters" in
  let graph = Graph.v ~title:doc ~ylabel:"value" () in
  let data () =
    match wrap getrusage () with
    | Error (`Msg _) -> Data.v []
    | Ok ru ->
      Data.v
        [
          float "utime" ~graph (tv ru.utime);
          float "stime" ~graph (tv ru.stime);
          uint64 "maxrss" ~graph ru.maxrss;
          uint64 "ixrss" ~graph ru.ixrss;
          uint64 "idrss" ~graph ru.idrss;
          uint64 "isrss" ~graph ru.isrss;
          uint64 "minflt" ~graph ru.minflt;
          uint64 "maxflt" ~graph ru.majflt;
          uint64 "nswap" ~graph ru.nswap;
          uint64 "inblock" ~graph ru.inblock;
          uint64 "outblock" ~graph ru.outblock;
          uint64 "msgsnd" ~graph ru.msgsnd;
          uint64 "msgrcv" ~graph ru.msgrcv;
          uint64 "nsignals" ~graph ru.nsignals;
          uint64 "nvcsw" ~graph ru.nvcsw;
          uint64 "nivcsw" ~graph ru.nivcsw;
        ]
  in
  Src.v ~doc ~tags ~data "resource_usage"

let kinfo_mem_src ~tags =
  let doc = "System kernel information" in
  let graph = Graph.v ~title:doc ~ylabel:"value" () in
  let uname = uname () and pid = Unix.getpid () in
  let kinfo () =
    match uname with
    | "FreeBSD" -> wrap sysctl_kinfo_proc pid
    | "Linux" -> linux_kinfo ()
    | s -> Error (`Msg ("unsupported operating system " ^ s))
  in
  (match kinfo () with
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "error while collecting kinfo: %s" msg)
  | Ok _ -> ());
  let data () =
    match kinfo () with
    | Error _ -> Data.v []
    | Ok mem ->
      let now = Unix.gettimeofday () in
      let uptime = now -. tv mem.start in
      Data.v
        [
          uint64 "vsize" ~graph mem.vsize;
          uint64 "rss" ~graph mem.rss;
          uint64 "tsize" ~graph mem.tsize;
          uint64 "dsize" ~graph mem.dsize;
          uint64 "ssize" ~graph mem.ssize;
          uint "cow_faults" ~graph mem.cow;
          uint64 "runtime" ~graph mem.runtime;
          float "uptime" ~graph uptime;
        ]
  in
  Src.v ~doc ~tags ~data "kinfo_mem"
