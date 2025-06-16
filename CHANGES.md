## v0.5.0 (2025-06-16)

- Cache_reporter: add a get_cache function, remove the returned get_cache
  function, document get_cache depends on cache_reporter (#63 @reynir @hannesm)
- Cache_reporter: add a callback function for pushing updates (#63 @hannesm)
- Cache_reporter: preserve a list of (tags, data), fixes #59 (#68 @hannesm)
- Update uuidm (#61 @reynir)
- Fix documentation link (reported in #62 by @reynir, fixed #69 by @hannesm)
- Fix documentation of Src.v (reported in #64 by @reynir, fixed #69 by @hannesm)

## v0.4.1 (2023-06-08)

- metrics-unix: adapt to mtime 2.0.0 changes (remove dependency on mtime.clock)
  (#58 @adatario)

## v0.4.0 (2022-01-28)

- metrics-influx: remove astring dependency (#52 @hannesm)
- metrics-rusage: remove rresult dependency (#52 @hannesm)
- metrics-mirage: remove this package (unused, #53 @hannesm)
- metrics: provide tags_enabled and all_enabled to allow introspection what
  sources are enabled (#55 @hannesm, requested by @reynir in #54)

## v0.3.0 (2021-06-02)

- Add a metrics-rusage opam package that collects data from getrusage and
  /proc/self/stat (and /proc/self/statm). (#50, @reynir and @hannesm)
- Update ocamlformat to 0.18.0
- "pinned" is now "dev" in opam files

## 0.2.0 (2020-05-03)

- Add mirage layer and influxdb reporter (#28, @hannesm)
- Gnuplot: namespacing improvements (#34, @CraigFe)
- Gnuplot: optional graph generation (#35, @CraigFe)
- Support OCaml 4.08 (#37, @CraigFe)
- Use OCamlFormat 0.14.1 (#38, #45, @CraigFe and @samoht)
- opam: remove the 'build' directive on dune dependency (#43, @CraigFe)
- introduce Metrics.cache_reporter -- a reporter holding the most recent
  measurement from each reporting sources (#42, @hannesm)
- Influx: expose the "encode_line_protocol" function (#42, @hannesm)
- Metrics_lwt: provide a source based on Logs.warn_count and
  Logs.error_count (#42, @hannesm)
- Metrics_lwt: provide a function to periodically poll a source
  (used e.g. for GC stats etc.) (#42, @hannesm)
- Mirage: fix the mirage subpackage for newer mirage APIs (#42, @hannesm)

## 0.1.0 (2018-10-19)

Initial version
