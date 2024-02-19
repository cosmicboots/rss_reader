(lang dune 3.8)

(name rss_reader)

(generate_opam_files true)

(source
 (github cosmicboots/rss_reader))

(authors "Thom Dickson <td3of4@gmail.com>")

(maintainers "Thom Dickson <td3of4@gmail.com>")

(using menhir 2.1)

(package
 (name rss_reader)
 (synopsis "RSS reader")
 (description "A simple RSS reader built in OCaml")
 (depends
  (ocaml
   (>= 5))
  ocamlformat
  cmdliner
  dune
  dream
  dream-livereload
  core
  caqti-lwt
  caqti-dynload
  caqti-driver-sqlite3
  ppx_deriving
  ppx_deriving_yojson
  tyxml
  rss
  petrol
  camelsnakekebab
  cohttp-lwt-unix
  petrol
  ppx_deriving_yojson
  ocamlformat
  argon2))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
