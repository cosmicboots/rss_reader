open Core
open Cmdliner

let sql_uri = "sqlite3:db.sqlite"

let start_dream port =
  Dream.run ~port
  @@ Dream.logger
  @@ Dream.sql_pool sql_uri
  @@ Dream.memory_sessions
  (*@@ Dream_livereload.inject_script ()*)
  @@ Dream.router Router.routes
;;

let run_etl () =
  let f =
    printf "\n=== Running ETL ===\n";
    let open Lwt_result.Syntax in
    let* conn = Caqti_lwt.connect @@ Uri.of_string sql_uri in
    let* () = Petrol.StaticSchema.initialise Models.Schema.schema conn in
    let open Lwt.Syntax in
    let* () = Etl.run conn in
    Lwt.return_ok ()
  in
  let _ = Lwt_main.run f in
  ()
;;

let ws_cmd =
  let port =
    Arg.(
      value
      & opt int 8080
      & info [ "p"; "port" ] ~doc:"Port to bind the server to" ~docv:"PORT")
  in
  Cmd.v (Cmd.info "web-server" ~doc:"Start the web server")
  @@ Term.(const start_dream $ port)
;;

let etl_cmd =
  Cmd.v (Cmd.info "etl" ~doc:"Fetch updates from server")
  @@ Term.(const run_etl $ const ())
;;

let group = Cmd.group (Cmd.info "rss_reader") [ ws_cmd; etl_cmd ]
let () = exit (Cmd.eval group)
