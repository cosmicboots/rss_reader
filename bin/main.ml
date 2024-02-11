open Core
open Cmdliner

let sql_uri = "sqlite3:db.sqlite"

let start_dream port interface =
  Dream.run ~port ~interface
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
  let host =
    Arg.(
      value
      & opt string "localhost"
      & info
          [ "i"; "interface" ]
          ~doc:"Interface to bind the server to"
          ~docv:"INTERFACE")
  in
  Cmd.v (Cmd.info "web-server" ~doc:"Start the web server")
  @@ Term.(const start_dream $ port $ host)
;;

let etl_cmd =
  Cmd.v (Cmd.info "etl" ~doc:"Fetch updates from server")
  @@ Term.(const run_etl $ const ())
;;

let create_user username =
  let f () =
    printf "Enter password: ";
    (* TODO: The input should be hidden when reading in the passwords *)
    let password1 = Stdlib.read_line () in
    printf "Re-enter password: ";
    let password2 = Stdlib.read_line () in
    if String.(password1 <> password2)
    then (
      printf "Passwords do not match\n";
      Lwt.return (Error (`Msg "Passwords do not match")))
    else
      let open Lwt_result.Syntax in
      let* conn = Caqti_lwt.connect @@ Uri.of_string sql_uri in
      let* () = Petrol.StaticSchema.initialise Models.Schema.schema conn in
      match Auth_utils.Password.hash_password password1 with
      | Ok (_, encoded) ->
        let* () = Models.User.insert_user ~username ~password:encoded conn in
        Lwt.return_ok ()
      | Error _ -> Lwt.return (Error (`Msg "Failed to hash password"))
  in
  let res = Lwt_main.run (f ()) in
  match res with
  | Ok () -> printf "User created successfully\n"
  | Error (`Msg msg) -> printf "Error: %s\n" msg
  | Error (`Caqti_error err) ->
    printf "Caqti error: %s\n" (Caqti_error.show err)
  | Error (`Connect_failed _) -> printf "`Connect_failed\n"
  | Error (`Connect_rejected _) -> printf "`Connect_rejected\n"
  | Error (`Decode_rejected _) -> printf "`Decode_rejected\n"
  | Error (`Encode_failed _) -> printf "`Encode_failed\n"
  | Error (`Encode_rejected _) -> printf "`Encode_rejected\n"
  | Error (`Load_failed _) -> printf "`Load_failed\n"
  | Error (`Load_rejected _) -> printf "`Load_rejected\n"
  | Error (`Post_connect _) -> printf "`Post_connect\n"
  | Error (`Request_failed _) -> printf "`Request_failed\n"
  | Error (`Request_rejected _) -> printf "`Request_rejected\n"
  | Error ((`Response_failed _) as er) ->
    printf "Response_failed: %s\n" @@ Caqti_error.show er;
  | Error (`Response_rejected _) -> printf "`Response_rejected\n"
;;

let create_user_cmd =
  let username =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"Username" ~docv:"USERNAME")
  in
  Cmd.v (Cmd.info "create-user" ~doc:"Create a user")
  @@ Term.(const create_user $ username)
;;

let group =
  Cmd.group (Cmd.info "rss_reader") [ ws_cmd; etl_cmd; create_user_cmd ]
;;

let () = exit (Cmd.eval group)
