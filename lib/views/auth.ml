open Tyxml
open Base

let page req =
  Lwt.return
    Html.(
      Sl.card
        [ h1 [ txt "Login" ]
        ; form
            ~a:[ a_method `Post ]
            [ Dream.csrf_tag req |> Unsafe.data
            ; Sl.input
                ~a:
                  [ a_input_type `Text
                  ; a_name "username"
                  ; a_placeholder "Username"
                  ; a_label "Username"
                  ]
                []
            ; Sl.input
                ~a:
                  [ a_input_type `Password
                  ; a_name "password"
                  ; a_placeholder "Password"
                  ; a_label "Password"
                  ]
                []
            ; Sl.input ~a:[ a_input_type `Submit; a_value "Login" ] []
            ]
        ])
;;

let login_get req =
  match Dream.session_field req "user" with
  | Some _ -> Dream.redirect req "/"
  | None -> setup_page ~title:"Login" req @@ page req
;;

let login_post req =
  Dream.log "Login post";
  let open Lwt.Syntax in
  let* form = Dream.form req in
  let* () =
    match form with
    | `Ok form_data ->
      let username = List.assoc "username" form_data in
      let password = List.assoc "password" form_data in
      (*Dream.set_session_field req "user" username*)
      let* user = Dream.sql req @@ Models.User.get_user ~username in
      let* () =
        match user with
        | Ok user ->
          (* Check password *)
          (match
             Auth_utils.Password.verify_password ~hash:user.password password
           with
           | Ok true ->
             let* () = Dream.set_session_field req "user" username in
             Dream.add_flash_message req "success" "Logged in";
             Lwt.return ()
           | Ok false ->
             Dream.add_flash_message req "error" "Invalid password";
             Lwt.return ()
           | Error _ ->
             Dream.add_flash_message req "error" "Error verifying password";
             Lwt.return ())
        | Error _ ->
          Dream.add_flash_message req "error" "User doesn't exist";
          Lwt.return ()
      in
      Lwt.return ()
    | _ -> Lwt.return ()
  in
  Dream.redirect req "/"
;;

let logout req =
  let open Lwt.Syntax in
  let* () = Dream.drop_session_field req "user" in
  Dream.redirect req "/"
;;
