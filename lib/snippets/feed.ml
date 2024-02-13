open Core
open Tyxml

let feed_elt (feed : Models.Channel.t) =
  Html.(
    tr
      [ td [ txt feed.name ]
      ; td [ txt feed.desc ]
      ; td [ txt feed.uri ]
      ; td
          [ Sl.button
              ~a:[ Hx.get @@ sprintf "/api/feeds/%d/edit" feed.id ]
              [ txt "Edit" ]
          ; Sl.button
              ~a:
                [ Hx.confirm "Are you sure you want to delete?"
                ; Hx.delete @@ sprintf "/api/feeds/%d" feed.id
                ]
              [ txt "Delete" ]
          ]
      ])
;;

let feed_edit_elt (feed : Models.Channel.t) req =
  Html.(
    tr
      [ td [ input ~a:[ a_name "name"; a_value feed.name ] () ]
      ; td [ input ~a:[ a_name "desc"; a_value feed.desc ] () ]
      ; td [ input ~a:[ a_name "uri"; a_value feed.uri ] () ]
      ; td
          [ Dream.csrf_tag req |> Unsafe.data
          ; Sl.button
              ~a:[ Hx.get @@ sprintf "/api/feeds/%d" feed.id ]
              [ txt "Cancel" ]
          ; Sl.button
              ~a:
                [ Hx.include_ (`Closest "tr")
                ; Hx.put @@ sprintf "/api/feeds/%d" feed.id
                ]
              [ txt "Save" ]
          ]
      ])
;;

let get req =
  let open Lwt.Syntax in
  let id = int_of_string @@ Dream.param req "feed_id" in
  let* feed = Dream.sql req @@ Models.Channel.get_channel ~id in
  let* feed = Caqti_lwt.or_fail feed in
  Lwt.return @@ feed_elt feed
;;

let put req =
  let open Lwt.Syntax in
  let* form = Dream.form req in
  match form with
  | `Ok form_data ->
    let find = List.Assoc.find_exn ~equal:String.equal form_data in
    let id = int_of_string @@ Dream.param req "feed_id" in
    let name = find "name" in
    let desc = find "desc" in
    let uri = find "uri" in
    let* res =
      Dream.sql req @@ Models.Channel.update_channel ~id ~name ~desc ~uri
    in
    let* () = Caqti_lwt.or_fail res in
    Lwt.return @@ feed_elt { id; name; desc; uri }
  | _ -> Lwt.return @@ Html.(tr [ td [ txt "ERROR" ] ])
;;

let post req =
  let open Lwt.Syntax in
  let* form = Dream.form req in
  match form with
  | `Ok form_data ->
    let find = List.Assoc.find_exn ~equal:String.equal form_data in
    let name = find "name" in
    let desc = find "desc" in
    let uri = find "uri" in
    let* res =
      Dream.sql req @@ Models.Channel.insert_channel ~name ~desc ~uri
    in
    let* () = Caqti_lwt.or_fail res in
    Lwt.return @@ Html.(span [ txt "Success!" ])
  | _ -> Lwt.return @@ Html.(span [ txt "ERROR" ])
;;

let delete req =
  let open Lwt.Syntax in
  let id = int_of_string @@ Dream.param req "feed_id" in
  let* res = Dream.sql req @@ Models.Channel.delete_channel ~id in
  let* () = Caqti_lwt.or_fail res in
  Lwt.return ()
;;

let get_edit req =
  let open Lwt.Syntax in
  let id = int_of_string @@ Dream.param req "feed_id" in
  let* feed = Dream.sql req @@ Models.Channel.get_channel ~id in
  let* feed = Caqti_lwt.or_fail feed in
  Lwt.return @@ feed_edit_elt feed req
;;
