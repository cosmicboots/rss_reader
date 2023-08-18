open Core
open Tyxml

let button_style () =
  [ "rounded-lg"; "bg-slate-300"; "dark:bg-slate-700"; "p-1" ]
;;

let feed_elt (feed : Models.Channel.t) =
  Html.(
    tr
      [ td [ txt feed.name ]
      ; td [ txt feed.desc ]
      ; td [ txt feed.uri ]
      ; td
          [ button
              ~a:
                [ a_class @@ button_style ()
                ; Hx.get @@ sprintf "/api/feeds/%d/edit" feed.id
                ]
              [ txt "Edit" ]
          ]
      ])
;;

let feed_edit_elt (feed : Models.Channel.t) =
  Html.(
    tr
      [ td [ input ~a:[ a_name "name"; a_value feed.name ] () ]
      ; td [ input ~a:[ a_name "desc"; a_value feed.desc ] () ]
      ; td [ input ~a:[ a_name "uri"; a_value feed.uri ] () ]
      ; td
          [ button
              ~a:
                [ a_class @@ button_style ()
                ; Hx.get @@ sprintf "/api/feeds/%d" feed.id
                ]
              [ txt "Cancel" ]
          ; button
              ~a:
                [ a_class @@ button_style ()
                ; Hx.include_ (`Closest "tr")
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
  let* form = Dream.form ~csrf:false req in
  match form with
  | `Ok form_data ->
    let find = List.Assoc.find_exn ~equal:String.equal form_data in
    let id = int_of_string @@ Dream.param req "feed_id" in
    let name = find "name" in
    let desc = find "desc" in
    let uri = find "uri" in
    Lwt.return @@ feed_elt { id; name; desc; uri }
  | _ -> Lwt.return @@ Html.(tr [ td [ txt "ERROR" ] ])
;;

let get_edit req =
  let open Lwt.Syntax in
  let id = int_of_string @@ Dream.param req "feed_id" in
  let* feed = Dream.sql req @@ Models.Channel.get_channel ~id in
  let* feed = Caqti_lwt.or_fail feed in
  Lwt.return @@ feed_edit_elt feed
;;
