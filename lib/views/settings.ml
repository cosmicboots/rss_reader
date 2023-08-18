open Core
open Tyxml
open Base

let feed_manager req =
  let open Lwt_result.Syntax in
  let* feeds = Dream.sql req Models.Channel.get_channels in
  Lwt.return_ok
    Html.(
      table
        ~a:
          [ a_class [ "border"; "dark:border-slate-700" ]
          ; Hx.target (`Closest "tr")
          ; Hx.swap `OuterHTML
          ]
        ~thead:
          (thead
             [ tr
                 [ th [ txt "Name" ]
                 ; th [ txt "Description" ]
                 ; th [ txt "URI" ]
                 ]
             ])
      @@ List.map ~f:Snippets.Feed.feed_elt feeds)
;;

let get req =
  let open Lwt.Syntax in
  let* feeds = feed_manager req in
  let* feeds = Caqti_lwt.or_fail feeds in
  setup_page ~title:"Settings"
  @@ Lwt.return Html.(div [ h1 [ txt "Current Feeds" ]; feeds ])
;;
