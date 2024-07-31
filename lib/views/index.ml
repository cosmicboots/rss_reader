(** Views for the index page *)

open Core
open Tyxml
open Base

let sidebar req =
  let open Lwt.Syntax in
  let* channels = Snippets.Channels.list req in
  Lwt.return
    Html.(
      div
        ~a:[ Sl.Util.slot "start"; a_style "overflow: auto; padding: 1rem;" ]
        [ h1 [ txt "Categories." ]; channels ])
;;

let content () =
  Html.(
    div
      ~a:[ Sl.Util.slot "end"; a_style "overflow: auto;" ]
      [ div
          ~a:[ a_id "article-content"; a_style "padding: 1rem;" ]
          [ p [ em [ txt "Choose an article on the left" ] ] ]
      ])
;;

let page req =
  let open Lwt.Syntax in
  let* sidebar = sidebar req in
  Lwt.return
    Html.(
      if Utils.is_mobile req
      then div [ sidebar; content () ]
      else
        Sl.split_panel
          ~a:
            [ Sl.SplitPanel.position 50
            ; a_style "overflow: auto; height: 100%;"
            ]
          [ Sl.split_panel
              ~a:
                [ Sl.Util.slot "start"
                ; a_style "overflow: auto; height: 100%;"
                ]
              [ sidebar
              ; div
                  ~a:
                    [ Sl.Util.slot "end"
                    ; a_id "article-list"
                    ; a_style "overflow: auto; height: 100%;"
                    ]
                  []
              ]
          ; content ()
          ])
;;

(** The get endpoint for the index page *)
let get req = setup_page ~title:"RSS Reader" req @@ page req
