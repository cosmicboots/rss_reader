(** Views for the index page *)

open Core
open Tyxml
open Base

let sidebar req =
  let open Lwt.Syntax in
  let* posts = Snippets.Posts.list req in
  Lwt.return
    Html.(
      div
        ~a:[ Sl.Util.slot "start"; a_style "overflow: auto; padding: 1rem;" ]
        [ h1 [ txt "Article List" ]
        ; Sl.button
          (* TODO: This refresh button should run the etl batch to refresh the feeds *)
            ~a:[ Hx.get "/api/posts"; Hx.target (`Css "#post-list") ]
            [ Sl.icon
                ~a:
                  [ Sl.Util.slot "prefix"
                  ; Sl.IconButton.name (`String "arrow-clockwise")
                  ]
                []
            ; txt "Refresh"
            ]
        ; Sl.divider []
        ; div ~a:[ a_id "post-list" ] [ posts ]
        ])
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
            [ Sl.SplitPanel.position 25
            ; a_style "flex: 1 1 1px; overflow: auto;"
            ]
          [ sidebar; content () ])
;;

(** The get endpoint for the index page *)
let get req = setup_page ~title:"RSS Reader" req @@ page req
