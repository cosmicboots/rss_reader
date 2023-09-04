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
        ~a:[ Sl.Util.slot "start"; a_style "overflow: auto" ]
        [ h1 [ txt "Article List" ]; posts ])
;;

let content () =
  Html.(
    div
      ~a:[ Sl.Util.slot "end"; a_style "overflow: auto;" ]
      [ div
          ~a:[ a_id "article-content" ]
          [ p [ em [ txt "Choose an article on the left" ] ] ]
      ])
;;

let page req =
  let open Lwt.Syntax in
  let* sidebar = sidebar req in
  Lwt.return
    Html.(
      Sl.split_panel
        ~a:
          [ Sl.SplitPanel.position 25
          ; a_style "flex: 1 1 1px; overflow: auto;"
          ]
        [ sidebar; content () ])
;;

(** The get endpoint for the index page *)
let get req = setup_page ~title:"RSS Reader" @@ page req
