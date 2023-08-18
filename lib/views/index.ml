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
        ~a:
          [ a_class [ "bg-slate-200"; "dark:bg-slate-800"; "overflow-scroll" ]
          ]
        [ h1 ~a:[ a_class [ "px-4" ] ] [ txt "Article List" ]; posts ])
;;

let content () =
  Html.(
    div
      ~a:
        [ a_class
            [ "col-start-2"
            ; "bg-slate-200"
            ; "dark:bg-slate-800"
            ; "p-4"
            ; "overflow-scroll"
            ]
        ]
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
      div
        ~a:
          [ a_class [ "grid" ]
          ; a_style "overflow: auto; grid-template-columns: 300px auto;"
          ]
        [ sidebar; content () ])
;;

(** The get endpoint for the index page *)
let get req = setup_page ~title:"RSS Reader" @@ page req
