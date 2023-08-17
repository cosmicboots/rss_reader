(** Views for the index page *)

open Core
open Tyxml
open Base

let page _req =
  Html.(
    div
      ~a:
        [ a_class
            [ "col-start-2"; "col-end-5"; "bg-slate-200"; "rounded-lg"; "p-4" ]
        ]
      [ h1 [ txt "Article Content" ]
      ; div ~a:[ a_id "article-content" ] [ p [ txt "Hello world" ] ]
      ])
;;

(** The get endpoint for the index page *)
let get req = setup_page req "RSS Reader" @@ page req
