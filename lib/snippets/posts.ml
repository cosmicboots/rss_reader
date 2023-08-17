open Core
open Tyxml

(** [list ()] returns a list of divs representing a post list *)
let list req =
  let open Lwt.Syntax in
  let* itms = Dream.sql req Models.Post.get_posts in
  let* itms = Caqti_lwt.or_fail itms in
  Dream.log "Pulled %d items from the db\n" @@ List.length itms;
  Lwt.return
    Html.(
      div
      @@ List.map
           ~f:(fun itm ->
             div
               ~a:
                 [ a_class [ "rounded-lg"; "my-2"; "p-2"; "bg-slate-100" ]
                 ; a_style "cursor: pointer;"
                 ; Hx.get @@ sprintf "/api/posts/%d" itm.id
                 ; Hx.target (`Css "#article-content")
                 ; Hx.swap `InnerHTML
                 ]
               [ span [ txt itm.title ] ])
           itms)
;;

let get req =
  let open Lwt.Syntax in
  let id = int_of_string @@ Dream.param req "post_id" in
  let* itm = Dream.sql req @@ Models.Post.get_post ~id in
  let* itm = Caqti_lwt.or_fail itm in
  Lwt.return Html.(div [ h1 [ txt itm.title ]; p [ txt itm.desc ] ])
;;
