open Core
open Tyxml

(** [list ()] returns a list of divs representing a post list *)
let list req =
  let open Lwt.Syntax in
  let* itms = Dream.sql req Models.Post.get_posts in
  let* itms = Caqti_lwt.or_fail itms in
  Dream.log "Pulled %d items from the db" @@ List.length itms;
  Lwt.return
    Html.(
      div
      @@ List.map
           ~f:(fun itm ->
             div
               ~a:
                 [ a_class [ "p-2"; "bg-slate-100"; "border-2" ]
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
  let cat_items =
    List.map ~f:(fun cat -> Html.(li [ txt cat ])) itm.categories
  in
  Dream.log "Cate items: %s" @@ [%show: string list] itm.categories;
  Lwt.return
    Html.(
      div
        ([ h1 [ txt itm.title ]
         ; a
             ~a:
               [ a_href itm.guid
               ; a_target "_blank"
               ; a_class [ "underline"; "text-sky-400" ]
               ]
             [ txt itm.guid ]
         ; br ()
         ]
         @ (if List.length cat_items > 0
            then
              [ span [ txt "Categories:" ]
              ; ul ~a:[ a_class [ "list-disc" ] ] cat_items
              ]
            else [])
         @ [ div ~a:[ a_class [ "bg-slate-100" ] ] [ txt itm.desc ] ]))
;;
