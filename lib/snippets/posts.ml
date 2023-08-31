open Core
open Tyxml

(** [list req] returns a structured shoelace tree with all the posts

    Clicking on a post will trigger an AJAX request to [/api/posts/<id>] *)
let list req =
  let open Lwt.Syntax in
  let* itms = Dream.sql req Models.Post.get_posts in
  let* itms = Caqti_lwt.or_fail itms in
  let itms = List.Assoc.sort_and_group ~compare:String.compare itms in
  Dream.log "Pulled %d items from the db" @@ List.length itms;
  Lwt.return
    Html.(
      Sl.tree
      @@ List.map
           ~f:(fun (chan, itms) ->
             Sl.tree_item
             @@ [ txt chan ]
             @ List.map
                 ~f:(fun itm ->
                   Sl.tree_item
                     ~a:
                       [ Hx.get @@ sprintf "/api/posts/%d" itm.id
                       ; Hx.target (`Css "#article-content")
                       ; Hx.swap `InnerHTML
                       ]
                     [ txt itm.title ])
                 itms)
           itms)
;;

let get req =
  let open Lwt.Syntax in
  let id = int_of_string @@ Dream.param req "post_id" in
  let* itm = Dream.sql req @@ Models.Post.get_post ~id in
  let* itm = Caqti_lwt.or_fail itm in
  let cat_items =
    List.map
      ~f:(fun cat -> Html.(Sl.badge ~a:[ Sl.Badge.pill () ] [ txt cat ]))
      itm.categories
  in
  Lwt.return
    Html.(
      Sl.card
        ~a:
          [ a_style
              {|
                        overflow: auto;
                        max-width:800px;
                        margin:auto;
                        display:block;
                     |}
          ]
        [ div
            ~a:[ Sl.Util.slot "header" ]
            (a
               ~a:[ a_href itm.guid; a_target "_blank" ]
               [ h1 [ txt itm.title ] ]
             ::
             (if List.length cat_items > 0
              then span [ txt "Categories: " ] :: cat_items
              else []))
        ; div [ Unsafe.data itm.desc ]
        ])
;;
