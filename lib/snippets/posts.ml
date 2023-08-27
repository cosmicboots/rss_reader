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
      Sl.tree
      @@ List.map
           ~f:(fun (chan_name, itm) ->
             Sl.tree_item
               ~a:
                 [ Hx.get @@ sprintf "/api/posts/%d" itm.id
                 ; Hx.target (`Css "#article-content")
                 ; Hx.swap `InnerHTML
                 ]
               [ span [ txt @@ chan_name ^ ": " ^ itm.title ] ])
           itms)
;;

let get req =
  let open Lwt.Syntax in
  let id = int_of_string @@ Dream.param req "post_id" in
  let* itm = Dream.sql req @@ Models.Post.get_post ~id in
  let* itm = Caqti_lwt.or_fail itm in
  let cat_items =
    List.map ~f:(fun cat -> Html.(Sl.badge [ txt cat ])) itm.categories
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
            ~a:[ Unsafe.string_attrib "slot" "header" ]
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
