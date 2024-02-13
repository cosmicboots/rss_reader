open Core
open Tyxml
open Base

let feed_manager req =
  let open Lwt_result.Syntax in
  let* feeds = Dream.sql req Models.Channel.get_channels in
  Lwt.return_ok
    Html.(
      table
        ~a:[ Hx.target (`Closest "tr"); Hx.swap `OuterHTML ]
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

let new_feed req =
  Html.(
    form
      ~a:
        [ Hx.post "/api/feeds"
        ; Hx.target @@ `Css "#form-msg"
        ; Hx.swap `InnerHTML
        ]
      [ Dream.csrf_tag req |> Unsafe.data
      ; div ~a:[ a_id "form-msg" ] []
      ; Sl.input ~a:[ a_name "name"; a_placeholder "Name"; a_label "Name" ] []
      ; Sl.input
          ~a:
            [ a_name "desc"
            ; a_placeholder "Description"
            ; a_label "Description"
            ]
          []
      ; Sl.input ~a:[ a_name "uri"; a_placeholder "URI"; a_label "URI" ] []
      ; Sl.button ~a:[ a_button_type `Submit ] [ txt "Add Feed" ]
      ])
;;

let get req =
  let open Lwt.Syntax in
  let* feeds = feed_manager req in
  let* feeds = Caqti_lwt.or_fail feeds in
  setup_page ~title:"Settings" req
  @@ Lwt.return
       Html.(
         div
           ~a:[ a_style "margin: 0 auto; max-width: 800px;" ]
           [ Sl.card
               ~a:[ a_style "width: 100%; margin: 1rem auto;" ]
               [ h2 ~a:[ Sl.Util.slot "header" ] [ txt "Current Feeds" ]
               ; feeds
               ]
           ; Sl.card
               ~a:[ a_style "width:100%; margin: 1rem auto;" ]
               [ h2 ~a:[ Sl.Util.slot "header" ] [ txt "Add a Feed" ]
               ; new_feed req
               ]
           ])
;;
