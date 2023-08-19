open Core
open Tyxml
open Base

let feed_manager req =
  let open Lwt_result.Syntax in
  let* feeds = Dream.sql req Models.Channel.get_channels in
  Lwt.return_ok
    Html.(
      table
        ~a:
          [ a_class [ "border"; "dark:border-slate-700" ]
          ; Hx.target (`Closest "tr")
          ; Hx.swap `OuterHTML
          ]
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
      [ Snippets.Utils.csrf_tag req
      ; div ~a:[ a_id "form-msg" ] []
      ; label ~a:[ a_label_for "name" ] [ txt "Name" ]
      ; br ()
      ; input
          ~a:
            [ a_class @@ Snippets.Style.input_style ()
            ; a_name "name"
            ; a_placeholder "Name"
            ]
          ()
      ; br ()
      ; label ~a:[ a_label_for "desc" ] [ txt "Description" ]
      ; br ()
      ; input
          ~a:
            [ a_class @@ Snippets.Style.input_style ()
            ; a_name "desc"
            ; a_placeholder "Description"
            ]
          ()
      ; br ()
      ; label ~a:[ a_label_for "uri" ] [ txt "URI" ]
      ; br ()
      ; input
          ~a:
            [ a_class @@ Snippets.Style.input_style ()
            ; a_name "uri"
            ; a_placeholder "URI"
            ]
          ()
      ; br ()
      ; button
          ~a:
            [ a_button_type `Submit
            ; a_class @@ Snippets.Style.button_style ()
            ]
          [ txt "Add Feed" ]
      ])
;;

let get req =
  let open Lwt.Syntax in
  let* feeds = feed_manager req in
  let* feeds = Caqti_lwt.or_fail feeds in
  setup_page ~title:"Settings"
  @@ Lwt.return
       Html.(
         div
           ~a:[ a_class [ "max-w-screen-md"; "mx-auto"; "px-8" ] ]
           [ h2 [ txt "Current Feeds" ]
           ; feeds
           ; h2 [ txt "Add a Feed" ]
           ; new_feed req
           ])
;;
