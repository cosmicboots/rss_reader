open Core
open Tyxml
open Snippets.Utils

let scripts () =
  Html.
    [ script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.4" ] @@ txt ""
    ; style [ txt {|
:not(:defined) {
  visibility: hidden;
}
|} ]
    ; link
        ~rel:[ `Stylesheet ]
        ~href:
          "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.7.0/cdn/themes/light.css"
        ()
    ; link
        ~rel:[ `Stylesheet ]
        ~href:
          "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.7.0/cdn/themes/dark.css"
        ()
    ; script
        ~a:
          [ Unsafe.string_attrib "type" "module"
          ; a_src
              "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.7.0/cdn/shoelace-autoloader.js"
          ]
      @@ txt ""
    ]
;;

let toolbar darkmode =
  Html.(
    div (* Toolbar div *)
      ~a:[ a_style "display: flex;" ]
      [ div
          ~a:[ a_style "flex: 1 1 0;" ]
          [ Sl.button
              ~a:[ Sl.Button.variant `Text; a_href "/" ]
              [ txt "RSS Reader" ]
          ; Sl.button
              ~a:[ Sl.Button.variant `Text; a_href "/settings" ]
              [ txt "Settings" ]
          ]
      ; div
          ~a:[ a_style "flex: 0 1 0;" ]
          [ Snippets.Darkmode.dark_indicator darkmode ]
      ])
;;

let setup_page ~title:title_ req body =
  let open Lwt.Syntax in
  let* body_ = body in
  let darkmode =
    match Dream.session_field req "ui.darkmode" with
    | Some "false" -> false
    | _ -> true
  in
  Dream.html
  @@ elt_to_string
  @@ Html.(
       html
         ~a:
           [ a_class
               [ (if darkmode then "sl-theme-dark" else "sl-theme-light") ]
           ; a_style "height:100%"
           ]
         (head (title @@ txt title_)
          @@ [ meta ~a:[ a_content "width=device-width, initial-scale=1" ] () ]
          @ scripts ())
         (body
            ~a:
              [ a_style
                  "height: 100%; margin: 0; display: flex; flex-flow: column;"
              ]
            [ toolbar darkmode; body_ ]))
;;
