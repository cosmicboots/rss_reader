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

let toolbar () =
  Html.(
    div (* Toolbar div *)
      ~a:[]
      [ Sl.button
          ~a:[ Unsafe.string_attrib "variant" "text"; a_href "/" ]
          [ txt "RSS Reader" ]
      ; Sl.button
          ~a:[ Unsafe.string_attrib "variant" "text"; a_href "/settings" ]
          [ txt "Settings" ]
      ])
;;

let setup_page ~title:title_ body =
  let open Lwt.Syntax in
  let* body_ = body in
  Dream.html
  @@ elt_to_string
  @@ Html.(
       html
         ~a:[ a_class [ "sl-theme-dark" ]; a_style "height:100%" ]
         (head (title @@ txt title_) @@ scripts ())
         (body
            ~a:
              [ a_style
                  "height: 100%; margin: 0; display: flex; flex-flow: column;"
              ]
            [ toolbar (); body_ ]))
;;
