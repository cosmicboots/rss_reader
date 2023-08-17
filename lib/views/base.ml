open Core
open Tyxml
open Utils

let scripts () =
  Html.
    [ script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.4" ] @@ txt ""
    ; script ~a:[ a_src "https://cdn.tailwindcss.com" ] @@ txt ""
    ; style
        ~a:[ Unsafe.string_attrib "type" "text/tailwindcss" ]
        [ txt
            "@layer base {\n\
            \   h1 {\n\
            \      @apply text-3xl;\n\
            \      @apply my-4;\n\
            \   }\n\
            \   h2 {\n\
            \      @apply text-2xl;\n\
            \      @apply my-4;\n\
            \   }\n\
            \   h3 {\n\
            \      @apply text-xl;\n\
            \      @apply my-4;\n\
            \    }\n\
             }"
        ]
    ]
;;

let sidebar req =
  let open Lwt.Syntax in
  let* posts = Snippets.Posts.list req in
  Lwt.return
    Html.(
      div
        ~a:
          [ a_class
              [ "col-span-1"
              ; "bg-slate-200"
              ; "rounded-lg"
              ; "px-2"
              ; "py-4"
              ; "overflow-scroll"
              ]
          ]
        [ h1 [ txt "Article List" ]; posts ])
;;

let setup_page req title_ body_ =
  let open Lwt.Syntax in
  let* sidebar = sidebar req in
  Dream.html
  @@ elt_to_string
  @@ Html.(
       html
         (head (title @@ txt title_) @@ scripts ())
         (body
            ~a:
              [ a_class [ "grid grid-cols-4 gap-4 m-4" ]
              ; a_style "height: 100%; margin: 0; padding: 10px;"
              ]
            [ sidebar; body_ ]))
;;
