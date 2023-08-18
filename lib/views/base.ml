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

let toolbar () =
  Html.(
    div (* Toolbar div *)
      ~a:
        [ a_class
            [ "space-x-2"
            ; "px-2"
            ; "py-1"
            ; "dark:bg-slate-900"
            ; "dark:text-slate-200"
            ]
        ]
      [ a
          ~a:
            [ a_href "/"
            ; a_class
                [ "text-xl"
                ; "font-sans"
                ; "px-2"
                ; "dark:text-slate-100"
                ; "bg-slate-200"
                ; "dark:bg-slate-800"
                ; "rounded-lg"
                ]
            ]
          [ txt "RSS Reader" ]
      ; a ~a:[ a_href "/new-feed" ] [ txt "Add Feed" ]
      ; a ~a:[ a_href "/settings" ] [ txt "Settings" ]
      ])
;;

let setup_page ~title:title_ body =
  let open Lwt.Syntax in
  let* body_ = body in
  Dream.html
  @@ elt_to_string
  @@ Html.(
       html
         (head (title @@ txt title_) @@ scripts ())
         (body
            ~a:
              [ a_class [ "grid"; "dark:bg-slate-900"; "dark:text-slate-100" ]
              ; a_style "height: 100vh; grid-template-rows: auto 1fr;"
              ]
            [ toolbar (); body_ ]))
;;
