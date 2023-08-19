open Tyxml

let html_to_str elt = Format.asprintf "%a" (Html.pp ()) elt
let elt_to_string elt = Format.asprintf "%a" (Html.pp_elt ()) elt

let csrf_tag req =
  Html.(
    input
      ~a:
        [ a_input_type `Hidden
        ; a_name "dream.csrf"
        ; a_value @@ Dream.csrf_token req
        ])
    ()
;;
