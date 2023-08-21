open Tyxml

let html_to_str elt = Format.asprintf "%a" (Html.pp ()) elt
let elt_to_string elt = Format.asprintf "%a" (Html.pp_elt ()) elt
