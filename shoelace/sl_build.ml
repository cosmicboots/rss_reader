open Core

type value = { type_ : string option [@key "type"] [@default None] }
[@@deriving show { with_path = false }, yojson]

type attribute =
  { name : string
  ; description : string option [@default None]
  ; value : value
  }
[@@deriving show { with_path = false }, yojson]

type element =
  { name : string
  ; docs : string [@key "doc-url"]
  ; attributes : attribute list
  }
[@@deriving show { with_path = false }, yojson { strict = false }]

type html = { elements : element list }
[@@deriving show { with_path = false }, yojson { strict = false }]

type contributions = { html : html }
[@@deriving show { with_path = false }, yojson { strict = false }]

type schema =
  { name : string
  ; version : string
  ; contributions : contributions
  }
[@@deriving show { with_path = false }, yojson { strict = false }]

let read_file ic =
  let rec f acc ic =
    try
      let line = Stdlib.input_line ic in
      f (line :: acc) ic
    with
    | End_of_file -> acc
  in
  List.fold ~f:(fun acc x -> acc ^ "\n" ^ x) ~init:"" @@ List.rev @@ f [] ic
;;

let elt_of_element (element : element) =
  let name = String.chop_prefix_exn ~prefix:"sl-" element.name in
  let name = String.substr_replace_all ~pattern:"-" ~with_:"_" name in
  let reserved_words = Str.regexp {|include|} in
  let name = Str.global_replace reserved_words {|\0_|} name in
  sprintf
    {|
(** [%s children] will create a Shoelace component with [children] as children elements
Docs: %s
Attributes: %s *)
let %s ?a children = Tyxml.Html.Unsafe.node "%s" ?a children
|}
    name
    element.docs
    (String.concat ~sep:" " @@ List.map ~f:(fun x -> x.name) element.attributes)
    name
    element.name
;;

let () =
  let args = Array.to_list @@ Sys.get_argv () in
  let output_file = List.nth_exn args 1 in
  let input_file = List.nth_exn args 2 in
  let ic = Stdlib.open_in input_file in
  let oc = Stdlib.open_out output_file in
  let content = read_file ic in
  let json = schema_of_yojson @@ Yojson.Safe.from_string content in
  (match json with
   | Ok json ->
     printf "%s\n" @@ show_schema json;
     List.iter
       ~f:(fun elt -> Stdlib.output_string oc @@ elt_of_element elt)
       json.contributions.html.elements
   | Error x -> printf "%s\n" @@ "JSON Parse error: " ^ x);
  Stdlib.close_in ic;
  Stdlib.close_out oc;
  ()
;;
