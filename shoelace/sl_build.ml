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

let safe_name name =
  let reserved_words =
    Str.regexp
      (String.concat ~sep:{|\||} [ "include"; "open"; "lazy"; "type" ])
  in
  Str.global_replace reserved_words {|\0_|} name
;;

let safe_variant name =
  Camelsnakekebab.upper_camel_case name
  |> String.substr_replace_all ~pattern:"/" ~with_:"_"
  |> Str.global_replace (Str.regexp {|^[0-9]|}) {|X\0|}
;;

let gen_attribute (attribute : attribute) =
  let name = safe_name @@ Camelsnakekebab.lower_snake_case attribute.name in
  let open Option in
  let open Value_parser.Value_type in
  let line =
    attribute.value.type_
    >>| Value_parser.Main.parse
    >>| function
    | BooleanT ->
      sprintf
        "let %s () = Tyxml.Html.Unsafe.string_attrib \"%s\" \"\""
        name
        attribute.name
    | NumberT ->
      sprintf
        "let %s x = Tyxml.Html.Unsafe.int_attrib \"%s\" x"
        name
        attribute.name
    | StringT ->
      sprintf
        "let %s x = Tyxml.Html.Unsafe.string_attrib \"%s\" x"
        name
        attribute.name
    | List _ ->
      sprintf
        "let %s x = Tyxml.Html.Unsafe.space_sep_attrib \"%s\" x"
        name
        attribute.name
    | Union lst ->
      let variants =
        List.filter_map
          ~f:(function
            | String s -> Some (sprintf "`%s -> \"%s\"\n" (safe_variant s) s)
            | StringT -> Some {|`String s -> s |}
            | _ -> None)
          lst
      in
      if List.length variants > 0
      then (
        let fun_name = Camelsnakekebab.lower_snake_case attribute.name in
        sprintf
          {|let %s x = Tyxml.Html.Unsafe.string_attrib "%s" @@ (function
                | %s) x
           |}
          (safe_name fun_name)
          attribute.name
          (String.concat ~sep:" | " variants))
      else ""
    | Function (_, _) ->
      (* TODO: This API should be improved. Need to have a way to declare what
         kind of js function is used *)
      sprintf
        "let %s x = Tyxml.Html.Unsafe.string_attrib \"%s\" x"
        name
        attribute.name
    | String _ ->
      eprintf "WARN: String type not implemented yet\n";
      ""
    | Const c ->
      eprintf "WARN: Const type not implemented yet: (%s)\n" c;
      ""
    | Empty -> ""
  in
  match attribute.description, line with
  | Some desc, Some l when String.length l > 1 ->
    Some (sprintf "\n(** %s *)\n" desc ^ l)
  | None, Some l -> Some l
  | _ -> None
;;

let gen_module (element : element) =
  let name =
    Camelsnakekebab.upper_camel_case
    @@ String.chop_prefix_exn ~prefix:"sl-" element.name
  in
  let attributes = List.map ~f:gen_attribute element.attributes in
  let attributes_str =
    List.fold
      ~f:(fun acc x ->
        match x with
        | Some "" -> acc
        | Some x -> acc ^ x ^ ";;\n"
        | None -> acc)
      ~init:""
      attributes
  in
  if String.length attributes_str > 0
  then sprintf {|
module %s = struct

    %s
end
|} name attributes_str
  else ""
;;

let elt_of_element (element : element) =
  let name = String.chop_prefix_exn ~prefix:"sl-" element.name in
  let name = String.substr_replace_all ~pattern:"-" ~with_:"_" name in
  let name = safe_name name in
  let module_ = gen_module element in
  sprintf
    {|
%s
(** [%s children] will create a Shoelace component with [children] as children elements
Docs: %s
Attributes: %s *)
let %s ?a children = Tyxml.Html.Unsafe.node "%s" ?a children
|}
    module_
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
     let _ = show_schema json in
     (*printf "%s\n" @@ show_schema json;*)
     List.iter
       ~f:(fun elt -> Stdlib.output_string oc @@ elt_of_element elt)
       json.contributions.html.elements
   | Error x -> printf "%s\n" @@ "JSON Parse error: " ^ x);
  Stdlib.close_in ic;
  Stdlib.close_out oc;
  ()
;;
