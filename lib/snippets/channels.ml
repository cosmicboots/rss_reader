open Lwt.Syntax
open Tyxml

let entry_with_desc entry =
  let tt msg elt = Sl.tooltip ~a:[ Sl.Tooltip.content msg ] [ elt ] in
  let btn entry =
    Html.(
      Sl.tree_item
        ~a:
          [ Hx.swap `InnerHTML
          ; Hx.target (`Css "#article-list")
          ; Hx.get @@ Printf.sprintf "/api/channel/%d" entry.Models.Channel.id
          ]
        [ txt entry.name ])
  in
  if entry.Models.Channel.desc <> ""
  then tt entry.desc @@ btn entry
  else btn entry
;;

let list req =
  let* itms = Dream.sql req Models.Channel.get_channels in
  let* itms = Caqti_lwt.or_fail itms in
  Lwt.return (Sl.tree @@ List.map entry_with_desc itms)
;;
