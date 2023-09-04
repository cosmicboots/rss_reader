open Core

let dark_indicator darkmode =
  Sl.tooltip
    ~a:
      [ Sl.Tooltip.content "Toggle Darkmode"
      ; Hx.get "/api/toggle-darkmode"
      ; Hx.swap `OuterHTML
      ; Tyxml.Html.Unsafe.string_attrib "hx-on:click"
        @@ sprintf
             "document.documentElement.className = \"%s\""
             (if darkmode then "sl-theme-light" else "sl-theme-dark")
      ]
    [ Sl.icon_button
        ~a:
          [ (Sl.IconButton.name
             @@ if darkmode then `String "brightness-high" else `String "moon"
            )
          ]
        []
    ]
;;

let get req =
  let open Lwt.Syntax in
  let darkmode =
    match Dream.session_field req "ui.darkmode" with
    | Some "false" -> false
    | _ -> true
  in
  let* () =
    Dream.log "Swapping ui.darkmode";
    Dream.set_session_field req "ui.darkmode" (string_of_bool @@ not darkmode)
  in
  Lwt.return @@ dark_indicator (not darkmode)
;;
