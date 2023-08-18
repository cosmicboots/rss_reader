open Tyxml
open Base

let get _req =
  setup_page ~title:"Settings"
  @@ Lwt.return Html.(span [ txt "Hello from the settings page" ])
;;
