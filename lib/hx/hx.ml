open Tyxml

module SwapTarget = struct
  let to_string = function
    | `InnerHTML -> "innerHTML"
    | `OuterHTML -> "outerHTML"
    | `Beforebegin -> "beforebegin"
    | `Afterbegin -> "afterbegin"
    | `Beforeend -> "beforeend"
    | `Afterend -> "afterend"
    | `Delete -> "delete"
    | `None -> "none"
  ;;
end

let swap target =
  Html.Unsafe.string_attrib "hx-swap" @@ SwapTarget.to_string target
;;

module Target = struct
  let to_string = function
    | `This -> "this"
    | `Css s -> s
    | `Closest s -> "closest " ^ s
    | `Find s -> "find " ^ s
    | `Next s -> "next " ^ s
    | `Previous s -> "previous " ^ s
  ;;
end

let target target =
  Html.Unsafe.string_attrib "hx-target" @@ Target.to_string target
;;

let include_ elt =
  Html.Unsafe.string_attrib "hx-include" @@ Target.to_string elt
;;

let get path = Html.Unsafe.string_attrib "hx-get" path
let post path = Html.Unsafe.string_attrib "hx-post" path
let put path = Html.Unsafe.string_attrib "hx-put" path
let delete path = Html.Unsafe.string_attrib "hx-delete" path
let confirm msg = Html.Unsafe.string_attrib "hx-confirm" msg

module TriggerEvent = struct
  let to_string = function
    | `Click -> "click"
    | `Load -> "load"
    | `Revealed -> "revealed"
  ;;
end

let trigger ?(once = false) ?(changed = false) ?delay ?throttle event =
  let parse_time_arg name = function
    | Some x -> " " ^ name ^ ":" ^ Int.to_string x
    | None -> ""
  in
  (* Setup the trigger event modifiers *)
  let args = ref "" in
  if once then args := !args ^ " once";
  if changed then args := !args ^ " changed";
  args := !args ^ parse_time_arg "throttle" throttle;
  args := !args ^ parse_time_arg "delay" delay;
  (* Construct the html attribute *)
  Html.Unsafe.string_attrib "hx-trigger"
  @@ TriggerEvent.to_string event
  ^ !args
;;
