open Core
open Cohttp_lwt_unix
open Lwt
(* This is the "extract" step *)

module ParseError = struct
  type t = Channel of string list

  let show = function
    | Channel x -> List.fold ~f:(fun acc x -> acc ^ ", " ^ x) ~init:"" x
  ;;
end

let get_feed ~id db =
  let open Lwt_result.Syntax in
  let* chan = Models.Channel.get_channel ~id db in
  let open Lwt.Syntax in
  let* _resp, body = Client.get (Uri.of_string chan.uri) in
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return_ok body
;;

let translate_chan ~chan_id chan =
  let parse txt =
    let chan, errors = Rss_io.channel_of_string (Rss_io.make_opts ()) txt in
    if List.length errors > 0
    then Result.Error (ParseError.Channel errors)
    else Result.Ok chan
  in
  match parse chan with
  | Ok x ->
    List.map
      ~f:(fun itm ->
        ( Option.value ~default:"" itm.item_title
        , Option.value
            ~default:""
            (let open Option in
             itm.item_guid
             >>| function
             | Rss_types.Guid_name x -> x
             | Rss_types.Guid_permalink x -> Uri.to_string x)
        , chan_id
        , Option.value ~default:"" itm.item_desc
        , List.map ~f:(fun x -> x.cat_name) itm.item_categories ))
      x.ch_items
  | Error e -> failwith @@ ParseError.show e
;;

let load_chan db (title, guid, channel_id, desc, categories) =
  Models.Post.insert_post ~title ~guid ~channel_id ~desc ~categories db
;;

let run ~chan_id db =
  let open Lwt.Syntax in
  let* feed_body = get_feed ~id:chan_id db in
  let* feed_body = Caqti_lwt.or_fail feed_body in
  let raw_items = translate_chan ~chan_id feed_body in
  Lwt_list.iter_s (fun x -> load_chan db x >>= Caqti_lwt.or_fail) raw_items
;;
