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

(** [get_feed ~id db] will issue a Cohttp request to fetch a feed with an id of [id]

    [db] is the database connection *)
let get_feed ~id db =
  let open Lwt_result.Syntax in
  let* chan = Models.Channel.get_channel ~id db in
  let open Lwt.Syntax in
  let* _resp, body = Client.get (Uri.of_string chan.uri) in
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return_ok body
;;

(** [translate_chan ~chan_id chan] *)
let translate_chan ~chan_id chan =
  let chan = Syndic.Rss2.parse @@ Xmlm.make_input (`String (0, chan)) in
  List.map
    ~f:(fun itm ->
      ( Option.value ~default:"" @@ Option.map itm.source ~f:(fun x -> x.data)
      , Option.value
          ~default:""
          (let open Option in
           itm.link >>| Uri.to_string)
      , Option.value ~default:Ptime.epoch itm.pubDate
      , chan_id
      , snd itm.content
      , Option.value ~default:[]
        @@ Option.map itm.category ~f:(fun x -> String.split x.data ~on:'/') ))
    chan.items
;;

let load_chan db (title, guid, date, channel_id, desc, categories) =
  Models.Post.insert_post ~title ~guid ~date ~channel_id ~desc ~categories db
;;

let run ?chan_id db =
  let open Lwt.Syntax in
  let* chan_ids =
    match chan_id with
    | Some x -> Lwt.return [ x ]
    | None ->
      let* channels = Models.Channel.get_channels db in
      let* channels = Caqti_lwt.or_fail channels in
      Lwt.return @@ List.map ~f:(fun x -> x.id) channels
  in
  Lwt_list.iter_s
    (fun chan_id ->
      printf "Checking channel [%d]\n" chan_id;
      let* feed_body = get_feed ~id:chan_id db in
      let* feed_body = Caqti_lwt.or_fail feed_body in
      let raw_items = translate_chan ~chan_id feed_body in
      Lwt_list.iter_s
        (fun x ->
          load_chan db x
          >|= (function
                 | Ok () -> Ok ()
                 | Error (`Response_failed m as e) ->
                   let msg = Format.asprintf "%a\n" Caqti_error.pp_msg m.msg in
                   (* Ignore unique constraint failures. *)
                   if String.is_prefix ~prefix:"UNIQUE constraint failed" msg
                   then Ok ()
                   else Error e
                 | Error e -> Error e)
          >>= Caqti_lwt.or_fail)
        raw_items)
    chan_ids
;;
