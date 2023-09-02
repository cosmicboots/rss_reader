open Core

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

type t =
  { id : int
  ; guid : string
  ; date : Ptime.t
  ; channel_id : int
  ; title : string
  ; desc : string
  ; categories : string list
  }
[@@deriving show]

let t_of_tuple
  (id, (guid, (date, (channel_id, (title, (desc, (categories, ())))))))
  =
  { id
  ; guid
  ; date = Option.value_exn @@ Ptime.of_float_s date
  ; channel_id
  ; title
  ; desc
  ; categories =
      (if String.(categories <> "")
       then String.split ~on:',' categories
       else [])
  }
;;

let ( post_table
    , (Expr.[ id; guid; date; channel_id; title; desc; categories ] as
       all_fields) )
  =
  StaticSchema.declare_table
    schema
    ~name:"post"
    Schema.
      [ field
          ~constraints:[ primary_key ~auto_increment:true () ]
          "id"
          ~ty:Type.int
      ; field ~constraints:[ unique (); not_null () ] "guid" ~ty:Type.text
      ; field ~constraints:[ not_null () ] "date" ~ty:Type.real
      ; field
          ~constraints:
            [ foreign_key
                ~table:Channel.channel_table
                ~columns:[ Channel.id ]
                ()
            ; not_null ()
            ]
          "idChannel"
          ~ty:Type.int
      ; field ~constraints:[ not_null () ] "title" ~ty:Type.text
      ; field ~constraints:[ not_null () ] "desc" ~ty:Type.text
      ; field "categories" ~ty:Type.text
      ]
;;

let insert_post
  ~title:title_
  ~guid:guid_
  ~date:date_
  ~channel_id:channel_id_
  ~desc:desc_
  ~categories:categories_
  db
  =
  Query.insert
    ~table:post_table
    ~values:
      Expr.
        [ title := s title_
        ; guid := s guid_
        ; date := f @@ Ptime.to_float_s date_
        ; channel_id := i channel_id_
        ; desc := s desc_
        ; categories := s @@ String.concat ~sep:"," categories_
        ]
  |> Request.make_zero
  |> Petrol.exec db
;;

let get_posts db =
  let open Lwt_result.Infix in
  let chan_name, chan_name_ref =
    Expr.(as_ Channel.name ~name:"channel_name")
  in
  let chan_id, chan_id_ref = Expr.(as_ Channel.id ~name:"channel_id") in
  Query.select Expr.(chan_name_ref :: all_fields) ~from:post_table
  |> Query.join
       ~op:Query.LEFT
       ~on:Expr.(channel_id = chan_id_ref)
       (Query.select [ chan_id; chan_name ] ~from:Channel.channel_table)
  |> Request.make_many
  |> Petrol.collect_list db
  >|= List.map ~f:(fun (chan_name, x) -> chan_name, t_of_tuple x)
;;

let get_post ~id:id_ db =
  let open Lwt_result.Infix in
  Query.select all_fields ~from:post_table
  |> Query.where Expr.(id = i id_)
  |> Request.make_one
  |> Petrol.find db
  >|= t_of_tuple
;;
