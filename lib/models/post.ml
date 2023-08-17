open Core

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

type t =
  { id : int
  ; guid : string
  ; channel_id : int
  ; title : string
  ; desc : string
  ; categories : string list
  }
[@@deriving show]

let t_of_tuple (id, (guid, (channel_id, (title, (desc, (categories, ())))))) =
  { id
  ; guid
  ; channel_id
  ; title
  ; desc
  ; categories = String.split ~on:',' categories
  }
;;

let ( post_table
    , (Expr.[ id; guid; channel_id; title; desc; categories ] as all_fields) )
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
        ; channel_id := i channel_id_
        ; desc := s desc_
        ; categories
            := s
               @@ List.fold
                    ~f:(fun acc x -> acc ^ "," ^ x)
                    ~init:(Option.value ~default:"" (List.hd categories_))
                    (Option.value ~default:[] (List.tl categories_))
        ]
  |> Request.make_zero
  |> Petrol.exec db
;;

let get_posts db =
  let open Lwt_result.Infix in
  Query.select all_fields ~from:post_table
  |> Request.make_many
  |> Petrol.collect_list db
  >|= List.map ~f:t_of_tuple
;;

let get_post ~id:id_ db =
  let open Lwt_result.Infix in
  Query.select all_fields ~from:post_table
  |> Query.where Expr.(id = i id_)
  |> Request.make_one
  |> Petrol.find db
  >|= t_of_tuple
;;
