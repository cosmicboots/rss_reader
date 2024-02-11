let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

type t =
  { id : int
  ; username : string
  ; password : string
  }
[@@deriving show]

let t_of_tuple (id, (username, (password, ()))) = { id; username; password }

let user_table, (Expr.[ id; username; password ] as all_fields) =
  StaticSchema.declare_table
    schema
    ~name:"users"
    Schema.
      [ field
          ~constraints:[ primary_key ~auto_increment:true () ]
          "id"
          ~ty:Type.int
      ; field ~constraints:[ unique (); not_null () ] "username" ~ty:Type.text
      ; field ~constraints:[ not_null () ] "password" ~ty:Type.text
      ]
;;

let insert_user ~username:username_ ~password:password_ db =
  Query.insert
    ~table:user_table
    ~values:Expr.[ username := s username_; password := s password_ ]
  |> Request.make_zero
  |> Petrol.exec db
;;

let get_user ~username:username_ db =
  let open Lwt_result.Infix in
  Query.select all_fields ~from:user_table
  |> Query.where Expr.(username = s username_)
  |> Request.make_one
  |> Petrol.find db
  >|= t_of_tuple
;;
