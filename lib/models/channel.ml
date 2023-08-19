open Core

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

type t =
  { id : int
  ; name : string
  ; desc : string
  ; uri : string
  }
[@@deriving show]

let channel_table, (Expr.[ id; name; uri; desc ] as all_fields) =
  StaticSchema.declare_table
    schema
    ~name:"channel"
    Schema.
      [ field
          ~constraints:[ primary_key ~name:"id" ~auto_increment:true () ]
          "id"
          ~ty:Type.int
      ; field ~constraints:[ not_null () ] "name" ~ty:Type.text
      ; field ~constraints:[ not_null () ] "uri" ~ty:Type.text
      ; field ~constraints:[ not_null () ] "desc" ~ty:Type.text
      ]
;;

let insert_channel ~name:name_ ~uri:uri_ ~desc:desc_ db =
  Query.insert
    ~table:channel_table
    ~values:Expr.[ name := s name_; uri := s uri_; desc := s desc_ ]
  |> Request.make_zero
  |> Petrol.exec db
;;

let get_channels ?name:name_ db =
  let open Lwt_result.Infix in
  Query.select Expr.[ id; name; uri; desc ] ~from:channel_table
  |> (match name_ with
    | Some x -> Query.where Expr.(name = s x)
    | None -> fun x -> x)
  |> Request.make_many
  |> Petrol.collect_list db
  >|= List.map ~f:(fun (id, (name, (uri, (desc, ())))) ->
    { id; name; uri; desc })
;;

let update_channel ~id:id_ ?name:name_ ?desc:desc_ ?uri:uri_ db =
  let update_expr = ref [] in
  List.iter
    ~f:(fun y ->
      Option.iter ~f:(fun x ->
        update_expr := Expr.((fst y := s x) :: !update_expr))
      @@ snd y)
    [ name, name_; desc, desc_; uri, uri_ ];
  Query.update ~table:channel_table ~set:!update_expr
  |> Query.where Expr.(id = i id_)
  |> Request.make_zero
  |> Petrol.exec db
;;

let get_channel ~id:id_ db =
  let open Lwt_result.Infix in
  Query.select all_fields ~from:channel_table
  |> Query.where Expr.(id = i id_)
  |> Request.make_one
  |> Petrol.find db
  >|= fun (id, (name, (uri, (desc, ())))) -> { id; name; uri; desc }
;;

let delete_channel ~id:id_ db =
  Query.delete ~from:channel_table
  |> Query.where Expr.(id = i id_)
  |> Request.make_zero
  |> Petrol.exec db
;;
