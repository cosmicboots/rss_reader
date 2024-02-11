open Snippets.Utils

let api_handler snip req =
  let open Lwt.Syntax in
  let* res = snip req in
  Dream.html @@ elt_to_string res
;;

let unit_api_handler snip req =
  let open Lwt.Syntax in
  let* () = snip req in
  Dream.empty `OK
;;

let api_routes : Dream.route list =
  [ (Dream.get "/posts"
     @@ fun req ->
     let open Lwt.Syntax in
     Dream.log "Running etl";
     let* () = Dream.sql req Etl.run in
     Dream.log "Ran etl";
     let* res = Snippets.Posts.list req in
     Dream.html @@ elt_to_string res)
  ; Dream.get "/posts/:post_id" @@ api_handler Snippets.Posts.get
  ; Dream.get "/feeds/:feed_id" @@ api_handler Snippets.Feed.get
  ; Dream.get "/feeds/:feed_id/edit" @@ api_handler Snippets.Feed.get_edit
  ; Dream.get "/toggle-darkmode" @@ api_handler Snippets.Darkmode.get
  ; Dream.put "/feeds/:feed_id" @@ api_handler Snippets.Feed.put
  ; Dream.post "/feeds" @@ api_handler Snippets.Feed.post
  ; Dream.delete "/feeds/:feed_id" @@ unit_api_handler Snippets.Feed.delete
  ]
;;

let routes : Dream.route list =
  [ Dream.get "/" Views.Index.get
  ; Dream.get "/settings" Views.Settings.get
  ; Dream.get "/static/**" @@ Dream.static "static/"
  ; Dream.scope "/api" [ Dream.origin_referrer_check ] api_routes
  ; Dream_livereload.route ()
  ]
;;
