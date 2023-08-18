open Views.Utils

let api_handler snip req =
  let open Lwt.Syntax in
  let* res = snip req in
  Dream.html @@ elt_to_string res
;;

let api_routes : Dream.route list =
  [ Dream.get "/posts" @@ api_handler @@ Snippets.Posts.list
  ; Dream.get "/posts/:post_id" @@ api_handler @@ Snippets.Posts.get
  ; Dream.get "/feeds/:feed_id" @@ api_handler @@ Snippets.Feed.get
  ; Dream.get "/feeds/:feed_id/edit" @@ api_handler @@ Snippets.Feed.get_edit
  ; Dream.put "/feeds/:feed_id" @@ api_handler @@ Snippets.Feed.put
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
