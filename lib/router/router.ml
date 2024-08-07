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
  [ Dream.get "/posts/:post_id" @@ api_handler Snippets.Posts.get
  ; Dream.get "/feeds/:feed_id" @@ api_handler Snippets.Feed.get
  ; Dream.get "/feeds/:feed_id/edit" @@ api_handler Snippets.Feed.get_edit
  ; Dream.put "/feeds/:feed_id" @@ api_handler Snippets.Feed.put
  ; Dream.get "/channel/:chan_id" @@ api_handler Snippets.Posts.list
  ; Dream.post "/feeds" @@ api_handler Snippets.Feed.post
  ; Dream.delete "/feeds/:feed_id" @@ unit_api_handler Snippets.Feed.delete
  ]
;;

let require_login inner_handler req =
  match Dream.session_field req "user" with
  | Some _ -> inner_handler req
  | None -> Dream.redirect req "/login"
;;

let routes : Dream.route list =
  [ Dream.get "/login" Views.Auth.login_get
  ; Dream.post "/login" Views.Auth.login_post
  ; Dream.get "/logout" Views.Auth.logout
  ; Dream.get "/api/toggle-darkmode" @@ api_handler Snippets.Darkmode.get
  ; Dream.scope
      "/"
      [ require_login ]
      [ Dream.get "/" Views.Index.get
      ; Dream.get "/settings" Views.Settings.get
      ; Dream.get "/static/**" @@ Dream.static "static/"
      ; Dream.scope "/api" [ Dream.origin_referrer_check ] api_routes
      ; Dream_livereload.route ()
      ]
  ]
;;
