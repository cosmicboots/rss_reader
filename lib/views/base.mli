(** [setup_page ~title elt] will create a new page with [title] as the page
    title and [elt] as main page content *)
val setup_page
  :  title:string
  -> Dream.request
  -> [< Html_types.flow5 > `Div ] Tyxml_html.elt Lwt.t
  -> Dream.response Lwt.t
