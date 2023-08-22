type value_type =
  | BooleanT
  | NumberT
  | StringT
  | List of value_type
  | String of string
  | Const of string
  | Union of value_type list
  | Function of value_type * value_type
  | Empty
[@@deriving show { with_path = false }]
