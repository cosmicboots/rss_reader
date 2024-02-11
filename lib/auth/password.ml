let hash_password password =
  let open Argon2 in
  let hash_len = 32 in
  let t_cost = 2 in
  let m_cost = 65536 in
  let parallelism = 1 in
  let salt = Dream.random 16 in
  let salt_len = String.length salt in
  let encoded_len =
    encoded_len ~salt_len ~hash_len ~t_cost ~m_cost ~parallelism ~kind:ID
  in
  hash
    ~t_cost
    ~m_cost
    ~parallelism
    ~pwd:password
    ~salt
    ~kind:ID
    ~hash_len
    ~encoded_len
    ~version:VERSION_NUMBER
;;

let verify_password ~hash:hash_ password =
  let open Argon2 in
  verify ~encoded:hash_ ~pwd:password ~kind:ID
;;
