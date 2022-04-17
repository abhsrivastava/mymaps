open OUnit2
open HashTable

let empty_test = 
  "empty has no bindings" >:: (fun _ -> assert_equal [] (create Hashtbl.hash 0 |> bindings))

let insert_test = 
  let lst = [("foo", "bar")] in
  let map = create Hashtbl.hash 1 in 
  "insert an binding into the map" >:: (fun _ -> assert_equal lst (insert "foo" "bar" map; bindings map))

let insert_with_resize_test = 
  let lst = [("baz", "bazz"); ("foo", "bar")] in
  let map = create Hashtbl.hash 1 in 
  "insert a binding into a map with resize" >:: (fun _ -> assert_equal lst (insert "foo" "bar" map; insert "baz" "bazz" map; bindings map))

let find_test = 
  let map = create Hashtbl.hash 1 in
  "find an element from the map" >:: (fun _ -> assert_equal (Some "bar") (insert "foo" "bar" map; find "foo" map))

let remove_test = 
  let map = create Hashtbl.hash 1 in 
  "remove an element from the map" >:: (fun _ -> assert_equal None (insert "foo" "bar" map; remove "foo" map; find "foo" map))

let hashtable_tests = [empty_test; insert_test; find_test; remove_test; insert_with_resize_test]
let hashtable_suite = "hash table map test suite" >::: hashtable_tests
let _ = run_test_tt_main hashtable_suite