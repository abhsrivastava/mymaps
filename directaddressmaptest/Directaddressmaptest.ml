open OUnit2
open DirectAddressMap

let empty_test = 
  "empty has no bindings" >:: (fun _ -> assert_equal [] (create 0 |> bindings))

let fromlist_test = 
  let lst = [(0, "foo")] in 
  "create a map from list" >:: (fun _ -> assert_equal lst (from_list 1 lst |> bindings))

let find_test = 
  let lst = [(0, "foo")] in
  "find an element in a map" >:: (fun _ -> assert_equal (Some "foo") (from_list 1 lst |> find 0))

let remove_test = 
  let lst = [(0, "foo")] in
  let map = from_list 1 lst in
  "remove an element from the map" >:: (fun _ -> assert_equal [] (remove 0 map; bindings map))

let insert_test = 
  let lst = [(0, "foo")] in
  let map = create 1 in 
  "insert an binding into the map" >:: (fun _ -> assert_equal lst (insert 0 "foo" map; bindings map))

let directaddressmap_tests = [empty_test; fromlist_test; find_test; remove_test; insert_test]

let directaddressmap_suite = "direct address map test suite" >::: directaddressmap_tests
let _ = run_test_tt_main directaddressmap_suite