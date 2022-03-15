open OUnit2
open DirectAddressMap

let empty_test = 
  "empty has no bindings" >:: (fun _ -> assert_equal [] (create 0 |> bindings))

let fromlist_test = 
  let lst = [(0, "foo")] in 
  "create a map from list" >:: (fun _ -> assert_equal lst (from_list 1 lst |> bindings))

let directaddressmap_tests = [empty_test; fromlist_test]
let directaddressmap_suite = "direct address map test suite" >::: directaddressmap_tests
let _ = run_test_tt_main directaddressmap_suite