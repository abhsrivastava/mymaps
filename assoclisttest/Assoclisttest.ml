open OUnit2
open AssocList

let empty_test = 
  "empty has no bindings" >:: (fun _ -> assert_equal [] (bindings empty))

let from_list_test = 
  let lst = [(3110, "fun")] in
  "create a map using from_list" >:: (fun _ -> assert_equal lst (bindings (from_list lst)))

let assoclist_tests = [empty_test; from_list_test]

let assoclist_testsuite = "assoc list test suite" >::: assoclist_tests
let _ = run_test_tt_main assoclist_testsuite