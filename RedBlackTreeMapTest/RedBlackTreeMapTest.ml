open OUnit2
open RedBlackTreeMap

let size_test _ =
  let map = empty in 
  assert_equal 0 (size map)

let size_1_test _ = 
  let map = (insert 1 "test" empty) in 
  assert_equal 1 (size map)

let size_2_test _ =
  let map = (insert 2 "test" (insert 1 "test" empty)) in 
  assert_equal 2 (size map)

let find_test_1 _ = 
  let map = empty in 
  let result = find 1 map in 
  assert_equal None result

let find_test_2 _ = 
  let map = (insert 1 "foo" empty) in 
  let result = find 1 map in 
  assert_equal (Some "foo") result

let bindings_test _ = 
  let map = (insert 1 "foo" empty) in 
  let result = bindings map in 
  assert_equal [(1, "foo")] result

let redblacktree_tests = [
  "empty set has size 0" >:: size_test;
  "add 1 item to empty map and size should be 1" >:: size_1_test;
  "add 2 items to empty map and size should be 2" >:: size_2_test;
  "find should return None on an empty map" >:: find_test_1;
  "find should return value when used on an inserted value" >:: find_test_2;
  "bindings should return an array of inserted values" >:: bindings_test;
]

let redblacktree_suite = "Tests for HashMap based on RedBlackTree" >::: redblacktree_tests
let _ = run_test_tt_main redblacktree_suite