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

let max_test _ = 
  let t = (insert 10 "f" (insert 20 "b1" (insert 50 "b2" empty))) in
  match max t with 
  | None -> assert_failure "max value not returned"
  | Some v -> assert_equal 50 (fst v)

let remove_test_1 _ = 
  let t = (remove 10 empty) in 
  assert_equal empty t 

let remove_test_2 _ = 
  let t = (remove 10 (insert 10 "foo" empty)) in 
  assert_equal empty t

let remove_test_3 _ = 
  let t = (remove 1 (insert 5 "c" (insert 1 "b" (insert 10 "a" empty)))) in 
  let result1 = find 1 t in 
  let result2 = find 5 t in 
  let result3 = find 10 t in 
  assert_equal result1 None; assert_equal result2 (Some "c"); assert_equal result3 (Some "a")

  let remove_test_4 _ = 
    let t = (remove 2 (insert 1 "d" (insert 5 "c" (insert 15 "e" (insert 2 "b" (insert 10 "a" empty)))))) in 
    let result1 = find 1 t in 
    let result2 = find 2 t in 
    let result3 = find 5 t in 
    let result4 = find 10 t in 
    let result5 = find 15 t in 
    assert_equal result1 (Some "d"); assert_equal result2 None; assert_equal result3 (Some "c"); assert_equal result4 (Some "a"); assert_equal result5 (Some "e")
  

let redblacktree_tests = [
  "empty set has size 0" >:: size_test;
  "add 1 item to empty map and size should be 1" >:: size_1_test;
  "add 2 items to empty map and size should be 2" >:: size_2_test;
  "find should return None on an empty map" >:: find_test_1;
  "find should return value when used on an inserted value" >:: find_test_2;
  "bindings should return an array of inserted values" >:: bindings_test;
  "get the maximum key value in a tree" >:: max_test;
  "remove element from empty returns an empty map" >:: remove_test_1;
  "remove root node from map returns an empty map" >:: remove_test_2;
  "remove node with 1 child" >:: remove_test_3;
  "remove node with 2 children" >:: remove_test_4;
]

let redblacktree_suite = "Tests for HashMap based on RedBlackTree" >::: redblacktree_tests
let _ = run_test_tt_main redblacktree_suite