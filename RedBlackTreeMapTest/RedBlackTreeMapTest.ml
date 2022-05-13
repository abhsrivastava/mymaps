open OUnit2
open RedBlackTreeMap

let redblacktree_tests = []
let redblacktree_suite = "Tests for HashMap based on RedBlackTree" >::: redblacktree_tests
let _ = run_test_tt_main redblacktree_suite