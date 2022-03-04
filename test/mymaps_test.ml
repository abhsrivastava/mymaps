open OUnit2
open mymaps
let empty_test = "empty has no bindings" >:: (fun _ -> assert_equal [] (empty))
let mymap_tests = [empty_test]
let suite = "maps suite" >::: mymap_tests
let _ = run_test_tt_main suite