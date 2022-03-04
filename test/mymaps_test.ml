open OUnit2
open Mymaps

let hello_test = 
  "say hello" >:: (fun _ -> assert_equal "Hello World" (sayHello))

let empty_test = 
  let (result : (int * int) list) = empty in 
  "empty has no bindings" >:: (fun _ -> assert_equal [] (result))

let mymap_tests = [hello_test]

let suite = "maps suite" >::: mymap_tests
let _ = run_test_tt_main suite