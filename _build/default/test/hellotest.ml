(* Test slice function *)

open Pong


let test_slice () = 
  Alcotest.(check (list string)) 
    "Simple list" ["c"; "d"; "e"; "f"; "g"] (Main.slice 2 6 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"])

  let () =
  let open Alcotest in
  run "Utils" [
      "list-concat", [ test_case "List slicing" `Quick  test_slice ];
    ]
