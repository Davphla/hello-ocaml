open Effect
open Effect.Deep

type _ Effect.t += Ping : unit t 
| Pong : unit t;;

let ping () = Printf.printf "Ping\n"; perform Pong;;
let pong () = Printf.printf "Pong\n"; perform Ping;;

let run (main : unit -> unit): unit =
  let rec spawn (f : unit -> unit) =
    try_with f () {
      effc = fun (type a) (eff: a t) -> 
      match eff with 
      | Ping -> Some (fun (_: (a, unit) continuation) -> spawn ping;)
      | Pong -> Some (fun (_: (a, unit) continuation) -> spawn pong;)
      | _ -> None
    } in
  
  spawn main;;

let run_concurential = run (fun _ -> perform Ping);;
