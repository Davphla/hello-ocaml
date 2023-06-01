open Effect
open Effect.Deep

type _ Effect.t += Ping : int -> unit t
| Fork : (unit -> unit) -> unit t;;

let fork f = perform (Fork f);;

let exec_pong i = Printf.printf "%d\n" i;;


let run (main : unit -> unit): unit =
  let proc_queue = Queue.create () in

  let cur_ping = ref 0 in

  let enqueue k v = 
    let task () = continue k v in
    Queue.add task proc_queue in

  let dequeue () =
    Queue.take proc_queue () in
    
  let rec spawn (f : unit -> unit) =
    try_with f () {
      effc = fun (type a) (eff: a t) -> 
      match eff with 
      | Ping i -> Some (fun (k: (a, _) continuation) -> 
        let send_msg () = exec_pong !cur_ping; cur_ping := i; in 
        send_msg (); dequeue (); (enqueue k ()); )
      | Fork f -> Some (fun (k: (a, _) continuation) -> (enqueue k ()); spawn f;)
      | _ -> None
    } in
  
  spawn main;;

let pong i = perform (Ping i);;

let run_concurential = run (fun _ -> 
  fork (fun _ -> pong 1); 
  fork (fun _ -> pong 0);
  fork (fun _ -> pong 1); 
  fork (fun _ -> pong 0);
);;
