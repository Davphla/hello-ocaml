module T = Domainslib.Task

    
module C
 = struct 

  type mutex_condvar = {
    mutex: Mutex.t;
    condition: Condition.t
  };;

  let mutex_condvar_key =
    Domain.DLS.new_key (fun () ->
      let m = Mutex.create () in
      let c = Condition.create () in
      {mutex=m; condition=c});;

  
  type 'a chan = 'a Atomic.t;;

  let make () = Atomic.make None;;
  

  let set_chan_atom obj old_c new_c recur_f = 
    if not @@ Atomic.compare_and_set obj old_c new_c then 
      Domain.cpu_relax ();
      recur_f ();;

  let block_until_available old_c = 
    let mc = Domain.DLS.get mutex_condvar_key in
    let msg_slot = ref old_c in
    begin
      Mutex.lock mc.mutex;
      while !msg_slot = old_c do
        Condition.wait mc.condition mc.mutex;
      done;
      Mutex.unlock mc.mutex;
      !msg_slot
    end;;
      

  let rec send obj new_c = 
    let old_c = Atomic.get obj in 
    let send_val () = 
      let recur = (fun _ -> send obj new_c) in 
      set_chan_atom obj old_c new_c recur;
    in

    match old_c with
    | None -> 
      send_val ()
    | Some _ -> 
      ignore @@ block_until_available old_c;
      send_val ();;


  let rec recv obj =
    let cont = Atomic.get obj in 
    let clear_val () = 
      let recur = (fun _ -> recv obj) in 
      set_chan_atom obj cont None recur;
    in
    
    match cont with
    | None -> 
      let new_c = block_until_available None in 
      clear_val ();
      new_c
    | cont -> 
      clear_val ();
      cont

end

open Printf;;

let total_count = 10000

let increment count = count + 1;;

let process schan rchan = 
  for _ = 1 to total_count do
    let c = increment @@ Option.get @@ C.recv rchan in
      printf "%d\n" c;
      C.send schan (Some c)
  done;;
  

let counter_start = let c1 = C.make () in 
  let c2 = C.make () in
  let _ = Domain.spawn (fun _ -> process c1 c2) in 
  let p2 = Domain.spawn (fun _ -> process c2 c1) in 
  C.send c1 (Some 0);
  Domain.join p2

let run_parallel = counter_start
  
let () = run_parallel
