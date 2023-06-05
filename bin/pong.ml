module T = Domainslib.Task

    
module C
 = struct 


  let make () = Atomic.make None;;

  (* let block_until_available old_c = 
    let mc = Domain.DLS.get mutex_condvar_key in
    let msg_slot = ref old_c in
    begin
      Mutex.lock mc.mutex;
      while !msg_slot = old_c do
        Condition.wait mc.condition mc.mutex;
      done;
      Mutex.unlock mc.mutex;
      !msg_slot
    end;; *)
      
  let rec set_chan_atom obj new_c = 
    let old_c = Atomic.get obj in 
    if Atomic.compare_and_set obj old_c new_c then () else
      (Domain.cpu_relax ();
      set_chan_atom obj new_c)

  let rec block_until_available obj old_c =
    let cont = Atomic.get obj in 
    if cont = old_c then 
      (Domain.cpu_relax ();
      block_until_available obj old_c;)
    else
      cont  


  let send obj new_c = 
    let old_c = Atomic.get obj in 
    let send_val () = 
      set_chan_atom obj new_c;
    in

    match old_c with
    | None -> 
      send_val ()
    | Some _ -> 
      ignore @@ block_until_available obj old_c;
      send_val ();;


  
  let recv obj =
    let cont = Atomic.get obj in 
    let clear_chan obj = 
      set_chan_atom obj None
    in
    
    match cont with
    | None -> 
      let new_c = block_until_available obj None in 
      clear_chan obj;
      new_c
    | cont -> 
      clear_chan obj;
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
  let _p1 = Domain.spawn (fun _ -> process c1 c2) in 
  let p2 = Domain.spawn (fun _ -> process c2 c1) in 
  C.send c1 (Some 0);
  Domain.join p2

let run_parallel = counter_start
  
let () = run_parallel
