module T = Domainslib.Task

let rec tak x y z p =
  if x > y then
    let x' = T.async p (fun _ -> (tak (x-1) y z p)) in
    let y' = T.async p (fun _ -> (tak (y-1) z x p)) in
    let z' = T.async p (fun _ -> (tak (z-1) x y p)) in
    tak (T.await p x') (T.await p y') (T.await p z') p
  else z


let tak_start pool = tak 2 36 24 pool


let run_parallel = let pool = T.setup_pool ~num_domains:8 () in
  let _ = T.run pool (fun _ -> counter_start) in
  T.teardown_pool pool;;