let rec nth i = function 
  | [] -> None
  | a :: b -> if i = 0 then Some a else nth (i - 1) b;;

let rec last = function 
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

let rec lenght = function
  | [] -> 0 
  | (_ :: b) -> 1 + lenght b;;

let rec slice b e = function
  | [] -> []
  | (x :: xs) -> 
    let drop = slice (b - 1) (e - 1) xs in
    let take = x :: slice 0 (e - 1) xs in
    if b > 0 then drop else
      (if e >= 0 then take else []) ;;


