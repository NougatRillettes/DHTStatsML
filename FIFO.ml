(* Limited size fifo *)

type 'a fifo = (int ref * 'a list ref * int);;

exception EmptyFifo;;

let make m =
  if m = 0 then failwith "Cannot create 0 sized fifo";
  (ref 0, ref [], m)
;;

let pop (n,l,_) =
  match (!n,!l) with
    | (0,[]) -> raise EmptyFifo;
    | (_,x::xs) -> decr n; l := xs; x
    | _ -> failwith "Fifo STructure violated !"
;;

let empty (n,_,_) = 
  !n == 0
;;

let size (_,_,m) = m;;

let push x (n,l,max) =
  if !n == max then
    begin
      (* l := List.rev (List.tl (List.rev !l)); *)
      l := [];
      decr n;
    end;
  incr n;
  l := (x::!l)
;;
