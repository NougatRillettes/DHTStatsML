(* Limited size fifo *)

type 'a fifo = (int ref * 'a list ref * int * int ref);;

exception EmptyFifo;;

let make m =
  if m = 0 then failwith "Cannot create 0 sized fifo";
  (ref 0, ref [], m,ref 0)
;;

let pop (n,l,_,_) =
  match (!n,!l) with
    | (0,[]) -> raise EmptyFifo;
    | (_,x::xs) -> decr n; l := xs; x
    | _ -> failwith "Fifo Structure violated !"
;;

let empty (n,_,_,_) = 
  !n == 0
;;

let size (n,_,_,_) = !n;;

let push x (n,l,max,seen) =
  if !n == max then
    begin
      (*
      l := List.rev (List.tl (List.rev !l)); 
      decr n;
      *)
      l := [];
      n := 0;
    
    end;
  incr n;
  incr seen;
  l := (x::!l)
;;

let seen (_,_,_,r) = !r;;
