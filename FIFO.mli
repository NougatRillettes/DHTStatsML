(* Limited size fifo *)

type 'a fifo = (int ref * 'a list ref * int);;

val make : int -> 'a fifo;;

val pop : 'a fifo -> 'a;;

val empty : 'a fifo -> bool;;

val size : 'a fifo -> int;;

val push : 'a -> 'a fifo -> unit ;;
