(* Compact node Info Handlers *)

let get_id s = String.sub s 0 20;;

let get_ip s =
  let aux n = string_of_int (Char.code (s.[20+n])) in
  (String.concat "." (List.map aux [0;1;2;3]) : string)
;;

let get_port s = 
  let port = String.sub s 24 2 in
  (Char.code (port.[0]))*256 + (Char.code (port.[1]))
;;


(* Various *)

let reverseString s =
  let n = String.length s in
  let s' = String.make n 'c' in
  for i = 0 to (n-1) do
    s'.[i] <- s.[n-i-1];
  done;
  s'
;;

let seeds = Array.make 20 0;;
let rndCount = ref 100;;

let random_id () =
  if !rndCount = 100 then
    begin
      for i = 0 to 19 do
        seeds.(i) <- Random.int 256;
      done;
      rndCount := 0;
    end;
  let buf = String.make 20 '0' in
  for i = 0 to 19 do
    seeds.(i) <- (seeds.(i) + 1) mod 256;
    buf.[i] <- Char.chr seeds.(i);
  done;
  incr rndCount;
  buf
;;

let generateNeighbors s =
  let rec aux = function
    | 0 -> []
    | n -> (random_id ())::(aux (n-1))
  in
  aux 1;;
(*   let s' = String.copy s in *)
(*   let allOne = lnot 0 in *)
(*   let res = ref [] in *)
(*   for i = 19 downto 17 do *)
(*     let n = Char.code s'.[i] in *)
(*     for j = 0 to 4 do *)
(*       s'.[i] <- Char.chr ((n land (allOne lsl (2*j))) mod 256); *)
(*       let s'' = String.copy s' in *)
(*       res := s'' :: !res; *)
(*     done; *)
(*   done; *)
(*   !res *)
(* ;; *)
(*   let res = ref [] in *)
(*   for i = 19 downto 0 do *)
(*     let n = Char.code s.[i] in *)
(*     let s' = String.copy s in *)
(*     s'.[i] <- Char.chr 0; *)
(*     res := s' :: !res; *)
(*   done; *)
(*   !res *)
(* ;; *)
