open Unix
open Bencoding
open Requests



type interm = No | Interm of int;;

type trans_id_info =
  {
    time : float; 
    find_node : bool;
    interm : interm;
}

type trans_id_info_case = None | Some of trans_id_info

let currentTrans = Array.init 65537 (fun n -> None);;
let index = ref 0;;

let rec choose_trans_num () = 
if (!index==65536) then index := 1 else index := !index+1;
match (currentTrans.(!index)) with
| None -> !index
| Some info ->  begin 
  if (Unix.time() > (info.time +. 3.)) 
  then begin currentTrans.(!index) <- None; !index end
  else choose_trans_num ()
end


let addrBootstrap = ADDR_INET(inet_addr_of_string "82.221.103.244", 6881);;


let int_to_trans_num i = 
(*d'un entier vers un string de transaction number*)
  let res = "aa" in 
  res.[0] <- (Char.chr ((i-(i mod 256))/256));
  res.[1] <- (Char.chr (i mod 256));
  res
;; 


let generateTargetNode () = "abcdefghij0123456789" ;;(*trouver un noeud à chercher, en maintenant une liste de noeuds déjà trouvés?*)


let targetNode = ref "abcdefghij0123456789";;
let trans_num = ref 1 ;;
let myID = "jihgfedbca9876543210";;


(* let rec trouve_noeud requestsToSend = *)
(* (\*on démarre cette fonction avec requestsToSend ayant un seul élément, un QFindNode vers un certain noeud en cherchant un certain targetNode. *)
(* Cette fonction s'arrete lorsque toutes les requetes ont été effectuées.*\)  *)
(*     match requestsToSend with *)
(*     |[] -> () *)
(*     |(QPing x, serv_addr)::requestsToSend' -> begin ignore(envoie_requetePing (QPing x) serv_addr); trouve_noeud requestsToSend' end *)
(*     |(QFindNode x, serv_addr)::requestsToSend' ->  *)
(*       begin *)
(* 	let answer = envoie_requeteFind_nodes (QFindNode x) serv_addr in *)
(* 	if ((\*on nous renvoie l'addresse ip du targetNode*\) false)  *)
(* 	then  (\*alors on le ping*\) *)
(* 	  begin  *)
(* 	    incr trans_num;  *)
(* 	    trouve_noeud ((QPing{qp_t = (int_to_trans_num trans_num); qp_id= !targetNode} , (\*normalement nv_serv_addr*\) serv_addr)::(requestsToSend')) *)
(* 	  end *)
(* 	else if ((\*on nous renvoie l'addresse ip d'un noeud plus proche*\) false) *)
(* 	then (\*on lui demande où est targetNode*\) *)
(* 	  begin *)
(* 	    incr trans_num;  *)
(* 	    trouve_noeud ((QFindNode {qfn_t = (int_to_trans_num trans_num) ; qfn_id=myID; qfn_target = !targetNode},  (\*normalement nv_serv_addr*\) serv_addr)::requestsToSend') *)
(* 	  end *)
(* 	else (\*on nous renvoie les ids d'autres noeuds plus proches*\) *)
(* 	  begin *)
(* 	    let nvellesreqs = List.map  *)
(* 	      (fun x ->  incr trans_num ; (QFindNode {qfn_t = (int_to_trans_num trans_num) ; qfn_id=myID; qfn_target =x}, serv_addr)) answer.afn_nodes in *)
(* 	     trouve_noeud (nvellesreqs@requestsToSend') (\*est-ce qu'il vaut mieux faire d'abord les novuelles requetes ou celles d'avant?*\) *)
(* 	  end *)
(*       end *)
(* ;; *)

(* let receiver sck queue stats () = *)
(*   let n = ref 0 in (\*number of packet handled, for debugging purposes*\) *)
(*   let sizeBuffer = 1500 in *)
(*   ignore @@ select [sck] [] [] (-1.0); *)
(*   let buffer_reponse = String.create 1500 in *)
(*   if (recvfrom sck buffer_reponse 0 sizeBuffer [] > sizeBuffer) then *)
(*     Printf.fprintf stderr "[%d] Buffer size was too small in receiver!\n"; *)
  
(* ;; *)
  



let random_id () =
  let buf = String.make 20 '0' in
  for i = 0 to 19 do
    buf.[i] <- Char.chr (Random.int 256);
  done;
  buf
;;

let reverseString s =
  let n = String.length s in
  let s' = String.make n 'c' in
  for i = 0 to (n-1) do
    s'.[i] <- s.[n-i-1];
  done;
  s'
;;


let get_id s = String.sub s 0 20;;
let get_ip s =
  let aux n = string_of_int (Char.code (s.[20+n])) in
  (String.concat "." (List.map aux [0;1;2;3]) : string)
;;

let get_port s = 
  let port = String.sub s 24 2 in
  (Char.code (port.[0]))*256 + (Char.code (port.[1]))
;;

type node_info = 
  {
    unanswered_requests : int ref;
}

module NodeInfoMap = Map.Make(String) ;;
let ens = ref NodeInfoMap.empty;;

let addReqFifo target id addr fifo =
   let query =
      bencodeQFindNode {
        qfn_id = "12345678901234567890";
        qfn_t = "00";
        qfn_target = target;
        qfn_want = 1;
      }
   in
   FIFO.push (query,addr,id) fifo;
;;

let handleReadySocket sck fifo =
  let readBuf = String.make 1500 '0' in
  ignore (recvfrom sck readBuf 0 1500 []);
  try
    let answ = bencoded_to_Find_NodesAnswer (parser readBuf) in
     begin
       try
         
        (* Printf.printf "Receiving from %S\n%!" answ.afn_id;  *)
         (NodeInfoMap.find answ.afn_id !ens).unanswered_requests := 0
           
      with
        | Not_found ->  Printf.printf "Erreur interne\n";
     end;
    let genAddr s =
      let ip = get_ip s in
      let port = get_port s in
      (*Printf.printf "Adding to FIFO : %S at %s:%d\n%!" (get_id s) ip port;*)
  ADDR_INET(inet_addr_of_string ip, port)
    in
    List.iter (fun s -> addReqFifo (random_id ()) (get_id s) (genAddr s) fifo) answ.afn_nodes;
    (*Printf.printf "Fifo size : %d\n%!" (FIFO.size fifo);*)

  with
    | (Bad_Answer _) -> ()
    |  Not_found ->  Printf.printf "Erreur interne\n";
;;

let rec envoie socket bencoded serv_addr = 
  try 
    sendto socket bencoded 0 (String.length bencoded) [] serv_addr
  with
  |Unix_error(ENOBUFS, _, _) -> begin
    Printf.printf "manque de buffer! on réessaie\n";
    sleep(1);
    envoie socket bencoded serv_addr
  end

let rec receive_requests requetes socket= 
  let (f1, f2, f3) = select [socket] [] [] 0.1 in
  begin
    match f1 with
      |[] ->
          let n = (NodeInfoMap.cardinal !ens) in
          Printf.printf "Nous avons un set qui a une taille de %i\n%!" n;
          if true
          then (send_requests requetes socket)
          else ()
      |[socket] -> begin handleReadySocket socket requetes; receive_requests requetes socket end
      |_ -> begin Printf.printf "erreur interne!\n"; send_requests requetes socket end
  end
    
and send_requests requetes socket= 
  let compteur = ref 0 in
  if (FIFO.empty requetes) then 
    for i=0 to 50 do
      addReqFifo (random_id ()) "\235\2556isQ\255J\236)\205\186\171\242\251\227F|\194g"  addrBootstrap requetes
    done;
  while (not(FIFO.empty requetes) && !compteur < 100) do
    try
      let (bencoded, serv_addr, id_node) = FIFO.pop requetes in
      (*Printf.printf "Sending to %S\n%!" id_node;*)
      try 
        if (not(!((NodeInfoMap.find id_node !ens).unanswered_requests) > 2000)) 
        then begin
	  envoie socket bencoded serv_addr;
	  incr (NodeInfoMap.find id_node !ens).unanswered_requests;
        end
      with Not_found -> 
        begin
	  envoie socket bencoded serv_addr;
	  let i = ref 1 in
	  ens := NodeInfoMap.add id_node {unanswered_requests = i} !ens;
        end;
        incr compteur
    with | (Unix_error (EINVAL,_,_)) -> ();
  done;
  receive_requests requetes socket
;;
    

let main =
  let requetes = FIFO.make 100000 in
  let s = socket PF_INET SOCK_DGRAM 0 in
  send_requests requetes s
;;
