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
  


module IDSet = Set.Make(String) ;;

let random_id () =
  let fd = open_in "/dev/urandom" in
  let buf = String.make 20 '0' in
  input fd buf 0 20;
  close_in fd;
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

let testlol id want =
  let s = socket PF_INET SOCK_DGRAM 0 in
  let bencodedFind_nodesQuery= bencodeQuery (QFindNode {qfn_t = "aa"; qfn_id="abcdefghij0123456789"; qfn_target = id; qfn_want = want} ) in
  ignore (sendto s bencodedFind_nodesQuery 0 (String.length bencodedFind_nodesQuery) [] addrBootstrap); 
  let buffer_reponse = String.create 1500 in
  ignore ( recvfrom s buffer_reponse 0 1500 []);
  close s;
  parser buffer_reponse;
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

let addReqFifo id addr fifo =
   let query =
      bencodeQFindNode {
        qfn_id = "12345678901234567890";
        qfn_t = "00";
        qfn_target = id;
        qfn_want = 1;
      }
   in
   FIFO.push (query,addr) fifo;
;;

let handleReadySocket sck fifo =
  let readBuf = String.make 1500 '0' in
  ignore (recvfrom sck readBuf 0 1500 []);
  try 
    let answ = bencoded_to_Find_NodesAnswer (parser readBuf) in
    let genAddr s =
      let ip = get_ip s in
      let port = get_port s in
      ADDR_INET(inet_addr_of_string ip, port)
    in
    List.iter (fun s -> addReqFifo (get_id s) (genAddr s) fifo) answ.afn_nodes
  with
    | (Bad_Answer _) -> ();
;;


let rec receive_requests requetes socket= 
  let (f1, f2, f3) = select [socket] [] [] 0.1 in
  begin
    match f1 with
    |[] -> send_requests requetes socket
    |[socket] -> begin handleReadySocket socket requetes; receive_requests requetes socket end
    |_ -> begin Printf.printf "erreur interne!\n"; send_requests requetes socket end
  end
    
and send_requests requetes socket= 
  let compteur = ref 0 in
  if FIFO.empty requetes then 
    for i=0 to 100 do
      addReqFifo (random_id ()) requetes
    done;
  while (not(FIFO.empty requetes) && !compteur < 100) do 
    let (bencoded, serv_addr) = FIFO.pop requetes in
    sendto socket bencoded 0 (String.length bencoded) [] serv_addr;
    incr compteur
  done;
  receive_requests requetes socket
;;
    

let main =
  let requetes = FIFO.make 1000 in
  let s = socket PF_INET SOCK_DGRAM 0 in
  let premiere_target = random_id () in 
  let premiere_requete = bencodeQuery (QFindNode {qfn_t = "aa"; qfn_want = 1; qfn_id="12345678901234567890"; qfn_target = premiere_target}) in
  FIFO.push (premiere_requete, addrBootstrap) requetes;
  send_requests requetes s
;;
