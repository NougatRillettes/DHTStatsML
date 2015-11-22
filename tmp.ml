open Unix
open Bencoding



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
let ens = Hashtbl.create 500000;;

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
         (Hashtbl.find ens answ.afn_id).unanswered_requests := 0
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
          let n = (Hashtbl.length ens) in
          Printf.printf "Nous avons un set qui a une taille de %i\n%!" n;
          if true
          then (send_requests requetes socket)
          else ()
      |[socket] -> begin handleReadySocket socket requetes; receive_requests requetes socket end
      |_ -> begin Printf.printf "erreur interne!\n"; send_requests requetes socket end
  end
    
and send_requests requetes socket= 
  let compteur = ref 0 in
 
  for i=0 to 1000 do
    addReqFifo (random_id ()) "\235\2556isQ\255J\236)\205\186\171\242\251\227F|\194g"  addrBootstrap requetes
  done;
  while (not(FIFO.empty requetes) && !compteur < 30000) do

      let (bencoded, serv_addr, id_node) = FIFO.pop requetes in
        (*Printf.printf "Sending to %S\n%!" id_node;*)
      try
        envoie socket bencoded serv_addr;
        begin
          try 
	    incr (Hashtbl.find ens id_node).unanswered_requests;
          with Not_found -> 
            begin
	      let i = ref 1 in
	      Hashtbl.add ens id_node {unanswered_requests = i};
            end;
        end;
        incr compteur
    with | (Unix_error (EINVAL,_,_)) -> ();
  done;
  receive_requests requetes socket
;;
    

let main =
  let requetes = FIFO.make 300000 in
  let s = socket PF_INET SOCK_DGRAM 0 in
  send_requests requetes s
;;
