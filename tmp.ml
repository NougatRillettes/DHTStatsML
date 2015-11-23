open Unix
open Bencoding
open Tools


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


let addrBootstrap = ref (ADDR_INET (inet_addr_of_string "82.221.103.244", 6881));;


let int_to_trans_num i = 
(*d'un entier vers un string de transaction number*)
  let res = "aa" in 
  res.[0] <- (Char.chr ((i-(i mod 256))/256));
  res.[1] <- (Char.chr (i mod 256));
  res
;; 

let trans_num = ref 1 ;;
let ourID = ref "jihgfedbca9876543210";;

type node_info = 
  {
    mutable asked : bool;
    mutable answered : bool;
    mutable version_used : string;
    mutable truncated_t : bool;
}

module NodeInfoMap = Map.Make(String) ;;
let ens = Hashtbl.create 500000;;


let statTimer = ref (Unix.time ());;
let lastCard = ref 0.0;;

let addReqFifo target id addr fifo =
   let query =
      bencodeQFindNode {
        qfn_id = !ourID;
        qfn_t = "789632145";
        qfn_target = target;
        qfn_want = 1;
      }
   in
   FIFO.push (query,addr,id) fifo;
;;

let handleReadySocket sck fifo =
  let readBuf = String.make 1500 '0' in
  let (_,addrFrom) = (recvfrom sck readBuf 0 1500 []) in
  try
    let answ = bencoded_to_Find_NodesAnswer (parser readBuf) in
     begin
       try
         begin
        (* Printf.printf "Receiving from %S\n%!" answ.afn_id;  *)
           (Hashtbl.find ens answ.afn_id).answered <- true;
           if (((Hashtbl.find ens answ.afn_id).version_used ) == "")
	   then (Hashtbl.find ens answ.afn_id).version_used <- answ.afn_v;
           (Hashtbl.find ens answ.afn_id).truncated_t <- (answ.afn_t != "789632145")
	 end
      with
        | Not_found ->  Printf.printf "Erreur interne\n";
     end;
    let genAddr s =
      let ip = get_ip s in
      let port = get_port s in
      (* Printf.printf "Adding to FIFO : %S at %s:%d\n%!" (get_id s) ip port; *)
  ADDR_INET(inet_addr_of_string ip, port)
    in
    for i = 0 to 00 do
      addReqFifo (random_id ()) (answ.afn_id) (addrFrom) fifo;
    done;
    List.iter
      (fun s ->
        addReqFifo (random_id ()) (get_id s) (genAddr s) fifo;
      )
      answ.afn_nodes;
    (* Printf.printf "Fifo size : %d\n%!" (FIFO.size fifo); *)
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
  end;;

let bootStrapId = String.make 20 '0';;
      
let rec receive_requests requetes socket= 
  let (f1, f2, f3) = select [socket] [] [] 0. in
  begin
    match f1 with
      |[] ->
          let n = (Hashtbl.length ens) in
          let timeR = 0.0 in
          if (Unix.time ()) -. !statTimer >= timeR then
            begin
              statTimer := Unix.time ();
              Printf.printf "Nous avons un set qui a une taille de %i\n" n;
              Printf.printf "Vitesse : %F\n%!"
                (((float_of_int n) -. !lastCard) /. (if timeR = 0.0 then 1.0 else timeR));
              lastCard := float_of_int n;
            end;
          if true
          then (send_requests requetes socket)
          else ()
      |[socket] -> begin handleReadySocket socket requetes; receive_requests requetes socket end
      |_ -> begin Printf.printf "erreur interne!\n"; send_requests requetes socket end
  end
    
and send_requests requetes socket= 
  let compteur = ref 0 in
  if FIFO.empty requetes then
    for i=0 to 25000 do
      addReqFifo (random_id ()) bootStrapId !addrBootstrap requetes
    done;
  while (not(FIFO.empty requetes) && !compteur < 30000) do

      let (bencoded, serv_addr, id_node) = FIFO.pop requetes in
        (* Printf.printf "Sending to %S\n%!" id_node; *)
      try
        envoie socket bencoded serv_addr;
        begin
          try 
	   (Hashtbl.find ens id_node).asked <- true;
          with Not_found -> 
            begin
	      Hashtbl.add ens id_node {asked = true; version_used = ""; truncated_t = false; answered = false; };
            end;
        end;
        incr compteur
    with | (Unix_error (EINVAL,_,_)) -> ();
  done;
  receive_requests requetes socket
;;
    

let main =
  let (addrinfo::_) = getaddrinfo "router.utorrent.com" "6881" [] in
  addrBootstrap := addrinfo.ai_addr;
  let requetes = FIFO.make (int_of_float 10e5) in
  let s = socket PF_INET SOCK_DGRAM 0 in
  ourID := random_id ();
  let bencodedQuery =
    bencodeQFindNode {
      qfn_id = !ourID;
      qfn_t = "00";
      qfn_target = !ourID;
      qfn_want = 1;
    } in    
  ignore ( sendto s bencodedQuery 0 (String.length bencodedQuery) [] !addrBootstrap); 
  let buffer_reponse = String.create 1500 in
  ignore ( recvfrom s buffer_reponse 0 1500 []);
  let bootStrapId' =  bencoded_to_id (parser buffer_reponse) in
  for i = 0 to 19 do
    bootStrapId.[i] <- bootStrapId'.[i];
  done;
  send_requests requetes s
;;
