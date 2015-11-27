open Unix
open Bencoding
open Tools

let addrBootstrap = ref (ADDR_INET (inet_addr_of_string "82.221.103.244", 6881));;

let ourID = ref "jihgfedbca9876543210";;
let comptTMP = ref 0;;

type node_info = 
  {
    mutable asked : int;
    mutable answered : bool;
    mutable version_used : string;
    mutable truncated_t : bool;
    mutable bep32 : bool;
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
        qfn_want = 3;
      }
   in
   FIFO.push (query,addr,id) fifo;
;;

let handleReadySocket sck fifo =
  incr comptTMP;
  let genAddr s =
    let ip = get_ip s in
    let port = get_port s in
    (*Printf.printf "%s : %i\n" ip port;*)
    (*Printf.printf "Adding to FIFO : %S at %s:%d\n%!" (get_id s) ip port;*)
    ADDR_INET(inet_addr_of_string ip, port)
  in
  let readBuf = String.make 1500 '0' in
  let (_,addrFrom) = (recvfrom sck readBuf 0 1500 []) in
  try
    let answ = bencoded_to_Find_NodesAnswer (parser readBuf) in
    let ensEntry =
      try
        (Hashtbl.find ens answ.afn_id)
      with
        | Not_found ->begin 

            Hashtbl.add ens answ.afn_id {asked = 0; version_used = ""; truncated_t = false; answered = true; bep32 = false};
            (Hashtbl.find ens answ.afn_id);
	end
    in
    (*Printf.printf "Receiving from %S\n%!" answ.afn_id; *)
    ensEntry.version_used <- answ.afn_v;
    ensEntry.truncated_t <- ensEntry.truncated_t || (answ.afn_t != "789632145");
    ensEntry.bep32 <- ensEntry.bep32 || not(answ.afn_nodes6 = []);
    (* Discover neighbors *)
    if not ensEntry.answered then
      begin
    	List.iter
    	  (fun s ->
            addReqFifo s answ.afn_id addrFrom fifo
    	  )
    	  (generateNeighbors answ.afn_id);
    	ensEntry.answered <- true;
      end;
    (* Probe given nodes *)
    List.iter
      (fun s ->
        try
	  let new_node = (Hashtbl.find ens s) in
          let asked = new_node.asked in
          if (asked < 5) then addReqFifo (random_id()) (get_id s) (genAddr s) fifo;
        with
          | Not_found ->
              addReqFifo (random_id()) (get_id s) (genAddr s) fifo;
      )
      answ.afn_nodes;
  with
    | Bad_Answer s ->  ();
    | _ -> ()
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

let dumpStats () =
  let hname = try Sys.argv.(1) with | _ -> "" in
  let file = open_out (Printf.sprintf "DHTdata%s_%f" hname (Unix.time ())) in
  let aux s n = (Char.code s.[n])*256 + (Char.code s.[n+1]) in
  Hashtbl.iter
    (fun id data ->
      Printf.fprintf file
        "%04x%04x%04x%04x%04x%04x%04x%04x%04x%04x %d %B %B %30S\n"
        (aux id 0)
        (aux id 2)
        (aux id 4)
        (aux id 6)
        (aux id 8)
        (aux id 10)
        (aux id 12)
        (aux id 14)
        (aux id 16)
        (aux id 18)
        data.asked
        data.answered
        data.truncated_t
        data.version_used
    )
    ens
;;
      
let rec receive_requests requetes socket= 
  let (f1, f2, f3) =
    let rec aux () =
      try
        select [socket] [] [] 1e-6
      with
        | (Unix_error (EINTR,_,_)) -> aux ()
    in
    aux ()
  in
  begin
    match f1 with
      |[] ->
          let n = (Hashtbl.length ens) in
          let timeR = 1.0 in
          if (Unix.time ()) -. !statTimer >= timeR then
            begin
              statTimer := Unix.time ();
              Printf.printf "Nous avons un set qui a une taille de %i\n" n;
	      let noeuds_qui_ont_repondu = ref 0 in
	      let noeuds_bep32 = ref 0 in
	      Hashtbl.iter 
		(fun x y -> 
		  if (y.answered) then incr noeuds_qui_ont_repondu;
		  if y.bep32 then incr noeuds_bep32;
		) 
		ens;
	      Printf.printf "Noeud qui nous ont répondu: %i dont ipv6 : %i\n" (!noeuds_qui_ont_repondu) (!noeuds_bep32);
	      Printf.printf "Donc pourcentage de réponses : %i dont ipv6 : %i\n" ((!noeuds_qui_ont_repondu)*100/n) ((!noeuds_bep32*100)/n);
              Printf.printf "Recus : %d\n" !comptTMP;
              Printf.printf "Fifo size : %d\n" (FIFO.size requetes); 
              Printf.printf "Vitesse : %F\n%!"                
                (((float_of_int n) -. !lastCard) /. (if timeR = 0.0 then 1.0 else timeR));
              lastCard := float_of_int n;
            end;
          if true
          then (send_requests requetes socket)
          else dumpStats ();
      |[socket] -> begin handleReadySocket socket requetes; receive_requests requetes socket end
      |_ -> begin Printf.printf "erreur interne!\n"; send_requests requetes socket end
  end
    
and send_requests requetes socket= 
  let compteur = ref 0 in
  if FIFO.empty requetes then
    for i=0 to 3 do
      addReqFifo (random_id ()) !ourID !addrBootstrap requetes;
    done;
  while (not(FIFO.empty requetes) && !compteur < 10) do
    let (bencoded, serv_addr, id_node) = FIFO.pop requetes in
    (* Printf.printf "Sending to %S\n%!" id_node; *)
    begin
      try 
	let new_node = Hashtbl.find ens id_node in
	new_node.asked <- new_node.asked +1;
      with Not_found -> 
	begin
	  Hashtbl.add ens id_node {asked = 1; version_used = ""; truncated_t = false; answered = false; bep32 = false};
	end;
    end;
    begin
      try
	envoie socket bencoded serv_addr;
	incr compteur
      with
        | (Unix_error (EINVAL,_,_)) -> ();
        | (Unix_error (_,"sendto","")) -> ();
    end;    
  done;
  receive_requests requetes socket
;;


let main =
  let (addrinfo::_) = getaddrinfo "router.utorrent.com" "6881" [] in
  addrBootstrap := addrinfo.ai_addr;
  let requetes = FIFO.make (int_of_float 1e2) in
  let s = socket PF_INET SOCK_DGRAM 0 in
  Unix.setsockopt_int s SO_SNDBUF (512*1024*1024*1024);
  ourID := random_id ();
  let bencodedQuery =
    bencodeQFindNode {
      qfn_id = !ourID;
      qfn_t = "789632145";
      qfn_target = random_id();
      qfn_want = 1;
    } in    
  envoie s bencodedQuery !addrBootstrap;
  let buffer_reponse = String.create 1500 in
  ignore ( recvfrom s buffer_reponse 0 1500 []);
  let bootStrapId' =  bencoded_to_id (parser buffer_reponse) in
  for i = 0 to 19 do
    bootStrapId.[i] <- bootStrapId'.[i];
  done;
  send_requests requetes s
;;
