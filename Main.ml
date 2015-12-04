open Unix
open Bencoding
open Tools

let addrBootstrap = ref (ADDR_INET (inet_addr_of_string "82.221.103.244", 6881));;

let ourID = ref "jihgfedbca9876543210";;
let comptTMP = ref 0;; (*nombre de réponses reçues*)

type node_info = (*ensemble d'informations que l'on stocke pour chaque noeud*)
  {
    mutable asked : int; (*nombre de requetes envoyées au noeud*)
    mutable answered : bool; (*ce noeud a-t-il répondu?*)
    mutable version_used : string; (*quelle est la version utilisée par ce noeud*)
    mutable truncated_t : bool; (*ce noeud a-t-il tronqué le champ t?*)
    mutable bep32 : bool; (*ce noeud utilise-t-il la bep32?*)
}

module NodeInfoMap = Map.Make(String) ;;
let ens = Hashtbl.create 3_000_000;; (*ensemble de toutes les infos associées à chaque id*)
let mens = Mutex.create ();;


let statTimer = ref (Unix.time ());;
let lastCard = ref 0;;
let endTimer = Unix.time ();;

let addReqFifo target id addr fifo = (*ajoute un find_node demandé à id à addr vers target dans fifo*)
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

let handleReadySocket sck fifo = (*traite une réponse reçue*)
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
    Mutex.lock mens;
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
    ensEntry.truncated_t <- ensEntry.truncated_t || (not (answ.afn_t = "789632145"));
    begin
      match answ.afn_nodes6 with
      |["none"] -> ()
      | _ -> ensEntry.bep32 <- true
    end;
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
    Mutex.unlock mens;
  with
    | Bad_Answer s ->  ();   
    | _ -> ();
;;

let rec envoie socket bencoded serv_addr = (*permet de ne pas arreter le programme lorsque sendto soulève l'erreur ENOBUFS*)
  try 
    sendto socket bencoded 0 (String.length bencoded) [] serv_addr
  with
  |Unix_error(ENOBUFS, _, _) -> begin
    Printf.printf "manque de buffer! on réessaie\n";
    sleep(1);
    envoie socket bencoded serv_addr
  end;;

let bootStrapId = String.make 20 '0';;

let dumpStats () = (*écrit les statistiques de l'échantillon*)
  let hname = try Sys.argv.(1) with | _ -> "" in
  let file = open_out (Printf.sprintf "DHTdata%s_%f" hname (Unix.time ())) in
  let aux s n = (Char.code s.[n])*256 + (Char.code s.[n+1]) in
  Hashtbl.iter
    (fun id data ->
      try
        Printf.fprintf file
          "%04x%04x%04x%04x%04x%04x%04x%04x%04x%04x %d %5B %5B %5B %30S\n"
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
          data.bep32
          data.version_used
      with
        | _ -> ();
    )
    ens;
  let filestats = open_out (Printf.sprintf "DHTstats%s_%f" hname (Unix.time ())) in
  let tailledeset = (Hashtbl.length ens) in
  let noeuds_qui_ont_repondu = ref 0 in
  let noeuds_bep32 = ref 0 in
  let noeuds_truncated_t = ref 0 in
  let noeuds_asked = ref 0 in
  let set_versions = Hashtbl.create 100 in
  Hashtbl.iter
    (fun x y ->
      if y.answered then incr noeuds_qui_ont_repondu;
      if y.bep32 then incr noeuds_bep32;
      if y.truncated_t then incr noeuds_truncated_t;
      noeuds_asked := !noeuds_asked + y.asked;
      begin
	try
          Hashtbl.replace set_versions y.version_used ((Hashtbl.find set_versions y.version_used)+1);
	with
	  Not_found -> Hashtbl.add set_versions y.version_used 1;
      end
    )
    ens;
  Printf.fprintf filestats "%i\n" tailledeset; (*taille de l'ensemble*)
  Printf.fprintf filestats "%i\n" !noeuds_qui_ont_repondu; (*nombre de noeuds qui ont répondu*)
  Printf.fprintf filestats "%i\n" !noeuds_bep32; (*nombre de noeuds qui nutilisent bep32*)
  Printf.fprintf filestats "%i\n" !noeuds_truncated_t; (*nombre de noeuds qui ont tronqué le champ t*)
  Printf.fprintf filestats "%i\n" ((!noeuds_asked)/ (!noeuds_qui_ont_repondu)); (*moyenne du nombre de requetes par noeud _qui ont répondu_*)
  Hashtbl.iter (fun x y -> Printf.fprintf filestats "%s\t%i\n" x y) set_versions;
  Printf.fprintf filestats "%i\n" !comptTMP; (*nombre de reponses reçues*)
;;

      
let rec receive_requests requetes socket= (*effectue un select de 0 secondes, puis lit et traite toutes les réponses de la socket*)
  let (f1, f2, f3) =
    let rec aux () =
      try
        select [socket] [] [] 0.0;
      with
        | (Unix_error (EINTR,_,_)) -> aux ()
    in
    aux ()
  in
  begin
    match f1 with
      |[] ->
          (*let n = (Hashtbl.length ens) in*)
          if (Unix.time ()) -. endTimer <= 18.5*.60.
          then (send_requests requetes socket)
          else begin close socket; Thread.exit () end;
      |[socket] -> begin handleReadySocket socket requetes; receive_requests requetes socket end
      |_ -> begin Printf.printf "erreur interne!\n"; send_requests requetes socket end
  end
    
and send_requests requetes socket= 
(*envoie une requete de la file, si la file est vide, ajoute des demandes vers bootstrap*)
  let compteur = ref 0 in
  if FIFO.empty requetes then
    for i=0 to 3 do
      addReqFifo (random_id ()) !ourID !addrBootstrap requetes;
    done;
  while (not(FIFO.empty requetes) && !compteur < 1) do
    let (bencoded, serv_addr, id_node) = FIFO.pop requetes in
    (* Printf.printf "Sending to %S\n%!" id_node; *)
    Mutex.lock mens;
    begin
      try 
	let new_node = Hashtbl.find ens id_node in
	new_node.asked <- new_node.asked +1;
      with Not_found -> 
	begin
	  Hashtbl.add ens id_node {asked = 1; version_used = ""; truncated_t = false; answered = false; bep32 = false};
	end;
    end;
    Mutex.unlock mens;
    begin
      try
	envoie socket bencoded serv_addr;
	incr compteur
      with
        | (Unix_error (EINVAL,_,_)) -> ();
        | (Unix_error (_,"sendto","")) -> ();
    end;    
  done;
  if FIFO.seen requetes >= 800_000 then
    begin
      Printf.printf "Refresh !\n";
      close socket;
      let (r,sck) = genArgs () in
      send_requests r sck;
    end
  else
    receive_requests requetes socket;
;;


let rec intermStats () =
(*affiche des stats lors de l'execution du programme*)
  let timeR = 1 in
  Unix.sleep timeR;
  Mutex.lock mens;
  let n = max 1 (Hashtbl.length ens) in
  Printf.printf "Nous avons un set qui a une taille de %i\n" n;
  let noeuds_qui_ont_repondu = ref 0 in
  let noeuds_bep32 = ref 0 in
  Hashtbl.iter 
    (fun x y -> 
      if (y.answered) then incr noeuds_qui_ont_repondu;
      if y.bep32 then incr noeuds_bep32;
    ) 
    ens;
  Mutex.unlock mens;
  Printf.printf "Noeud qui nous ont répondu: %i dont ipv6 : %i\n" (!noeuds_qui_ont_repondu) (!noeuds_bep32);
  Printf.printf "Donc pourcentage de réponses : %i dont ipv6 : %i\n" ((!noeuds_qui_ont_repondu)*100/n) ((!noeuds_bep32*100)/n);
  Printf.printf "Recus : %d\n" !comptTMP;
  Printf.printf "Vitesse : %i\n%!" ((n - !lastCard) / timeR);
  lastCard := n;
  intermStats ()
;;



let main =
  Thread.create intermStats ();
  let (addrinfo::_) = getaddrinfo "router.utorrent.com" "6881" [] in
  addrBootstrap := addrinfo.ai_addr;
  let s = socket PF_INET SOCK_DGRAM 0 in
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
  close(s);
  let bootStrapId' =  bencoded_to_id (parser buffer_reponse) in
  for i = 0 to 19 do
    bootStrapId.[i] <- bootStrapId'.[i];
  done;
  let nThread = 6 in
  let arrT = Array.make nThread (Thread.self ()) in
  for i = 1 to nThread do
    let (r,sck) = genArgs () in
    arrT.(i-1) <- Thread.create (send_requests r) sck;
  done;
  for i = 0 to nThread - 1 do
    Thread.join arrT.(i);
  done;
  dumpStats ();
;;
