open Unix

type qPing =
    {
      qp_t : string;
      qp_id : string;
    };;

type qFindNode =
    {
      qfn_t : string;
      qfn_id : string;
      qfn_target : string;
    };;

type aPing =
    {
      ap_t : string;
      ap_id : string;
    };;

type aFindNode =
    {
      afn_t : string;
      afn_id : string;
      afn_nodes : string list;
    };;

type query = QPing of qPing | QFindNode of qFindNode;;

type answer = APing of aPing | AFindNode of aFindNode;;


let bencodeStr s =
  String.concat ""
    [
      string_of_int (String.length s);
      ":";
      s
    ]

let bencodeQPing r =
  String.concat ""
          [ "d1:ad2:id";
            bencodeStr r.qp_id;
            "e1:q4:ping1:t";
            bencodeStr r.qp_t;
            "1:y1:qe"
          ]
;;

let bencodeQFindNode r =
  String.concat ""
    [
      "d1:ad2:id";
      bencodeStr r.qfn_id;
      "6:target";
      bencodeStr r.qfn_target;
      "e1:q9:find_node1:t";
      bencodeStr r.qfn_t;
      "1:y1:qe"
    ]
;;
    
let bencodeQuery  =
  function
    | QPing r -> bencodeQPing r
    | QFindNode r -> bencodeQFindNode r
;;


type bencoded = BDic of (string * bencoded) list | BString of string;;


exception Bad_Answer of string;;

let parser s =
  let i = ref 0 in
  let parse_string () =
    let i_column = String.index_from s !i ':' in
    let l = int_of_string (String.sub s !i (i_column - !i)) in
    i := (i_column + 1 + l);
    String.sub s (i_column + 1) l;
  in
  let rec aux () =
    match s.[!i] with
      | 'd' ->
          let rec parsedic () =
            match s.[!i] with
              | 'e' ->
                  incr i;
                  BDic []
              | _ ->
                  let k = parse_string () in
                  let v = aux () in
		  let tmp = parsedic () in 
		  begin
		    match tmp with
		    |BDic l -> BDic ((k,v) :: l)
		    |BString _ -> raise (Bad_Answer "Réponse reçue invalide")
		  end
          in
          incr i;
          parsedic ();
      | 'l' | 'i' ->
          while s.[!i] != 'e' do
            incr i;
          done;
          BDic [];
      | x when '0' <= x && x <= '9' ->
          BString (parse_string ());
      | _ -> failwith "Parse error :("
  in
  aux ()
;;
          


(*bencode_to_idAndDic : bencoded -> (string*BDic 'a)*)
let bencoded_to_idAndDic b = 
  match b with
  |BString _-> raise (Bad_Answer "La réponse n'est pas un BDic")
  |BDic l -> 
    begin
      try 
	let y = List.assoc "y" l in
	begin
	  match y with
	  |BString "r" -> 
	    let t = List.assoc "t" l in 
	    begin
	      match t with
	      |BString a -> 
		begin
		  let r = List.assoc "r" l in
		  begin
		    match r with
		    |BString _ ->  raise (Bad_Answer "Contenu du champ r invalide")
		    |BDic dic -> (a, dic)
		  end
		end
	      |BDic _ -> raise (Bad_Answer "Contenu du champ t invalide")
	    end;
	  |BString _ -> raise (Bad_Answer "Contenu du champ y invalide")
	  |BDic _ -> raise (Bad_Answer "Contenu du champ y invalide")
	end;
      with Not_found -> raise (Bad_Answer "Champ manquant dans la réponse dans la reponse");
    end
;;



let bencoded_to_id b = 
  let (a, dic) = bencoded_to_idAndDic b in
  try 
    let id = (List.assoc "id" dic) in
      begin
	match id with
	|BString num -> num
	|BDic _ ->  raise (Bad_Answer "Contenu du champ id invalide")
      end
  with Not_found -> raise (Bad_Answer "Champ id manquant dans la réponse")
      


(* bencoded_to_asnwer : bencoded -> answer *)
let bencoded_to_PingAnswer b= 
  let (a, dic) = bencoded_to_idAndDic b in
  if (List.length dic != 1) 
  then raise (Bad_Answer "Longueur du champ r invalide")
  else let num = bencoded_to_id b in  {ap_t = a; ap_id = num}
;;


let decoupe_nodes s = 
  (String.sub s 0 20)::(String.sub s 20 20)::(String.sub s 40 20)::(String.sub s 60 20)::(String.sub s 80 20)::(String.sub s 100 20)::(String.sub s 120 20)::(String.sub s 140 20)::[]
;;
      
      
let bencoded_to_Find_NodesAnswer b= 
  let (a, dic) = bencoded_to_idAndDic b in
  if (List.length dic != 2) 
  then raise (Bad_Answer "Longueur du champ r invalide")
  else 
      let num = bencoded_to_id b in 
      try
	let nodes = (List.assoc "nodes" dic) in 
	begin
	  match nodes with
	  |BString id_nodes -> {afn_t = a; afn_id = num; afn_nodes = (decoupe_nodes id_nodes)}
	  |BDic _ ->  raise (Bad_Answer "Contenu du champ nodes invalide")
	end
      with Not_found -> raise (Bad_Answer "Champ nodes manquant dans la réponse")
;;


let envoie_requetePing requetePing serv_addr =
  let s = socket PF_INET SOCK_DGRAM 0 in
  let bencodedPingQuery=bencodeQuery ( requetePing ) in                     (*QPing {qp_t = "aa"; qp_id="abcdefghij0123456789"})) in*)
  ignore @@ sendto s bencodedPingQuery 0 (String.length bencodedPingQuery) [] serv_addr; 
  let buffer_reponse = String.create 1500 in
  ignore @@ recvfrom s buffer_reponse 0 1500 [];
  bencoded_to_id (parser buffer_reponse)
;;


let envoie_requeteFind_nodes requeteFind_Nodes serv_addr=
  let s = socket PF_INET SOCK_DGRAM 0 in
  let bencodedFind_nodesQuery=bencodeQuery ( requeteFind_Nodes ) in    (*QFindNode {qfn_t = "aa"; qfn_id="abcdefghij0123456789"; qfn_target = target})) in*)
  ignore @@ sendto s bencodedFind_nodesQuery 0 (String.length bencodedFind_nodesQuery) [] serv_addr; 
  let buffer_reponse = String.create 1500 in
  ignore @@ recvfrom s buffer_reponse 0 1500 [];
  bencoded_to_Find_NodesAnswer (parser buffer_reponse)
;;



let addrBootstrap = ADDR_INET(inet_addr_of_string "67.215.246.10", 6881);;

let rec euclidean_rest n m = if (n<m) then n else (euclidean_rest (n-m) m)



let int_to_trans_num i = 
(*d'un entier vers un string de transaction number*)
  let res = "aa" in 
  res.[0] <- (Char.chr ((!i-(euclidean_rest !i 256))/256));
  res.[1] <- (Char.chr (euclidean_rest !i 256));
  res
;; 


let generateTargetNode () = "abcdefghij0123456789" ;;(*trouver un noeud à chercher, en maintenant une liste de noeuds déjà trouvés?*)


let targetNode = ref "abcdefghij0123456789";;
let trans_num = ref 1 ;;
let myID = "jihgfedbca9876543210";;


let rec trouve_noeud requestsToSend =
(*on démarre cette fonction avec requestsToSend ayant un seul élément, un QFindNode vers un certain noeud en cherchant un certain targetNode.
Cette fonction s'arrete lorsque toutes les requetes ont été effectuées.*) 
    match requestsToSend with
    |[] -> ()
    |(QPing x, serv_addr)::requestsToSend' -> begin ignore(envoie_requetePing (QPing x) serv_addr); trouve_noeud requestsToSend' end
    |(QFindNode x, serv_addr)::requestsToSend' -> 
      begin
	let answer = envoie_requeteFind_nodes (QFindNode x) serv_addr in
	if ((*on nous renvoie l'addresse ip du targetNode*) false) 
	then  (*alors on le ping*)
	  begin 
	    incr trans_num; 
	    trouve_noeud ((QPing{qp_t = (int_to_trans_num trans_num); qp_id= !targetNode} , (*normalement nv_serv_addr*) serv_addr)::(requestsToSend'))
	  end
	else if ((*on nous renvoie l'addresse ip d'un noeud plus proche*) false)
	then (*on lui demande où est targetNode*)
	  begin
	    incr trans_num; 
	    trouve_noeud ((QFindNode {qfn_t = (int_to_trans_num trans_num) ; qfn_id=myID; qfn_target = !targetNode},  (*normalement nv_serv_addr*) serv_addr)::requestsToSend')
	  end
	else (*on nous renvoie les ids d'autres noeuds plus proches*)
	  begin
	    let nvellesreqs = List.map 
	      (fun x ->  incr trans_num ; (QFindNode {qfn_t = (int_to_trans_num trans_num) ; qfn_id=myID; qfn_target =x}, serv_addr)) answer.afn_nodes in
	     trouve_noeud (nvellesreqs@requestsToSend') (*est-ce qu'il vaut mieux faire d'abord les novuelles requetes ou celles d'avant?*)
	  end
      end
;;


let main = 
    while (true) 
    do
      targetNode := generateTargetNode () ;
      trans_num := 1 ; 
      let requestsToSend = (QFindNode {qfn_t = (int_to_trans_num trans_num); qfn_id="abcdefghij0123456789"; qfn_target = !targetNode}, addrBootstrap)::[] 
      in incr trans_num; (trouve_noeud requestsToSend)
    done
;;
