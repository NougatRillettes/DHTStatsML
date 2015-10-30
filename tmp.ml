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
      if (List.length l != 3) then raise (Bad_Answer "Reponse de longueur incorrecte");
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

let bytes_to_string = String.create

let envoie_requetePing serv_addr=  ()
(* ca devrait etre à peu près bon mais il connait ni Bytes ni sendto_substring du coup ça marche po :(
  let s = socket PF_INET SOCK_DGRAM 0 in
  let bencodedPingQuery=(bencodeQuery (QPing {qp_t = "aa"; qp_id="abcdefghij0123456789"})) in
  Unix.sendto_substring s bencodedPingQuery 0 1500 [] serv_addr (*changer les valeurs 0 et 1500!!*)
  let buffer_reponse = Bytes.create 1500 in
  recvfrom s buffer_reponse 0 1500 ;
  parser (bytes_to_string (buffer_reponse)) *)

;;

