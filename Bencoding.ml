(* Queries *)

type qPing = (*type des requetes ping*)
    {
      qp_t : string;
      qp_id : string;
    };;

type qFindNode =(*type des requetes find_nodes*)
    {
      qfn_t : string;
      qfn_id : string;
      qfn_target : string;
      qfn_want : int; (* 1(=01) : ip4, 2 (=10) ip6, 3 (=11) ip4 and ip6 *)
    };;

type query = QPing of qPing | QFindNode of qFindNode;;

(** Bencoding *)


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
      "4:want";
      begin
      match r.qfn_want with
        | 2 -> "l2:n6e"
        | 3 -> "l2:n42:n6e"
        | _ -> "l2:n4e"
      end;
      "e1:q9:find_node1:t";
      bencodeStr r.qfn_t;
      "1:y1:q";
      "e"
    ]
;;
    
let bencodeQuery  =
  function
    | QPing r -> bencodeQPing r
    | QFindNode r -> bencodeQFindNode r
;;

(* Answers *)


type aPing = (*type des réponses ping*)
    {
      ap_t : string;
      ap_id : string;
      ap_v : string;
    };;

type aFindNode = (*type des réponses find_nodes*)
    {
      afn_t : string;
      afn_id : string;
      afn_nodes : string list;
      afn_nodes6 : string list;
      afn_v : string;
    };;

type answer = APing of aPing | AFindNode of aFindNode;;

(* parser *)

type bencoded = BDic of (string * bencoded) list | BString of string;;


exception Bad_Answer of string;;

let parser s =
(*décode une réponse*)
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
      | _ -> raise (Bad_Answer "Parse error :(")
  in
  aux ()
;;
          
(* Bencoded to answer *)

 

let verif_champ_t l = 
  try 
    let t = List.assoc "t" l in
    begin
      match t with
	|BString a -> a
	|BDic _ -> raise (Bad_Answer "Contenu du champ t invalide")
    end;
  with Not_found -> ""
      
    
let verif_champ_r l = 
  try
    let r = List.assoc "r" l in
    begin
      match r with
      |BString _ ->  raise (Bad_Answer "Contenu du champ r invalide")
      |BDic dic -> dic
    end
  with Not_found -> raise (Bad_Answer "Champ r manquant dans la reponse")
    
let verif_champ_v l =
  try
    let v = List.assoc "v" l in 
    begin
      match v with
      |BString b -> b
      |BDic _-> ""
    end
  with Not_found -> ""


(*bencode_to_idAndDic : bencoded -> (string*BDic 'a)*)

let bencoded_to_idAndDicAndV b = 
  match b with
  |BString _-> raise (Bad_Answer "La réponse n'est pas un BDic")
  |BDic l -> begin
    let a = verif_champ_t l in
    let dic = verif_champ_r l in
    let v = verif_champ_v l in
    (a, dic, v)
  end
    
   

let bencoded_to_id b = 
  let (a, dic, v) = bencoded_to_idAndDicAndV b in
  try 
    let id = (List.assoc "id" dic) in
      begin
	match id with
	|BString num -> num
	|BDic _ ->  raise (Bad_Answer "Contenu du champ id invalide")
      end
  with Not_found -> raise (Bad_Answer "Champ id manquant dans la réponse")
      


(* bencoded_to_answer : bencoded -> answer *)
let bencoded_to_PingAnswer b= 
  let (a, dic, v) = bencoded_to_idAndDicAndV b in
  if (List.length dic != 1) 
  then raise (Bad_Answer "Longueur du champ r invalide")
  else let num = bencoded_to_id b in  {ap_t = a; ap_id = num; ap_v = v}
;;


let rec decoupe_nodes = function
  | "" -> []
  | s ->
      let n = String.length s in
      (String.sub s 0 26)::(decoupe_nodes (String.sub s 26 (n-26)))
;;

let rec decoupe_nodes6 = function
  | "" -> []
  | s ->
      let n = String.length s in
      (String.sub s 0 38)::(decoupe_nodes (String.sub s 38 (n-38)))
;;
      

let bencoded_to_Find_NodesAnswer b= 
  let (a, dic, v) = bencoded_to_idAndDicAndV b in
  let num = bencoded_to_id b in
  let nodes = 
    try      
      begin
	match (List.assoc "nodes" dic) with
	|BString id_nodes -> decoupe_nodes id_nodes 
	|BDic _ ->  raise (Bad_Answer "Contenu du champ nodes invalide")
      end
    with Not_found -> []
  in
  let nodes6 = 
    try      
      begin
	match (List.assoc "nodes6" dic) with
	|BString id_nodes -> []
	|BDic _ ->  raise (Bad_Answer "Contenu du champ nodes6 invalide")
      end
    with Not_found -> ["none"]
  in
  {afn_t = a; afn_id = num; afn_nodes = nodes; afn_nodes6 = nodes6; afn_v = v}
;;



let get_ip s =
  let aux n = string_of_int (Char.code (s.[20+n])) in
  (String.concat "." (List.map aux [0;1;2;3]) : string)
;;
let get_port s = 
  let port = String.sub s 24 2 in
  (Char.code (port.[0]))*256 + (Char.code (port.[1]))
;;
