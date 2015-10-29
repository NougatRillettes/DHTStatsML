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

(* bencoded_to_asnwer : bencoded -> answer *)
let bencoded_to_answer = ();;
  (* GWEN COMPLETE ICI STP, LOVE LOVE *)


      
      
      
      
