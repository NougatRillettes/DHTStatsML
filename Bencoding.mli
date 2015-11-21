
type qPing = { qp_t : string; qp_id : string; }

type qFindNode = {
  qfn_t : string;
  qfn_id : string;
  qfn_target : string;
  qfn_want : int;
}
    
type query = QPing of qPing | QFindNode of qFindNode

val bencodeStr : string -> string
val bencodeQPing : qPing -> string
val bencodeQFindNode : qFindNode -> string
val bencodeQuery : query -> string



type aPing = { ap_t : string; ap_id : string; }

type aFindNode = {
  afn_t : string;
  afn_id : string;
  afn_nodes : string list;
}

type answer = APing of aPing | AFindNode of aFindNode

type bencoded = BDic of (string * bencoded) list | BString of string


val parser : string -> bencoded

val bencoded_to_idAndDic : bencoded -> string * (string * bencoded) list
val bencoded_to_id : bencoded -> string
val bencoded_to_PingAnswer : bencoded -> aPing
val bencoded_to_Find_NodesAnswer : bencoded -> aFindNode
