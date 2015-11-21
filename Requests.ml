
open Unix;;
open Bencoding;;

let envoie_requetePing requetePing serv_addr =
  let s = socket PF_INET SOCK_DGRAM 0 in
  let bencodedPingQuery=bencodeQuery ( requetePing ) in                     (*QPing {qp_t = "aa"; qp_id="abcdefghij0123456789"})) in*)
  ignore ( sendto s bencodedPingQuery 0 (String.length bencodedPingQuery) [] serv_addr); 
  let buffer_reponse = String.create 1500 in
  ignore ( recvfrom s buffer_reponse 0 1500 []);
  close s;
  bencoded_to_id (parser buffer_reponse)
;;

let envoie_requeteFind_nodes requeteFind_Nodes serv_addr=
  let s = socket PF_INET SOCK_DGRAM 0 in
  let bencodedFind_nodesQuery=bencodeQuery ( requeteFind_Nodes ) in    (*QFindNode {qfn_t = "aa"; qfn_id="abcdefghij0123456789"; qfn_target = target})) in*)
  ignore ( sendto s bencodedFind_nodesQuery 0 (String.length bencodedFind_nodesQuery) [] serv_addr); 
  let buffer_reponse = String.create 1500 in
  ignore ( recvfrom s buffer_reponse 0 1500 []);
  close s;
  bencoded_to_Find_NodesAnswer (parser buffer_reponse)
;;
