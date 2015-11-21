open Bencoding
open Unix

val envoie_requetePing : query -> sockaddr -> string

val  envoie_requeteFind_nodes : query -> sockaddr -> aFindNode
