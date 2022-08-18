open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = match t with 
| IntLeaf -> IntNode(x,None,IntLeaf,IntLeaf,IntLeaf)
| IntNode(a,None,c,d,e) 
	-> if a>x then IntNode(x,Some a,c,d,e) else if x>a then IntNode(a,Some x,c,d,e) else t
| IntNode(a,Some b,c,d,e) -> if x<a then IntNode(a,Some b,(int_insert x c),d,e) else if x>a then IntNode(a,Some b, c,d,(int_insert x e)) else IntNode(a,Some b,c,(int_insert x d),e)

let rec int_mem x t = match t with
| IntLeaf -> false
| IntNode(a,None,c,d,e) -> if x=a then true else false
| IntNode(a,Some b,c,d,e) -> if x<a then (int_mem x c) else if x>b then (int_mem x e) else if x<>a && x<>b then (int_mem x d) else true

let rec int_size t = match t with
| IntLeaf -> 0
| IntNode(a, None,c,d,e) -> 1
| IntNode(a, Some b, c,d,e) -> 2 + (int_size c)+(int_size d)+(int_size e)

let rec int_max t =if t=IntLeaf then 
	 raise (Invalid_argument "int_max")
else
(match t with
| IntLeaf -> 0
| IntNode(a,None,c,d,e) -> a
| IntNode(a,Some b,c,d,e) -> if e=IntLeaf then b else max b (int_max e))

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_contains k t =
 match t with
| MapLeaf -> false
| MapNode((a,v1),None,c,d,e) -> if k=a then true else false
| MapNode((a,v1),Some (b,v2),c,d,e) -> if k<a then (map_contains k c) else if k>b then (map_contains k e) else if k<>a && k<>b then (map_contains k d) else true


let rec map_put k v t = if (map_contains k t) then 
	raise (Invalid_argument "map_put")
else
match t with
| MapLeaf -> MapNode((k,v),None,MapLeaf,MapLeaf,MapLeaf)
| MapNode((a,b),None,c,d,e) -> if k<a then MapNode((k,v),Some (a,b),c,d,e) else MapNode((a,b),Some (k,v),c,d,e) 
| MapNode((k1,v1),Some (k2,v2),a,b,c) -> if k<k1 then MapNode((k1,v1),Some (k2,v2),(map_put k v a),b,c) else if k>k2 then MapNode((k1,v1),Some (k2,v2),a,b,(map_put k v c)) else MapNode((k1,v1),Some (k2,v2),a,(map_put k v b), c)




let rec map_get k t =
match t with
| MapLeaf -> raise (Invalid_argument "map_get")
| MapNode((k1,v1),None,c,d,e) -> if k1=k then v1 else raise (Invalid_argument "map_get")
| MapNode((k1,v1),Some (k2,v2),a,b,c) -> if k<k1 then (map_get k a) else if k>k2 then (map_get k c) else if k=k1 then v1 else if k=k2 then v2 else (map_get k b)

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (int * ((string * int) list)) list

let empty_table : lookup_table = [(0,[])]

let push_scope t =  match t with
| (n,[])::t -> (n+1,[])::(n,[])::t
| (m,(i,j)::s)::t -> (m+1,[])::(m,(i,j)::s)::t
| [] -> [(0,[])] 

let pop_scope t = match t with 
| (0,[])::[] -> failwith "No scopes remain!"
| (n,[])::t -> t
| (n,(a,b)::s)::t -> t
| [] -> failwith "No scopes remain!"
 


let rec add_var name value t =( let contains e lst = fold (fun a l -> match l with
| (i,j) -> if i=e then true else a
) false lst
in match t with
| (0,[])::[] -> failwith "There are no scopes to add a variable to!"
| (n,[])::(m,[])::t -> (n,(name,value)::[])::(m,[])::t
| (n,[])::(c,(d,e)::r)::q -> if d=name 
	then failwith "Duplicate variable binding in scope!"
	else if (contains name r)=true then failwith "Duplicate variable in scope!"
	else (n,(name,value)::[])::(c,(d,e)::r)::q
| (n,(a,b)::t)::p -> if a=name 
	then failwith "Duplicate variable binding in scope!" 
	else if (contains name t)=true then failwith "Duplicate variable binding in scope!"
 	else (n,(name,value)::(a,b)::t)::p 
| (_,[])::[] ->failwith "There are no scopes to add a variable to!" 
| []-> failwith "There are no scopes to add a variable to!"
)


let rec find e lst = match lst with
| (i,j)::t -> if i=e then j else find e t
| [] -> -101 

let rec lookup name t = (let contains e lst = fold (fun a l -> match l with
| (i,j) -> if i=e then true else a
) false lst
in match t with
| (0,[])::[] -> failwith "Variable not found!"
| (n,(a,b)::s)::t -> if a=name then b else if (contains name s)=true 
	then find name s 
	else lookup name t
| (n,[])::t -> lookup name t
| [] -> failwith "Variable not found!"
)
