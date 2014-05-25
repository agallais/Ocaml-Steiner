type poids = Null |P of int;;

let creer_graphe n =
  let graph = Array.make_matrix n n Null in

for i = 0 to (n-1) do 
(*On met des 1 de manière aléatoire *)

for j = 0 to 2 do
  let a = Random.int(n) in 
graph.(i).(a)<-P(1); graph.(a).(i)<- P(1);
done;

graph.(i).(i)<- P(0);

done;

graph;;


let somme p1 = function
  |Null -> Null
  |P(m)-> if p1=Null then Null else let P(n)= p1 in P(n+m);; 

min 1 2;;
let mini p1 = function
  |Null -> p1 
  |P(m)-> if p1 = Null then P(m) else let P(n)= p1 in (P(min n m));;

let completer_graph g =
  let n = Array.length g.(0) in
for i = 0 to (n-1) do 

for j = (i+1) to (n-1) do


for k = 0 to (n-1) do 

g.(i).(j) <- mini (g.(i).(j)) (somme g.(i).(k) g.(k).(j));

done;
g.(j).(i)<- g.(i).(j);
done;
done;
;;
let existe_Null g =
  let n = Array.length g in
  let compteur = ref 0 in
for i = 0 to (n-1) do 
for j= (i+1) to (n-1) do
 if g.(i).(j) = Null then compteur := !compteur +1;
done;
done;
!compteur;;




(*On a réussi à compléter le graphe *)

(*on va s'attaquer au problème de l'arbre couvrant*)

let kruskal graph= 
  let n = Array.length graph in
  let aux1 g n = let l = ref [] in
for i = 0 to (n-1) do
for j= (i+1) to (n-1) do

l:= (( i,j, graph.(i).(j))::!l);

done;
done;
!l
  in
aux1 graph n 
;;
(*kuskal fait la liste des arêtes du graphe*)


let comp a b =
match (a,b) with
|(_,_,P(i)),(_,_,P(j))-> ( i <= j)
;; 
(* le poids de a est strictement plus grand que le poids de b *)

let rec sort = function
  | [] -> []
  | x :: l -> insert x (sort l)

and insert elem = function
  | [] -> [elem]
  | x :: l -> 
      if comp elem x then elem :: x :: l else x :: insert elem l;;

let create_forest g = 
  let n = Array.length g in 
  let foret = Array.make n 0 in
for i = 0 to (n-1) do
foret.(i) <- i ;
done;
foret
;;

let rec etape_kruskal foret l acc =
match l with
|( i, j , P(n)) ::q -> if foret.(i)<>foret.(j) then 
    begin
foret.(j) <- foret.(i);
etape_kruskal foret q (( i, j , P(n))::acc)
    end
  else etape_kruskal foret q acc
|[]-> (foret,acc)
;;




let rec rev acc = function
  |[]-> acc
  |t::q -> rev (t::acc) q 
;;



(*Et Steiner désormais *)

let completer2 g =
  let n = Array.length g.(0) in
  let completude = Array.make_matrix n n [] in
for i = 0 to (n-1) do 
for j = (i+1) to (n-1) do
for k = 0 to (n-1) do 
if g.(i).(k) <> Null && g.(k).(j) <> Null then
begin
  let (P(m))= (somme g.(i).(k) g.(k).(j)) in
  if g.(i).(j) = Null then begin 
completude.(i).(j) <-  completude.(i).(k)@completude.(k).(j);
g.(i).(j)<- P(m);
end else  let P(l) = g.(i).(j) in
if l<= m then ()
else begin 
completude.(i).(j) <-  completude.(i).(k)@completude.(k).(j);
g.(i).(j)<- P(m);
end
end 

else ()
done;

g.(j).(i)<- g.(i).(j);completude.(j).(i)<- completude.(i).(j);
done;
done;
completude
;;

let gtravail = creer_graphe 6;;

completer_graph gtravail;;

existe_Null (gtravail);;

kruskal gtravail;;

let forett = create_forest gtravail;;

let listet=sort (kruskal gtravail);;

let _,l = etape_kruskal forett listet [] ;;

rev [] l;;

