
(*La fonction combinaisons forme les triplets (a,b,r) à partir 
  de valeurs de départ avec "(a,b)" étant des combinaisons 
 (tout les sous ensembles possibles à deux éléments) et "r" 
  une liste avec le reste de valeurs   *)
let rec combinaisons l=
  let rec souslst l a b=
    List.filter (fun x->x!=a&&x!=b) l 
  in
  let rec aux_cmb l l1 l2 =
    match l,l1 with
        _,_::[]->[]
      |_::[],h::t->aux_cmb t t l2
      |x::y::t,_ ->(x,y,souslst l2 x y)::(aux_cmb (x::t) l1 l2 )
      |_,_->[]
  in aux_cmb l l l;;

(*Type somme expression*)
type  expression=Valeur of int
                |Mult of  expression* expression
                |Moins of expression* expression
                |Plus of  expression* expression
                |Div of expression* expression;;
(*type produit nombre *)
type nombre ={ valeur : int; expr : expression };;

(*les fonctions correspondants aux applications des operateurs binaires*)

let addition n1 n2 l=
  [[{valeur=n1.valeur+n2.valeur; expr=Plus(n1.expr,n2.expr)}]]@[l];;

let multiplication n1 n2 l=
  [[{valeur=n1.valeur*n2.valeur; expr=Mult(n1.expr,n2.expr)}]]@[l];;

let soustraction n1 n2 l=
  [[{valeur=n1.valeur-n2.valeur; expr=Moins(n1.expr,n2.expr)}]]@[l];;

let division n1 n2 l=
  [[{valeur=n1.valeur/n2.valeur; expr=Div(n1.expr,n2.expr)}]]@[l];;


(*verifie si l'expression (f n1 n2) a déjà été calculé avec les operandes en cours *)
let estDans n1 n2 f l =
  List.exists (fun x->x.valeur=(f n1.valeur n2.valeur)) l;;


(*convertit un entier en type enregistrement "nombre" *) 
let to_n v=
  {valeur=v; expr=Valeur v};;

(*indique que le nombre a ete trouve*)
exception Trouve of nombre;;

(*recupere le nombre(qui va être remonté avec l'exception)
  dans la liste des expressions calculés 
  par la fonction application, appelé seulement si
  l'exception Trouve a été levée *)
let rec getRes l n =
  match l with 
      []->to_n n
    |h::t->if h.valeur=n then h else getRes t n;;

(*Application de 4 operateurs aux nombres a b,
  renvoie une liste de listes conteant des expressions valides plus le
  reste de nombres a combiner (contenu dand "c") *)
(*Concatene les expressions valides une à une grace aux appeles recursives
tout en gardant une liste "l" des expressions déjà calculés pour ne pas generer les doublons*)

let rec appl a b c l n =
  let concat li=(List.hd(li))@(if (List.length(List.tl li)!=0)
                               then
                                 List.hd(List.tl li) 
                               else
                                 []) 
  in 
    if not (estDans a b (+) l) then
      concat (addition a b c) :: appl a b c (List.hd (addition a b c)@l) n
    else
    if a.valeur !=1 && b.valeur !=1 &&not (estDans a b ( * ) l) 
    then
      concat  (multiplication a b c):: appl a b c (List.hd(multiplication a b c)@l) n
    else
    if a.valeur > b.valeur && not (estDans a b (-) l) 
    then
      concat(soustraction a b c):: appl a b c (List.hd(soustraction a b c)@l) n
    else
    if b.valeur > a.valeur && not (estDans b a (-) l) 
    then
      concat(soustraction b a c):: appl a b c (List.hd(soustraction b a c)@l) n
    else
    if b.valeur!=1 && (a.valeur mod b.valeur = 0) && not (estDans a b (/) l) 
    then
      concat(division a b c):: appl a b c (List.hd(division a b c)@l) n
    else
    if a.valeur!=1 && (b.valeur mod a.valeur = 0) && not (estDans b a (/) l)  
    then
      concat(division b a c):: appl a b c (List.hd(division b a c)@l) n
    else
      if (estDans (to_n n) (to_n 0) (+) l) (*la valeur cherché a été trouve*)
    then 
      raise (Trouve (getRes l n)) (*arreter la construction et remonter la valeur trouvé*)
    else
      [];;


(* combiner fait une première application de 4 operateurs binaires
   à chaque triplet de la liste de combinaisons  *)
let rec combiner l n =
  match l with
      []->[]
    |(a,b,r)::t->(appl (to_n a) (to_n b) (List.map to_n r) [] n)@combiner t n;;

(* renvoie le nombre le plus proche de l'entier cible *)
let max_n n1 n2 n =
  if (abs (n-n1.valeur))> (abs (n-n2.valeur))then
    n2
  else
    n1;;

(* renvoie le nombre le plus proche parmi une liste de nombres *)
let rec meilleure_approximation l n=
  let f n1 n2=max_n n1 n2 n in
    match l with
      []->to_n 0
     |h::[]->h
     |h::t ->f h (meilleure_approximation t n);;

(*renvoie la liste contenant l'entier partiel le plus proche et son reste
  associé *) 
let  aprox l  n =
  let rec apr_aux l l1 n= 
    match l with 
        []-> []
      |h::t -> if (meilleure_approximation (List.map List.hd l1) n).valeur=(List.hd h).valeur
          then
            [h]
          else
            apr_aux t l1 n
  in apr_aux l l n;;


(*convertit un nombre en un couple int*string*)
let nombre_to_string n=
  let rec nbaux e=
    match e with
        Valeur v->string_of_int v
      |Mult (e,e2)->"("^(nbaux e)^"*"^(nbaux e2)^")"
      |Plus (e,e2)->"("^(nbaux e)^"+"^(nbaux e2)^")"
      |Moins(e,e2)->"("^(nbaux e)^"-"^(nbaux e2)^")"
      |Div (e,e2)->"("^(nbaux e)^"/"^(nbaux e2)^")"
  in
  n.valeur,nbaux n.expr;;

(*fonction principale qui sert a combiner les valeurs retournés
  par une premiere application de quatre operateurs qui ont 
la forme [ [(a op b)] [c;d;e...]], plus precisement elle construit 
toutes les valeurs possibles en prenant l'expression en tete de la liste l 
et en lui applicant "op" avec le premier élément du reste : [[((a op b) op c)] [d;e...]
jusqu'à ce qu'il ne reste plus que deux element a combiner où on renvoie l'expression finale 
(qui va être concatené aux autres expressions finales dans la fonction englobante "reduire" *)
(*Le boolean p sert à indiquer si on unilise la strategie "guidée par l'entier cible" 
ou si on calcule toute les expressions possibles (pour la partie 1) *) 
let rec reduire l n p=
  let rec reduire_aux l1=
    match l1 with
      []->[]
        (*Il reste que deux element à combiner 
          on applique une derniere fois les 4 opperateurs 
          pour retourner la valeur finale *)
       |a::b::[] when p -> aprox (appl a b [] [] n) n
       |a::b::[]->(appl a b [] [] n)
                (*s'il il reste plus de 3 éléments
                  on reapelle la fonction reduire
                  avec une nouvelle liste (en enpruntant un nombre 
                  de la liste de reste (ici "t") et en applicant 
                  les 4 operateurs à (a,b,t) ) *)
       |a::b::t when p->reduire ((aprox (appl a b t [] n) n)) n p
       |a::b::t ->reduire ( (appl a b t [] n) ) n p        
       |_::[]->[]
  in
    match l with
        []->[]
       |h::t->(reduire_aux h)@(reduire t n p);;




(*combinaison de toute les fonction qui servent à trouver l'expressions 
de l'entier cible n. Combiner, reduire sont susceptibles de lever une exception
qui indique que le nombre a été trouve sans considèrer toutes les expressions.
On gère cette exception on recuperant la valeur remontée *)
let la_meilleure_combinaison l n=
  try nombre_to_string(meilleure_approximation
         (List.map List.hd 
            (reduire(combiner 
                         (combinaisons l)n)n true))n )
  with Trouve i ->nombre_to_string i;;

(*Lire les valeurs du clavier et les retourner dans une liste *)
let rec lire_valeurs () =
  match int_of_string(read_line ()) with
    x when x<=0->[]
   |x->x::lire_valeurs() ;;


let menu=("\n\nChoisissez une étape:
1-Generer les combinaisons possibles
2-Meilleure aproximation de l'entier cible
3-Formule qui s’évalue en le nombre recherché
Autre chose pour s'arreter\n");;

(*chaque fonction partie(i) et partie(i)_to_string correspond 
a la generation des valeurs requises suivant le type du calcul à effectuer
Le string construit est ensuite affiché dans la fonction jeu() *)

(*Construction de toutes les combinaisons possibles.*)
let partie1 l =
  List.map nombre_to_string (List.map List.hd (reduire(combiner(combinaisons l)(-1))(-1) false));;(*l'entier cible n'est pas utilisé, on mets une valeur quelconque *)

let partie1_to_string ()=
  let rec aux l=
    match l with
      []->""
     |(i,s)::t->"Valeur="^string_of_int(i)^" expression:"^s^"\n"^ aux t
  in
  (aux (partie1 (lire_valeurs(print_string("Donnez de 3 à maximum 8-10 valeurs:\nPour arreter tapez une valeur negative\n"))) ));;

(*Meilleure approximation *)
let partie2 () =
 la_meilleure_combinaison (lire_valeurs(print_string("Donnez les valeurs de départ\nPour arreter tapez une valeur negative\n")))
    (int_of_string(read_line (print_string("Donnez l'entier cible\n"))));;

let partie2_to_string ()=
  let aux cp =
  "On a trouvé:"^string_of_int(fst cp)^
    "\navec l'expression="^(snd cp)^"\n"
  in aux (partie2());;

(* Formule qui s’évalue en le nombre recherché *)
let partie3 n =la_meilleure_combinaison
                 (lire_valeurs(print_string("Donnez les valeurs de départ\nPour arreter tapez une valeur negative\n"))) n  ;;

let partie3_to_string ()=
  let n=(int_of_string(read_line (print_string("Donnez l'entier cible\n"))))
  in
  let cp=partie3 n
  in
  if n=(fst cp)
  then
    "Réussi: "^string_of_int(fst(cp))^" expression= "^(snd(cp))
  else
    "Erreur: l'expression exacte de ["^
      string_of_int(n)^"] n'a pas été trouvé\nLe mieux qu'on a pu faire:\nValeur:"^
        string_of_int(fst(cp))^" expression="^(snd(cp));;



(*Affichage du menu a chaque (re)appel, redirige ensuite vers 
  la partie correspondante suivant le choix de l'utilisateur *)
let rec jeu()=
  match int_of_string(read_line (print_string(menu))) with
    x when x=1->jeu(print_string(partie1_to_string()))
   |x when x=2->jeu(print_string(partie2_to_string()))
   |x when x=3->jeu(print_string(partie3_to_string()))
   |x->print_string("Au revoir\n");;
  

jeu();;


