(* 2. Scrutin uninominal *)
(* Question 1 *)
type candidat = string;;
type bulletin = candidat;;
type urne = bulletin list;;
type score = int;;
type panel = candidat list;;

(* Question 2 *)
 let rec compte (candidat:candidat) (urne:urne):score=
  match urne with
  []->0
  |e::suite-> let fonction=(compte candidat suite) in 
              if candidat = e then 1 + fonction else fonction;;

(* Question 3 *)
type resultat = (candidat * score) list;;

let rec depouiller (lc:panel) (u : urne) : resultat =
  match lc with 
  | [] -> []
  | pr :: [] -> (pr,compte pr u ) :: []
  | pr :: fin -> (pr,compte pr u ):: (depouiller fin u);;

(* Question 4 *)
let rec union (r1:resultat) (r2:resultat) : resultat =
  match r1,r2 with
  |[],_|_,[] -> []
  |(c1,s1)::fin1,(c2,s2)::fin2 -> (c1,s1+s2)::(union fin1 fin2);;

(* Question 5 *)
let rec max_depouiller (r:resultat):candidat*score=
  match r with
  []->("",-1)
  |(cand,scr)::fin->let (cand2,scr2)= (max_depouiller fin) in 
  if scr > scr2 then (cand,scr) else (cand2,scr2);;

(* Question 6 *)
let vainqueur_scrutin_uninominal (u:urne) (lc:panel):candidat=
  fst (max_depouiller (depouiller lc u));; 

(* Question 7 *)
let rec suppr_elem (list:'a list) (elt:'a):'a list=
  match list with
  []->[]
  |pr::fin-> if pr = elt then fin else (pr::(suppr_elem fin elt));;

let deux_premiers (u:urne) (lc:panel) : (candidat*score)*(candidat*score)=
  let dep = (depouiller lc u) in
    let win = (max_depouiller dep) in 
      win,(max_depouiller (suppr_elem dep win));;

let u3 = ["Alice"; "Bob"; "Charlie"; "Dave"; "Eve"; "Bob"; "Charlie"; "Charlie"; "Eve"; "Alice"; "Bob"; "Eve"; "Dave"; "Alice"; "Charlie"; "Charlie"; "Eve"; "Bob"; "Dave"; "Alice"; "Charlie"; "Charlie"; "Eve"; "Bob"; "Bob"; "Charlie"; "Dave"; "Eve"; "Bob"; "Charlie"; "Charlie"; "Eve"; "Alice"; "Bob"; "Eve"; "Dave"; "Alice"; "Charlie"; "Charlie"; "Eve"; "Bob"; "Dave"; "Alice"; "Charlie"; "Charlie"; "Eve"; "Bob"];;
let l31 = ["Alice"; "Bob"; "Charlie"; "Dave"; "Eve"];;

(* Question 8 *)

(*En général, dans un scrutin où chaque électeur ne peut voter que pour un seul candidat,
  la présence de plusieurs candidatures peut avoir des conséquences imprévues.
  Par exemple, dans l'exemple que vous avez donné, la candidature de Keny peut
  affecter la performance électorale de Stan en empêchant ce dernier de se qualifier
  pour le second tour. À la place, Kyle se qualifie avec Eric, alors que
  sans la candidature de Keny, Stan aurait probablement eu plus de chances
  de se qualifier pour le second tour avec Eric.
  Cela souligne la manière dont les candidatures minoritaires peuvent parfois
  impacter le résultat final d'un scrutin uninominal.
   
  Lors des élections présidentielles de l'an dernier, une situation similaire
  s'est produite : la présence de candidatures du Parti Communiste Français (PCF)
  et d'Europe Écologie Les Verts (EELV) a empêché l'Union Populaire
  de se qualifier pour le second tour. Autrement dit, la division des voix
  entre plusieurs candidatures a nui à l'Union Populaire et a favorisé la qualification
  d'autres candidats pour le second tour.*)

(* Question 9 *)

(*Le problème dans le scrutin uninominal est que chaque électeur ne peut voter que
  pour un seul candidat, ce qui peut conduire à des situations où plusieurs candidats
  présentant des idées similaires se présentent à l'élection, divisant ainsi les voix
  de l'électorat qui partage ces idées. Cette division des voix peut favoriser
  la qualification d'autres candidats qui ont une base électorale plus large mais
  qui ne représentent pas nécessairement les idées de la majorité. Ainsi, le scrutin
  uninominal peut parfois aboutir à des résultats qui ne reflètent pas fidèlement
  la volonté de l'électorat. *)

(* Question 10 *)

(*6 exposant 12 = 2 176 782 336

  On beaucoup plus de possibilités avec le jugement majoritaire
  qu'avec les 13 possibilités du scrutin uninomiale.
*)

(* Question 11 *)

type mention = Arejeter | Insuffisant | Passable | Assezbien | Bien | Tresbien;;

type bulletin_jm = mention list;;

type urne_jm = bulletin_jm list;;

(* Question 12 *)

let rec depouille_jm (u:urne_jm):mention list list=
  match u with
  []->[]
  |[]::suite->[]
  |_->List.map List.hd u::(depouille_jm (List.map List.tl u));;

(* Question 13 *)

let tri_mentions (ms:mention list list):mention list list=
  List.map (List.sort compare) ms;;

(* Question 14 *)

let mediane (l:'a list):'a=
  match l with
  []->Arejeter
  |l->let n = List.length l in List.nth l (n/2) ;;

(* Question 15 *)

let meilleure_mediane (u : mention list list) : mention =
  let l = tri_mentions u in
   let lt = (List.map (mediane) l) in
    let ltt = (List.sort compare) lt in
    List.nth ltt ((List.length ltt) - 1);;

(* Question 16 *)

let supprime_perdants (l: mention list list) : mention list list=
  let meil = meilleure_mediane l and l =  tri_mentions l in
    List.map (fun x -> if mediane x < meil then [] else x) l;;

let ms_triee:mention list list =
    [[Assezbien; Tresbien; Tresbien];
    [Arejeter; Assezbien; Assezbien];
    [Arejeter; Arejeter; Arejeter];
    [Passable; Tresbien; Tresbien]];;

(* Question 17 *)

let rec supprime_mention (l:mention list) (x:mention):mention list=
  match l with
  []->[]
  |m::suite->if m=x then suite else m::(supprime_mention suite x);;

let supprime_meilleure_mediane (l:mention list list):mention list list=
 let meil = meilleure_mediane l in
  List.map (fun x ->supprime_mention x meil) l;;

let xd = [[Assezbien; Tresbien; Tresbien]; []; []; [Passable; Tresbien; Tresbien]];;

(* Question 18 *)

let trouver_indice_candidat (l:mention list list) : int =
  let li = List.init (List.length l) (fun x -> x) in
    let li = List.map (fun x -> if List.nth l x = [] then -1 else x) li in
      List.find (fun x -> x <> -1) li;;

let rec vainqueur_jm (l:mention list list) (p:panel):candidat = 
  let l = supprime_perdants l in 
    let l2 = List.filter (fun x -> x <> []) l in
      let n = List.length l2 in
        match n with
        |0 -> ""
        |1 -> let i = trouver_indice_candidat l in List.nth p i
        |_ -> vainqueur_jm (supprime_meilleure_mediane l) p;;

(* Question 19 *)

let trouve_vainqueur_jm (u:urne_jm) (p:panel):candidat = 
  let l = depouille_jm u in
    vainqueur_jm l p;;

(* Question 20 *)

(* 
Le jugement majoritaire résout le problème du vote utile en demandant aux électeurs 
d'évaluer chaque candidat sur une échelle, ce qui permet une meilleure représentativité 
des préférences. Cependant, un autre point critique de ce système est sa complexité. 
Comparé à d'autres méthodes de vote, le jugement majoritaire peut être plus difficile 
à comprendre et à mettre en œuvre pour les électeurs et les responsables électoraux, 
ce qui pourrait entraîner des erreurs de vote ou une participation moindre.
 *)

(* Question 21 *)

type ville = string * resultat;; 
type region = Reg of string | Dpt of string;;
type arbre = Bv of ville | N of region * arbre list;;

(* Question 22 *)

let rec trouve_bv (a:arbre list) (v:string) : resultat = 
  match a with
  []-> failwith "empty list error"
  |N(_,x) -> trouve_bv x v
  |(Bv (s,r))::suite -> if s = v then r else trouve_bv suite v

let rec trouve_bv (a:arbre list) (ville:string) : resultat = 
  match a with
  |[] -> []
  |Bv(v,res)::suite -> if v = ville then res else trouve_bv suite ville
  |(N(_,l))::suite -> (trouve_bv l ville)@(trouve_bv suite ville);;


(* Question 23 *)

let gfv:resultat = let g=trouve_bv [ara] "Grenoble" and f = trouve_bv [ara] "Fontaine" and v = trouve_bv [ara] "Valence" in union (union g f) v;;