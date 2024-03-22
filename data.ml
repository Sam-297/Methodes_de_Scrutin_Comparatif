(*3 candidats*)
let lc1:panel = ["Eric";"Kyle";"Stan"];;
let u1:urne = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";"Eric";"Eric";"Stan";"Stan"];;

(*4 candidats*)
let lc2:panel = ["Eric";"Kyle";"Stan";"Keny"];;
let u2:urne = ["Keny";"Kyle";"Keny";"Kyle";"Kyle";"Keny";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";"Eric";"Eric";"Stan";"Stan"];;

(* Urne jugement majoritaire *)
let (b1:bulletin_jm) = [Tresbien;Assezbien;Arejeter;Passable];;
let (b2:bulletin_jm) = [Assezbien;Assezbien;Arejeter;Tresbien];;
let (b3:bulletin_jm) = [Tresbien;Arejeter;Arejeter;Tresbien];;

type urne_jm = bulletin_jm list;;

let (ujm1:urne_jm) = [b1;b2;b3];;
let (ujm2:urne_jm) =
  [[Bien; Assezbien; Insuffisant; Bien]; [Bien; Bien; Passable; Bien];
   [Assezbien; Assezbien; Insuffisant; Tresbien];
   [Arejeter; Assezbien; Insuffisant; Bien];
   [Assezbien; Arejeter; Assezbien; Bien];
   [Arejeter; Assezbien; Tresbien; Tresbien];
   [Assezbien; Arejeter; Insuffisant; Bien];
   [Passable; Passable; Insuffisant; Bien];
   [Insuffisant; Bien; Tresbien; Bien];
   [Arejeter; Assezbien; Passable; Tresbien];
   [Bien; Assezbien; Assezbien; Bien]; [Tresbien; Tresbien; Tresbien; Bien];
   [Passable; Tresbien; Insuffisant; Tresbien];
   [Insuffisant; Bien; Assezbien; Bien];
   [Arejeter; Passable; Insuffisant; Tresbien];
   [Arejeter; Tresbien; Assezbien; Tresbien];
   [Insuffisant; Passable; Bien; Tresbien];
   [Assezbien; Assezbien; Passable; Tresbien];
   [Passable; Insuffisant; Bien; Bien];
   [Assezbien; Insuffisant; Passable; Tresbien];
   [Tresbien; Arejeter; Insuffisant; Tresbien];
   [Passable; Arejeter; Bien; Tresbien];
   [Assezbien; Bien; Passable; Tresbien];
   [Tresbien; Tresbien; Assezbien; Tresbien];
   [Tresbien; Arejeter; Assezbien; Tresbien]; [Bien; Bien; Passable; Bien];
   [Tresbien; Arejeter; Passable; Bien];
   [Assezbien; Bien; Passable; Tresbien]; [Tresbien; Bien; Tresbien; Bien];
   [Arejeter; Arejeter; Tresbien; Bien];
   [Assezbien; Tresbien; Assezbien; Tresbien];
   [Passable; Tresbien; Bien; Tresbien];
   [Insuffisant; Bien; Arejeter; Tresbien];
   [Tresbien; Tresbien; Arejeter; Bien]; [Arejeter; Bien; Bien; Tresbien];
   [Arejeter; Assezbien; Assezbien; Tresbien];
   [Arejeter; Bien; Tresbien; Tresbien];
   [Passable; Bien; Arejeter; Tresbien];
   [Insuffisant; Tresbien; Insuffisant; Tresbien];
   [Assezbien; Bien; Insuffisant; Tresbien];
   [Passable; Insuffisant; Insuffisant; Tresbien];
   [Assezbien; Insuffisant; Passable; Tresbien];
   [Assezbien; Assezbien; Bien; Bien]; [Assezbien; Passable; Tresbien; Bien];
   [Insuffisant; Arejeter; Bien; Tresbien];
   [Assezbien; Bien; Passable; Tresbien];
   [Bien; Insuffisant; Arejeter; Tresbien]; [Passable; Bien; Tresbien; Bien];
   [Assezbien; Assezbien; Tresbien; Tresbien];
   [Tresbien; Bien; Tresbien; Tresbien];
   [Tresbien; Assezbien; Tresbien; Bien];
   [Arejeter; Assezbien; Insuffisant; Bien];
   [Assezbien; Insuffisant; Tresbien; Tresbien];
   [Arejeter; Bien; Passable; Tresbien]; [Assezbien; Arejeter; Bien; Bien];
   [Arejeter; Assezbien; Insuffisant; Bien];
   [Arejeter; Passable; Passable; Tresbien];
   [Assezbien; Bien; Tresbien; Tresbien];
   [Tresbien; Arejeter; Bien; Tresbien];
   [Assezbien; Tresbien; Bien; Tresbien];
   [Passable; Arejeter; Arejeter; Tresbien];
   [Arejeter; Passable; Insuffisant; Bien];
   [Tresbien; Passable; Assezbien; Tresbien];
   [Assezbien; Insuffisant; Arejeter; Bien];
   [Insuffisant; Tresbien; Assezbien; Tresbien];
   [Insuffisant; Bien; Insuffisant; Bien]; [Tresbien; Tresbien; Bien; Bien];
   [Insuffisant; Insuffisant; Bien; Bien];
   [Passable; Arejeter; Assezbien; Bien];
   [Tresbien; Tresbien; Insuffisant; Bien];
   [Tresbien; Passable; Tresbien; Bien];
   [Assezbien; Tresbien; Passable; Bien]; [Bien; Assezbien; Assezbien; Bien];
   [Insuffisant; Arejeter; Passable; Tresbien];
   [Tresbien; Bien; Arejeter; Bien];
   [Arejeter; Arejeter; Passable; Tresbien];
   [Arejeter; Bien; Assezbien; Bien]; [Arejeter; Passable; Assezbien; Bien];
   [Tresbien; Tresbien; Passable; Bien]; [Bien; Insuffisant; Passable; Bien];
   [Bien; Tresbien; Bien; Tresbien]; [Tresbien; Bien; Passable; Bien];
   [Passable; Assezbien; Insuffisant; Tresbien];
   [Bien; Arejeter; Insuffisant; Tresbien];
   [Assezbien; Passable; Assezbien; Bien];
   [Assezbien; Arejeter; Tresbien; Bien];
   [Tresbien; Insuffisant; Arejeter; Tresbien];
   [Arejeter; Insuffisant; Arejeter; Bien];
   [Tresbien; Bien; Passable; Tresbien];
   [Assezbien; Arejeter; Arejeter; Tresbien];
   [Insuffisant; Arejeter; Insuffisant; Bien];
   [Assezbien; Insuffisant; Arejeter; Bien];
   [Assezbien; Tresbien; Arejeter; Tresbien];
   [Passable; Arejeter; Tresbien; Bien];
   [Insuffisant; Arejeter; Insuffisant; Bien];
   [Insuffisant; Passable; Tresbien; Tresbien];
   [Passable; Arejeter; Arejeter; Tresbien];
   [Arejeter; Arejeter; Bien; Tresbien]; [Arejeter; Insuffisant; Bien; Bien];
   [Arejeter; Insuffisant; Arejeter; Bien]];;

let (ujm3:urne_jm) = 
  [[Arejeter; Bien; Tresbien; Insuffisant; Bien; Assezbien];
   [Insuffisant; Bien; Bien; Insuffisant; Passable; Assezbien];
   [Assezbien; Assezbien; Tresbien; Arejeter; Assezbien; Tresbien];
   [Bien; Assezbien; Bien; Passable; Insuffisant; Passable];
   [Insuffisant; Arejeter; Bien; Bien; Arejeter; Tresbien];
   [Assezbien; Arejeter; Tresbien; Tresbien; Arejeter; Bien];
   [Arejeter; Tresbien; Bien; Passable; Passable; Insuffisant];
   [Passable; Passable; Bien; Bien; Bien; Passable];
   [Insuffisant; Tresbien; Bien; Arejeter; Tresbien; Tresbien];
   [Arejeter; Passable; Tresbien; Assezbien; Bien; Assezbien];
   [Assezbien; Passable; Bien; Assezbien; Insuffisant; Arejeter];
   [Arejeter; Insuffisant; Bien; Passable; Insuffisant; Arejeter];
   [Insuffisant; Arejeter; Bien; Bien; Arejeter; Assezbien];
   [Assezbien; Bien; Tresbien; Bien; Arejeter; Tresbien];
   [Passable; Passable; Tresbien; Assezbien; Assezbien; Bien];
   [Arejeter; Assezbien; Bien; Tresbien; Assezbien; Bien];
   [Passable; Arejeter; Tresbien; Tresbien; Assezbien; Bien];
   [Assezbien; Tresbien; Tresbien; Assezbien; Insuffisant; Assezbien];
   [Tresbien; Assezbien; Tresbien; Arejeter; Insuffisant; Bien];
   [Bien; Tresbien; Bien; Arejeter; Bien; Insuffisant];
   [Insuffisant; Assezbien; Tresbien; Insuffisant; Bien; Bien];
   [Arejeter; Insuffisant; Tresbien; Arejeter; Assezbien; Tresbien];
   [Insuffisant; Tresbien; Tresbien; Arejeter; Arejeter; Insuffisant];
   [Passable; Bien; Tresbien; Bien; Arejeter; Assezbien];
   [Insuffisant; Assezbien; Tresbien; Passable; Passable; Arejeter];
   [Assezbien; Arejeter; Bien; Bien; Assezbien; Passable];
   [Arejeter; Bien; Bien; Bien; Bien; Tresbien];
   [Tresbien; Assezbien; Bien; Tresbien; Passable; Bien];
   [Bien; Tresbien; Tresbien; Arejeter; Bien; Insuffisant];
   [Arejeter; Arejeter; Tresbien; Passable; Tresbien; Bien];
   [Arejeter; Assezbien; Bien; Insuffisant; Insuffisant; Insuffisant];
   [Assezbien; Passable; Bien; Tresbien; Bien; Assezbien];
   [Passable; Bien; Bien; Bien; Passable; Assezbien];
   [Tresbien; Arejeter; Tresbien; Assezbien; Assezbien; Bien];
   [Insuffisant; Passable; Bien; Bien; Tresbien; Bien];
   [Tresbien; Insuffisant; Bien; Bien; Arejeter; Bien];
   [Arejeter; Tresbien; Tresbien; Passable; Assezbien; Bien];
   [Arejeter; Assezbien; Bien; Assezbien; Assezbien; Arejeter];
   [Bien; Tresbien; Tresbien; Passable; Insuffisant; Bien];
   [Tresbien; Arejeter; Tresbien; Assezbien; Passable; Passable];
   [Passable; Tresbien; Tresbien; Passable; Assezbien; Assezbien];
   [Bien; Bien; Tresbien; Insuffisant; Assezbien; Passable];
   [Passable; Passable; Tresbien; Insuffisant; Passable; Tresbien];
   [Assezbien; Passable; Bien; Insuffisant; Passable; Bien];
   [Passable; Tresbien; Tresbien; Assezbien; Arejeter; Assezbien];
   [Bien; Passable; Bien; Assezbien; Arejeter; Insuffisant];
   [Assezbien; Insuffisant; Bien; Passable; Tresbien; Passable];
   [Bien; Arejeter; Bien; Insuffisant; Assezbien; Assezbien];
   [Bien; Bien; Bien; Assezbien; Tresbien; Insuffisant];
   [Assezbien; Arejeter; Bien; Passable; Assezbien; Bien];
   [Tresbien; Passable; Bien; Assezbien; Bien; Tresbien];
   [Bien; Tresbien; Tresbien; Assezbien; Bien; Passable];
   [Passable; Insuffisant; Tresbien; Bien; Insuffisant; Tresbien];
   [Bien; Assezbien; Tresbien; Tresbien; Bien; Arejeter];
   [Bien; Bien; Bien; Passable; Insuffisant; Passable];
   [Bien; Tresbien; Tresbien; Assezbien; Passable; Bien];
   [Insuffisant; Passable; Bien; Passable; Tresbien; Arejeter];
   [Bien; Insuffisant; Bien; Passable; Tresbien; Arejeter];
   [Passable; Assezbien; Tresbien; Tresbien; Assezbien; Passable];
   [Passable; Assezbien; Tresbien; Passable; Arejeter; Tresbien];
   [Insuffisant; Tresbien; Tresbien; Assezbien; Assezbien; Assezbien];
   [Bien; Tresbien; Tresbien; Bien; Assezbien; Insuffisant];
   [Arejeter; Assezbien; Bien; Assezbien; Tresbien; Arejeter];
   [Bien; Tresbien; Tresbien; Tresbien; Arejeter; Arejeter];
   [Insuffisant; Tresbien; Bien; Arejeter; Insuffisant; Insuffisant];
   [Bien; Arejeter; Tresbien; Insuffisant; Passable; Passable];
   [Insuffisant; Insuffisant; Tresbien; Passable; Arejeter; Bien];
   [Insuffisant; Insuffisant; Tresbien; Tresbien; Tresbien; Insuffisant];
   [Tresbien; Passable; Tresbien; Passable; Passable; Passable];
   [Passable; Insuffisant; Bien; Tresbien; Tresbien; Tresbien];
   [Bien; Passable; Tresbien; Passable; Passable; Tresbien];
   [Passable; Arejeter; Tresbien; Assezbien; Assezbien; Passable];
   [Bien; Assezbien; Tresbien; Insuffisant; Arejeter; Tresbien];
   [Bien; Tresbien; Tresbien; Insuffisant; Passable; Assezbien];
   [Arejeter; Assezbien; Bien; Arejeter; Bien; Assezbien];
   [Bien; Tresbien; Bien; Tresbien; Tresbien; Tresbien];
   [Passable; Arejeter; Bien; Passable; Assezbien; Tresbien];
   [Assezbien; Passable; Bien; Tresbien; Assezbien; Assezbien];
   [Insuffisant; Tresbien; Bien; Arejeter; Tresbien; Bien];
   [Insuffisant; Bien; Bien; Bien; Bien; Arejeter];
   [Passable; Bien; Bien; Insuffisant; Tresbien; Insuffisant];
   [Passable; Passable; Tresbien; Assezbien; Tresbien; Arejeter];
   [Tresbien; Assezbien; Bien; Assezbien; Insuffisant; Bien];
   [Arejeter; Passable; Tresbien; Insuffisant; Tresbien; Passable];
   [Tresbien; Passable; Bien; Arejeter; Insuffisant; Insuffisant];
   [Tresbien; Bien; Tresbien; Passable; Insuffisant; Insuffisant];
   [Tresbien; Arejeter; Tresbien; Assezbien; Passable; Assezbien];
   [Assezbien; Bien; Tresbien; Passable; Tresbien; Arejeter];
   [Bien; Arejeter; Bien; Insuffisant; Tresbien; Tresbien];
   [Insuffisant; Arejeter; Tresbien; Bien; Tresbien; Passable];
   [Bien; Insuffisant; Tresbien; Passable; Tresbien; Assezbien];
   [Assezbien; Passable; Bien; Insuffisant; Bien; Passable];
   [Insuffisant; Assezbien; Tresbien; Bien; Bien; Assezbien];
   [Bien; Tresbien; Bien; Bien; Assezbien; Tresbien];
   [Insuffisant; Bien; Tresbien; Tresbien; Passable; Arejeter];
   [Passable; Insuffisant; Tresbien; Bien; Insuffisant; Arejeter];
   [Passable; Arejeter; Bien; Arejeter; Passable; Assezbien];
   [Passable; Arejeter; Tresbien; Assezbien; Tresbien; Arejeter];
   [Arejeter; Assezbien; Bien; Passable; Tresbien; Bien];
   [Bien; Bien; Bien; Assezbien; Tresbien; Passable]];;

(* Résultat élection présidentielle 2022 - Auvergne-Rhône-Alpes *)
let ara : arbre =
N (Reg "Auvergne-Rhône-Alpes",
 [N (Dpt "Drôme",
   [Bv ("Valence",
     [("ARTHAUD", 161); ("ROUSSEL", 595); ("MACRON", 7756); ("LASSALLE", 590);
      ("LE PEN", 4679); ("ZEMMOUR", 2080); ("MÉLENCHON", 8398);
      ("HIDALGO", 519); ("JADOT", 1701); ("PÉCRESSE", 1423); ("POUTOU", 186);
      ("DUPONT-AIGNAN", 573)]);
    Bv ("Romans-sur-Isère",
     [("ARTHAUD", 181); ("ROUSSEL", 371); ("MACRON", 4030); ("LASSALLE", 334);
      ("LE PEN", 3270); ("ZEMMOUR", 1072); ("MÉLENCHON", 4108);
      ("HIDALGO", 251); ("JADOT", 850); ("PÉCRESSE", 631); ("POUTOU", 111);
      ("DUPONT-AIGNAN", 341)])]);
  N (Dpt "Isère",
   [Bv ("Meylan",
     [("ARTHAUD", 28); ("ROUSSEL", 169); ("MACRON", 4457); ("LASSALLE", 164);
      ("LE PEN", 1288); ("ZEMMOUR", 928); ("MÉLENCHON", 2198);
      ("HIDALGO", 251); ("JADOT", 906); ("PÉCRESSE", 763); ("POUTOU", 64);
      ("DUPONT-AIGNAN", 162)]);
    Bv ("Echirolles",
     [("ARTHAUD", 104); ("ROUSSEL", 506); ("MACRON", 3276); ("LASSALLE", 259);
      ("LE PEN", 2737); ("ZEMMOUR", 779); ("MÉLENCHON", 5121);
      ("HIDALGO", 223); ("JADOT", 590); ("PÉCRESSE", 360); ("POUTOU", 92);
      ("DUPONT-AIGNAN", 202)]);
    Bv ("Fontaine",
     [("ARTHAUD", 55); ("ROUSSEL", 363); ("MACRON", 2111); ("LASSALLE", 146);
      ("LE PEN", 1835); ("ZEMMOUR", 541); ("MÉLENCHON", 3113);
      ("HIDALGO", 185); ("JADOT", 493); ("PÉCRESSE", 212); ("POUTOU", 83);
      ("DUPONT-AIGNAN", 121)]);
    Bv ("Saint-Martin-d'Hères",
     [("ARTHAUD", 58); ("ROUSSEL", 436); ("MACRON", 2769); ("LASSALLE", 207);
      ("LE PEN", 2289); ("ZEMMOUR", 661); ("MÉLENCHON", 4763);
      ("HIDALGO", 242); ("JADOT", 777); ("PÉCRESSE", 300); ("POUTOU", 119);
      ("DUPONT-AIGNAN", 161)]);
    Bv ("Gières",
     [("ARTHAUD", 16); ("ROUSSEL", 66); ("MACRON", 1071); ("LASSALLE", 84);
      ("LE PEN", 641); ("ZEMMOUR", 205); ("MÉLENCHON", 844); ("HIDALGO", 96);
      ("JADOT", 301); ("PÉCRESSE", 155); ("POUTOU", 30);
      ("DUPONT-AIGNAN", 61)]);
    Bv ("Grenoble",
     [("ARTHAUD", 256); ("ROUSSEL", 1300); ("MACRON", 15968);
      ("LASSALLE", 845); ("LE PEN", 6444); ("ZEMMOUR", 3389);
      ("MÉLENCHON", 24568); ("HIDALGO", 1488); ("JADOT", 5644);
      ("PÉCRESSE", 2019); ("POUTOU", 508); ("DUPONT-AIGNAN", 661)])])]);;

let panel_2022 = ["ARTHAUD";"ROUSSEL";"MACRON";"LASSALLE";"LE PEN";"ZEMMOUR";"MÉLENCHON";"HIDALGO";"JADOT";"PÉCRESSE";"POUTOU";"DUPONT-AIGNAN"];;

  
