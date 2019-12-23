-------------------------- TEST DE ARBRE_GENEALOGIQUE --------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Arbre_Genealogique;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


procedure Test_Arbre_Genealogique is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    Capacite : constant Integer := 5;

    package M_AG is new Arbre_Genealogique(Capacite);
    use M_AG;

    -- PROCEDURES DE TESTS --

    M_Informations : constant array (1..8) of M_R.T_Informations
            := ((To_Unbounded_String("aaa"),To_Unbounded_String("zzz"),M_R.HOMME
                ,(01,01,1999),To_Unbounded_String("AAA"),(02,01,2000)),
                (To_Unbounded_String("ddd"),To_Unbounded_String("www"),M_R.HOMME
                 ,(01,01,1968),To_Unbounded_String("ddd"),(02,01,2000)),
                (To_Unbounded_String("fff"),To_Unbounded_String("uuu"),M_R.FEMME
                 ,(01,01,1972),To_Unbounded_String("fff"),(02,01,2000)),
                (To_Unbounded_String("ggg"),To_Unbounded_String("ttt"),M_R.HOMME
                 ,(01,01,1942),To_Unbounded_String("ggg"),(02,01,2000)),
                (To_Unbounded_String("bbb"),To_Unbounded_String("yyy"),M_R.FEMME
                 ,(01,01,1940),To_Unbounded_String("bbb"),(02,01,2000)),
                (To_Unbounded_String("ccc"),To_Unbounded_String("xxx"),M_R.FEMME
                 ,(01,01,1951),To_Unbounded_String("ccc"),(02,01,2000)),
                (To_Unbounded_String("eee"),To_Unbounded_String("vvv"),M_R.HOMME
                 ,(30,06,1949),To_Unbounded_String("eee"),(02,01,2000)),
                (To_Unbounded_String("hhh"),To_Unbounded_String("sss"),M_R.FEMME
                 ,(19,06,1923),To_Unbounded_String("hhh"),(20,06,2000)));

    M_Informations_Modifie : constant array (1..3) of M_R.T_Informations
            := ((To_Unbounded_String("aaa"),To_Unbounded_String("zzz"),M_R.HOMME
                ,(01,01,1943),To_Unbounded_String("aaa"),(02,01,2000)),
                (To_Unbounded_String("eee"),To_Unbounded_String("vvv"),M_R.FEMME
                 ,(01,01,1921),To_Unbounded_String("ddd"),(02,01,2000)),
                (To_Unbounded_String("eee"),To_Unbounded_String("vvv"),M_R.HOMME
                 ,(01,01,1999),To_Unbounded_String("AAA"),(02,01,2000)));

    M_Parents : constant array (1..7) of T_Parent
            := (PERE, MERE, PERE, MERE, MERE, PERE, MERE);

    M_Id : constant array (1..7) of Integer := (1, 1, 2, 2, 3, 3, 4);


    procedure Tester_AG is

        Arbre : T_AG;

    begin

        Put_Line("Test Ajouter");
        Put_Line("Ajout 1ère personne :");
        Initialiser(Arbre, 1, M_Informations(1));
        pragma Assert(Taille(Arbre, 1) = 1);
        Afficher(Arbre, 1);
        for i in 1..7 loop
            Put("Ajout ");
            Put(i+1, 1);
            Put_Line("ème personne :");
            Ajouter(Arbre, M_Id(i), M_Parents(i), M_Informations(i+1));
            pragma Assert(Taille(Arbre, 1) = i + 1);
            Afficher(Arbre, 1);
        end loop;
        New_Line;

        Put_Line("Test Ajouter_Frere à 3");
        Ajouter_Frere(Arbre, 3, M_Informations_Modifie(1));
        Afficher(Arbre, 1);
        Afficher(Arbre, 2);
        New_Line;

        Put_Line("Test Ajouter_Demi_Frere à 1 avec père en commun");
        Ajouter_Demi_Frere(Arbre, 1, M_Informations_Modifie(2), PERE);
        Afficher(Arbre, 1);
        Afficher(Arbre, 3);
        New_Line;
        Put_Line("Test Ajouter_Demi_Frere à 2 avec mère en commun");
        Ajouter_Demi_Frere(Arbre, 2, M_Informations_Modifie(3), MERE);
        Afficher(Arbre, 1);
        Afficher(Arbre, 4);
        New_Line;

        Put_Line("Test Modifier");
        Put_Line("id 4 modifié :");
        Modifier(Arbre, 4, M_Informations_Modifie(1));
        Afficher(Arbre, 1);
        Put_Line("id 8 modifié :");
        Modifier(Arbre, 8, M_Informations_Modifie(2));
        Afficher(Arbre, 1);
        Put_Line("id 1 modifié :");
        Modifier(Arbre, 1, M_Informations_Modifie(3));
        Afficher(Arbre, 1);
        pragma Assert(Taille(Arbre, 1) = 8);
        New_Line;

        Put_Line("Test Afficher_Ancetres_Génération");
        Put_Line("Affichage des ancêtres de 2ème génération de 1");
        Afficher_Ancetres_Generation(Arbre, 1, 2);
        Put_Line("Affichage des ancêtres de 1ère génération de 2");
        Afficher_Ancetres_Generation(Arbre, 2, 1);
        New_Line;

        Put_Line("Test Mono_Parent");
        Put_Line("Affichage des personnes n'ayant qu'un parent connu");
        Mono_Parent(Arbre, 1);
        New_Line;

        Put_Line("Test Deux_Parents");
        Put_Line("Affichage des personnes ayant deux parents connus");
        Deux_Parents(Arbre, 1);
        New_Line;

        Put_Line("Test Sans_Parent");
        Put_Line("Affichage des personnes n'ayant aucun parent connu");
        Sans_Parent(Arbre, 1);
        New_Line;

        Put_Line("Test Afficher n Generations");
        Put_Line("Affichage de deux générations à partir de 1");
        Afficher(Arbre, 1, 2, 1);
        Put_Line("Affichage d'une génération à partir de 4");
        Afficher(Arbre, 1, 1, 4);
        New_Line;

        Put_Line("Test Afficher_Homonymes");
        Put_Line("Affichage des ancêtres homonymes de 3 et 4");
        Afficher_Homonymes(Arbre, 3, 4);
        New_Line;

        Put_Line("Test Supprimer");
        Put_Line("Suppression de id 3");
        Supprimer(Arbre, 3, 1);
        Afficher(Arbre, 1);
        pragma Assert(Taille(Arbre, 1) = 5);
        Put_Line("Suppression de id 4");
        Supprimer(Arbre, 4, 1);
        Afficher(Arbre, 1);
        pragma Assert(Taille(Arbre, 1) = 3);
        New_Line;

        Put_Line("Afficher les informations détaillées de 5 :");
        Afficher_Informations_Detaillee (Arbre, 5);

        Put_Line("Detruire l'arbre");

        Detruire(Arbre);
        New_Line;

    end Tester_AG;

    -- EXECUTION DES PROGRAMMES DE TEST --

begin
    Put_Line ("Test du module Arbre_Genealogique");
    Tester_AG;
    Put_Line("Tous les tests ont été passés avec succès !");
end Test_Arbre_Genealogique;
