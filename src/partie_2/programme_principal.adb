-- PROGRAMME PRINCIPAL --
--    PARTIES 1 & 2    --

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Arbre_Genealogique;

procedure Programme_Principal is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    package M_AG is new Arbre_Genealogique(100);
    use M_AG;
    use M_AG.M_R;
    use M_AG.M_LC_Integer;
    use M_AG.M_LC_AB;

    -- PROCEDURES DE SAISIES --

    procedure Saisir_Informations (Informations : in out T_Informations;
                                   Modification : in     Boolean) is

        Sexe, Decede : Character;

    begin

        Put      ("Nom                      : ");
        Get_Line (Informations.Nom);
        Put      ("Prénom                   : ");
        Get_Line (Informations.Prenom);
        if not (Modification) then
            loop
                Put      ("Sexe (H/F)               : ");
                Get (Sexe);
                Skip_Line;
                if Sexe = 'H' or Sexe = 'h' then
                    Informations.Sexe := HOMME;
                elsif Sexe = 'F' or Sexe = 'f' then
                    Informations.Sexe := FEMME;
                else
                    Put_Line ("Charactère saisi non valide, veuillez recommencer");
                end if;
                exit when Sexe = 'H' or Sexe = 'h' or Sexe = 'F' or Sexe = 'f';
            end loop;
        else
            null;
        end if;
        Put_Line ("Date de naissance ");
        Put      ("     - Jour  (JJ)        : ");
        Get (Informations.Date_Naissance.Jour);
        Put      ("     - Mois  (MM)        : ");
        Get (Informations.Date_Naissance.Mois);
        Put      ("     - Annee (AAAA)      : ");
        Get (Informations.Date_Naissance.Annee);
        Put      ("Lieu de naissance        : ");
        Get_Line (Informations.Lieu_Naissance);
        Skip_Line;
        loop
            Put      ("Personne décédée ? (O/N) : ");
            Get (Decede);
            Skip_Line;
            if Decede = 'O' or Decede = 'o' then
                Put_Line  ("Date de décès ");
                Put      ("     - Jour  (JJ)        : ");
                Get (Informations.Date_Deces.Jour);
                Put      ("     - Mois  (MM)        : ");
                Get (Informations.Date_Deces.Mois);
                Put      ("     - Annee (AAAA)      : ");
                Get (Informations.Date_Deces.Annee);
            elsif Decede = 'N' or Decede = 'n' then
                Informations.Date_Deces := (01,01,10000);
            else
                Put_Line ("Charactère saisi non valide, veuillez recommencer");
            end if;
            exit when Decede = 'O' or Decede = 'o' or Decede = 'N' or Decede = 'n';
        end loop;

    exception
        when Ada.IO_Exceptions.Data_Error =>
            Put_Line ("Saisie d'un caractère invalide, veuillez recommencer");
            Skip_Line;
            Saisir_Informations (Informations, Modification);
    end Saisir_Informations;

    procedure Saisir_Identifiant (Identifiant       : in out Integer;
                                  Num_Arbre_Courant : in     Integer;
                                  Arbre             : in     T_AG) is
    begin

        Put_Line ("Si vous souhaitez consulter l'arbre généalogique, tapez 0.");
        Put ("Identifiant (ou 0 pour affichage) : ");
        Get (Identifiant);
        Skip_Line;
        if Identifiant = 0 then
            M_AG.Afficher (Arbre, Num_Arbre_Courant);
            Put ("Identifiant : ");
            Get (Identifiant);
            Skip_Line;
        else
            null;
        end if;

    exception
        when Ada.IO_Exceptions.Data_Error =>
            Put_Line ("Erreur, veuillez saisir un nombre entier");
            Skip_Line;
            Saisir_Identifiant (Identifiant, Num_Arbre_Courant, Arbre);
    end Saisir_Identifiant;

    -- PROCEDURES LIEES AU CHARGEMENT DE L'EXEMPLE --

    procedure Charger_Exemple (AG                : out T_AG;
                               Num_Arbre_Courant : in  Integer) is

        -- Nom, Prenom, Sexe, Date_Naissance, Lieu_Naissance, Date_Deces
        Informations : constant array (1..18) of T_Informations
                := ((To_Unbounded_String("Brochard"),To_Unbounded_String("Hubert"),M_R.HOMME -- 1
                    ,(07,04,1999),To_Unbounded_String("Arles"),(01,01,10000)),
                    (To_Unbounded_String("Brochard"),To_Unbounded_String("Anne-Claire"),M_R.FEMME -- 2
                     ,(15,11,1968),To_Unbounded_String("Fréjus"),(01,01,10000)),
                    (To_Unbounded_String("Brochard"),To_Unbounded_String("Ernest"),M_R.HOMME -- 3
                     ,(30,03,1972),To_Unbounded_String("Bayonne"),(01,01,10000)),
                    (To_Unbounded_String("Girard"),To_Unbounded_String("Christine"),M_R.FEMME -- 4
                     ,(25,07,1940),To_Unbounded_String("Meudon"),(01,01,10000)),
                    (To_Unbounded_String("Girard"),To_Unbounded_String("Fernand"),M_R.HOMME -- 5
                     ,(14,02,1939),To_Unbounded_String("Meudon"),(01,01,10000)),
                    (To_Unbounded_String("Brochard"),To_Unbounded_String("Michelle"),M_R.FEMME -- 6
                     ,(27,01,1935),To_Unbounded_String("Sète"),(01,01,10000)),
                    (To_Unbounded_String("Brochard"),To_Unbounded_String("Serge"),M_R.HOMME -- 7
                     ,(30,06,1938),To_Unbounded_String("Sète"),(01,01,10000)),
                    (To_Unbounded_String("Regnard"),To_Unbounded_String("Géraldine"),M_R.FEMME -- 8
                     ,(19,09,1915),To_Unbounded_String("Saint-Nazaire"),(14,01,1997)),
                    (To_Unbounded_String("Regnard"),To_Unbounded_String("Alain"),M_R.HOMME  -- 9
                     ,(30,06,1912),To_Unbounded_String("La Rochelle"),(27,10,1989)),
                    (To_Unbounded_String("Girard"),To_Unbounded_String("Marcelle"),M_R.FEMME -- 10
                     ,(04,03,1921),To_Unbounded_String("Tourcoing"),(18,07,2004)),
                    (To_Unbounded_String("Girard"),To_Unbounded_String("Silvain"),M_R.HOMME -- 11
                     ,(21,12,1918),To_Unbounded_String("Tourcoing"),(05,09,1997)),
                    (To_Unbounded_String("Lebas"),To_Unbounded_String("Séraphine"),M_R.FEMME -- 12
                     ,(10,03,1908),To_Unbounded_String("Poitiers"),(02,06,1973)),
                    (To_Unbounded_String("Lebas"),To_Unbounded_String("Léon"),M_R.HOMME -- 13
                     ,(17,02,1912),To_Unbounded_String("Limoges"),(19,11,1974)),
                    (To_Unbounded_String("Brochard"),To_Unbounded_String("Rose"),M_R.FEMME -- 14
                     ,(24,04,1921),To_Unbounded_String("Besançon"),(05,12,1995)),
                    (To_Unbounded_String("Brochard"),To_Unbounded_String("Louis"),M_R.HOMME -- 15
                     ,(20,06,1917),To_Unbounded_String("Orléans"),(12,08,1998)),
                    (To_Unbounded_String("Bousquet"),To_Unbounded_String("Rosalie"),M_R.FEMME -- 16
                     ,(08,04,1896),To_Unbounded_String("Argenteuil"),(23,01,1953)),
                    (To_Unbounded_String("Baudet"),To_Unbounded_String("Florent"),M_R.HOMME -- 17
                     ,(11,12,1900),To_Unbounded_String(""),(09,02,1978)),
                    (To_Unbounded_String("Brochard"),To_Unbounded_String("Michelle"),M_R.FEMME -- 18
                     ,(02,04,1891),To_Unbounded_String("Perpignan"),(26,02,1923)));
        Parents : constant array (1..17) of T_Parent
                := (MERE,PERE,MERE,PERE,MERE,PERE,MERE,PERE,MERE,PERE,
                    MERE,PERE,MERE,PERE,MERE,PERE,MERE);
        Identifiants : constant array (1..17) of Integer
                := (1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,10,15);

    begin

        Initialiser (AG, Num_Arbre_Courant, Informations(1));
        for i in 1..17 loop
            Ajouter (AG, Identifiants(i), Parents(i), Informations(i+1));
        end loop;

    end Charger_Exemple;

    procedure Choisir_Exemple (AG                : in out T_AG;
                               Num_Arbre_Courant : in     Integer) is

        Choix        : Character;
        Informations : T_Informations;

    begin

        loop
            Put      ("Charger exemple (O/N)               : ");
            Get (Choix);
            Skip_Line;
            if Choix = 'O' or Choix = 'o' then
                Charger_Exemple (AG, Num_Arbre_Courant);
            elsif Choix = 'N' or Choix = 'n' then
                -- Ajout du premier élément --
                Put_Line ("Pour commencer, veuillez saisir les informations de");
                Put_Line ("la première personne à ajouter à l'arbre.");
                New_Line (2);
                Saisir_Informations (Informations, False);
                Initialiser (AG, Num_Arbre_Courant, Informations, True);
            else
                Put_Line ("Charactère saisi non valide, veuillez recommencer");
            end if;
            exit when Choix = 'O' or Choix = 'o' or Choix = 'N' or Choix = 'n';
        end loop;
    exception
        when M_AG.M_R.Date_Naissance_Impossible_Exception =>
            Put_Line ("La date de naissance saisie n'est pas valide");
            Put_Line ("Veuillez recommencer");
            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
            Skip_Line;
            Skip_Line;
            Choisir_Exemple (AG, Num_Arbre_Courant);
        when M_AG.M_R.Date_Deces_Impossible_Exception =>
            Put_Line ("La date de décès saisie n'est pas valide");
            Put_Line ("Veuillez recommencer");
            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
            Skip_Line;
            Skip_Line;
            Choisir_Exemple (AG, Num_Arbre_Courant);
        when M_AG.Ordre_Date_Incoherent_Exception =>
            Put_Line ("La date de décès saisie est antérieure à la date de");
            Put_Line ("naissance");
            Put_Line ("Veuillez recommencer");
            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
            Skip_Line;
            Skip_Line;
            Choisir_Exemple (AG, Num_Arbre_Courant);

    end Choisir_Exemple;

    -- VARIABLES --

    Quitter_Programme,
    Quitter,
    Quitter_Boucle      : Boolean;
    Num_Action,
    Id_Ajouter,
    Id_Ancetres,
    Id_Ancetres_Gen,
    Num_Generation,
    Id_Afficher,
    Id_Supprimer,
    Id_Ancetres_n_Gen,
    Num_Generations,
    Id_Homonyme_1,
    Id_Homonyme_2,
    Id_Modifications,
    Num_Modification,
    Id_Informations,
    Num_AG_Courant,
    Taille_Retour    : Integer := 0; -- Initialisés à 0 pour éviter les warnings
    AG                  : T_AG;
    --Parent_Ajouter      : T_Parent;
    Informations        : T_Informations;
    Choix,
    Decede,
    Mere_Ou_Pere        : Character;

    -- IMPLEMANTATION --

begin

    Quitter_Programme := False;

    New_Line (4);
    Put_Line ("Bienvenue dans le gestionnaire d'arbre généalogique");
    Put_Line ("             Tom Pillot, Quentin Thuet             ");
    New_Line (4);

    Put_Line ("Veuillez commencer par remplir un premier arbre");
    Put_Line ("Vous pouvez renseigner les informations relatives");
    Put_Line ("à un premier individu ou charger un exemple");

    Choisir_Exemple (AG, 1);


    -- Boucle principale --
    loop
        begin

            Put_Line ("                    MENU PRINCIPAL                 ");
            New_Line;
            Put_Line ("  0. Quitter le programme");
            for i in 1..Taille (AG.AB) loop
                Put ("  ");
                Put (i, 1);
                Put (". Accéder à l'arbre ");
                Put (i, 1);
                Put (" de taille ");
                Put (Taille (AG, Element(AG.Id_Racine, i)), 1);
                New_Line;
            end loop;
            Put      ("  ");
            Put      (Taille (AG.AB) + 1, 1);
            Put_Line (". Ajouter un nouvel arbre");
            New_Line;
            Put_Line ("Veuillez saisir le nombre correspondant à l'action");
            Put      ("désirée : ");
            Get      (Num_AG_Courant);
            Skip_Line;

            if Num_AG_Courant = 0 then
                Quitter_Programme := True;
            elsif Num_AG_Courant <= Taille (AG.AB) + 1 and Num_AG_Courant > 0 then

                if Num_AG_Courant = Taille (AG.AB) + 1 then
                    Put_Line ("Vous souhaitez ajouter un nouvel arbre");
                    -- Ajout du premier élément --
                    Put_Line ("Pour commencer, veuillez saisir les informations de");
                    Put_Line ("la première personne à ajouter à l'arbre.");
                    New_Line (2);
                    Saisir_Informations (Informations, False);
                    Initialiser (AG, Taille (AG.AB) + 1, Informations);
                else
                    null;
                end if;
                -- Sous boucle à l'intérieur d'un arbre --
                Quitter := False;
                loop
                    begin
                        -- Affichage du menu principal --
                        New_Line (4);
                        Put      ("  Menu principal de l'arbre" );
                        Put      (Num_AG_Courant, 1);
                        New_Line (2);
                        Put_Line ("  0.  Quitter l'arbre");
                        Put_Line ("  1.  Ajouter un parent ou un frère/soeur");
                        Put_Line ("  2.  Obtenir le nombre d'ancêtres d'un individu");
                        Put_Line ("  3.  Obtenir les identifiants des ancêtres situés");
                        Put_Line ("      à une génération donnée d'un individu");
                        Put_Line ("  4.  Afficher l'arbre généalogique");
                        Put_Line ("  5.  Supprimer un individu et ses ancêtres");
                        Put_Line ("  6.  Obtenir les identifiants des individus dont");
                        Put_Line ("      on ne connait qu'un parent");
                        Put_Line ("  7.  Obtenir les identifiants des individus dont");
                        Put_Line ("      on connait les deux parents");
                        Put_Line ("  8.  Obtenir les identifiants des individus dont");
                        Put_Line ("      on ne connait aucun parent");
                        Put_Line ("  9.  Obtenir les identifiants des ancêtres d'un");
                        Put_Line ("      individu sur un nombre donné de générations");
                        Put_Line ("  10. Obtenir les ancêtres homonymes à deux");
                        Put_Line ("      individus");
                        Put_Line ("  11. Afficher les informations détaillées sur un");
                        Put_Line ("      individu");
                        Put_Line ("  12. Modifier les informations détaillées d'un");
                        Put_Line ("      individu");
                        New_Line;
                        loop
                            Put_Line ("Veuillez saisir le nombre correspondant à l'action");
                            Put      ("désirée : ");
                            begin
                                Get (Num_Action);
                                Skip_Line;
                                if Num_Action < 0 or Num_Action > 12 then
                                    Put_Line ("Action inexistante, veuillez recommencer");
                                end if;
                            exception
                                when Ada.IO_Exceptions.Data_Error =>
                                    Put_Line ("Erreur, veuillez saisir un nombre entier");
                                    Skip_Line;
                            end;
                            New_Line (4);
                            exit when Num_Action >= 0 and Num_Action <= 12;
                        end loop;
                        case Num_Action is

                            -- AJOUTER --
                        when 1 =>
                            Put_Line ("Vous souhaitez : ");
                            Put_Line ("  1. Ajouter un frère ou une soeur");
                            Put_Line ("  2. Ajouter un demi-frère ou une demi-soeur");
                            Put_Line ("  3. Ajouter un nouveau parent");
                            loop
                                Put_Line ("Veuillez saisir le nombre entier correspondant à");
                                Put      ("l'action que vous voulez réaliser : ");
                                Get      (Choix);
                                Skip_Line;
                                New_Line (2);
                                if Choix = '1' then
                                    Put_Line ("Veuillez saisir l'identifiant de la personne à qui");
                                    Put_Line ("vous voulez ajouter un frère ou une soeur.");
                                    Saisir_Identifiant (Id_Ajouter, Num_AG_Courant, AG);
                                    New_Line;
                                    Saisir_Informations (Informations, False);
                                    Ajouter_Frere (AG, Id_Ajouter, Informations);
                                elsif Choix = '2' then
                                    Put_Line ("Veuillez saisir l'identifiant de la personne à qui");
                                    Put_Line ("vous voulez ajouter un demi-frère ou une demi-soeur.");
                                    Saisir_Identifiant (Id_Ajouter, Num_AG_Courant, AG);
                                    New_Line;
                                    Put_Line ("Veuillez saisir les informations relatives à la");
                                    Put_Line ("nouvelle personne.");
                                    Saisir_Informations (Informations, False);
                                    loop
                                        Put      ("Le parent en commun est-il la mère ou le père ? (M/P) : ");
                                        Get (Mere_Ou_Pere);
                                        Skip_Line;
                                        if Mere_Ou_Pere = 'M' or Mere_Ou_Pere = 'm' then
                                            Ajouter_Demi_Frere (AG, Id_Ajouter, Informations, MERE);
                                        elsif Mere_Ou_Pere = 'P' or Mere_Ou_Pere = 'p' then
                                            Ajouter_Demi_Frere (AG, Id_Ajouter, Informations, PERE);
                                        else
                                            Put_Line ("Charactère saisi non valide, veuillez recommencer");
                                        end if;
                                        exit when Mere_Ou_Pere = 'M' or Mere_Ou_Pere = 'm' or Mere_Ou_Pere = 'P' or Mere_Ou_Pere = 'p';
                                    end loop;
                                elsif Choix = '3' then
                                    New_Line (2);
                                    Put_Line ("Veuillez saisir l'identifiant de la personne à qui");
                                    Put_Line ("vous voulez ajouter un parent.");
                                    Saisir_Identifiant (Id_Ajouter, Num_AG_Courant, AG);
                                    New_Line;
                                    Saisir_Informations (Informations, False);
                                    if Informations.Sexe = HOMME then
                                        Ajouter (AG, Id_Ajouter, PERE, Informations);
                                    else
                                        Ajouter (AG, Id_Ajouter, MERE, Informations);
                                    end if;
                                else
                                    Put_Line ("Nombre saisi non valide, veuillez recommencer");
                                end if;
                                exit when Choix >= '1' and Choix <= '3';
                            end loop;



                            -- NOMBRE D'ANCETRES --
                        when 2 =>
                            Put_Line ("Veuillez saisir l'identifiant de la personne dont");
                            Put_Line ("vous voulez connaitre le nombre d'ancêtres connus");
                            Saisir_Identifiant (Id_Ancetres, Num_AG_Courant, AG);
                            Taille_Retour := Taille (AG, Id_Ancetres);
                            New_Line;
                            Put      ("Cette personne a ");
                            Put      (Taille_Retour - 1, 0);
                            Put_Line (" ancêtres connus.");
                            New_Line (2);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- ANCETRES A UNE GENERATION DONNEE --
                        when 3 =>
                            Put_Line ("Veuillez saisir l'identifiant de la personne dont");
                            Put_Line ("vous voulez connaitre les ancêtres");
                            Saisir_Identifiant (Id_Ancetres_Gen, Num_AG_Courant, AG);
                            loop
                                Put_Line ("Veuillez saisir la génération à laquelle vous");
                                Put_Line ("voulez connaitre les ancêtres (sous la forme");
                                Put      ("nombre entier positif) : ");
                                Get (Num_Generation);
                                Skip_Line;
                                exit when Num_Generation > 0;
                            end loop;
                            New_Line;
                            Put_Line ("Les ancêtres de cette personne à la génération");
                            Put_Line ("donnée sont : ");
                            Afficher_Ancetres_Generation (AG, Id_Ancetres_Gen, Num_Generation);
                            New_Line (2);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- AFFICHAGE --
                        when 4 =>
                            loop
                                begin
                                    Quitter_Boucle := False;
                                    Put_Line ("Veuillez saisir l'identifiant à partir duquel");
                                    Put      ("afficher l'arbre (0 pour afficher tout l'arbre) : ");
                                    Get (Id_Afficher);
                                    Skip_Line;
                                    Quitter_Boucle := True;
                                exception
                                    when Ada.IO_Exceptions.Data_Error =>
                                        Put_Line ("Erreur, veuillez saisir un nombre entier");

                                end;
                                exit when Quitter_Boucle;
                            end loop;
                            if Id_Afficher = 0 then
                                M_AG.Afficher (AG, Num_AG_Courant, id_racine => Element (AG.Id_Racine, Num_AG_Courant));
                            else
                                M_AG.Afficher (AG, Num_AG_Courant, id_racine => Id_Afficher);
                            end if;
                            New_Line (2);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- SUPPRIMER --
                        when 5 =>
                            Put_Line ("Veuillez saisir l'identifiant à supprimer");
                            Put_Line ("!!Attention, cela supprimera aussi ses ancêtres!!");
                            Saisir_Identifiant (Id_Supprimer, Num_AG_Courant, AG);
                            New_Line (2);
                            Put_Line ("Les individus suivants seront supprimés :");
                            M_AG.Afficher (AG, Num_AG_Courant, -1, Id_Supprimer);
                            loop
                                Put      ("Confirmer ? (O/N) : ");
                                Get (Choix);
                                Skip_Line;
                                if Choix = 'O' or Choix = 'o' then
                                    Taille_Retour := Taille (AG, Element (AG.Id_Racine, Num_AG_Courant));
                                    Supprimer (AG, Id_Supprimer, Num_AG_Courant);
                                    New_Line (2);
                                    Put      (Taille_Retour - Taille (AG, Element (AG.Id_Racine, Num_AG_Courant)), 1);
                                    Put_Line (" éléments supprimé(s)");
                                    Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                                    Skip_Line;
                                elsif Choix = 'N' or Choix = 'n' then
                                    New_Line (2);
                                    Put_Line ("0 élément supprimé");
                                    Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                                    Skip_Line;
                                else
                                    Put_Line ("Charactère saisi non valide, veuillez recommencer");
                                end if;
                                exit when Choix = 'O' or Choix = 'o' or Choix = 'N' or Choix = 'n';
                            end loop;

                            -- ID MONO PARENT --
                        when 6 =>
                            Put_Line ("Les individus qui n'ont qu'un parent renseigné sont :");
                            Mono_Parent (AG, Num_AG_Courant);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- ID DEUX PARENTS --
                        when 7 =>
                            Put_Line ("Les individus qui ont deux parents renseignés sont :");
                            Deux_Parents (AG, Num_AG_Courant);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- ID AUCUN PARENT --
                        when 8 =>
                            Put_Line ("Les individus qui n'ont aucun parent renseigné sont :");
                            Sans_Parent (AG, Num_AG_Courant);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- AFFICHER N GENERATIONS --
                        when 9 =>
                            Put_Line ("Veuillez saisir l'identifiant de la personne à");
                            Put_Line ("partir duquel vous voulez afficher l'arbre");
                            Saisir_Identifiant (Id_Ancetres_n_Gen, Num_AG_Courant, AG);
                            loop
                                Put_Line ("Veuillez saisir le nombre de générations sur");
                                Put_Line ("lesquelles vous voulez connaître les ancêtres");
                                Put      ("(sous la forme nombre entier positif) : ");
                                Get (Num_Generations);
                                Skip_Line;
                                exit when Num_Generations > 0;
                            end loop;
                            New_Line;
                            Put_Line ("Les ancêtres de cette personne sur le nombre");
                            Put      ("donnée de générations sont : ");
                            M_AG.Afficher (AG, Num_AG_Courant, Num_Generations, Id_Ancetres_n_Gen);
                            New_Line (2);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- AFFICHER HOMONYMES --
                        when 10 =>
                            Put_Line ("Veuillez saisir l'identifiant d'un premier individu");
                            Saisir_Identifiant (Id_Homonyme_1, Num_AG_Courant, AG);
                            Put_Line ("Veuillez saisir l'identifiant d'un second individu");
                            Saisir_Identifiant (Id_Homonyme_2, Num_AG_Courant, AG);
                            New_Line;
                            Put_Line ("Les ancêtres homonymes aux deux individus saisis sont :");
                            Afficher_Homonymes (AG, Id_Homonyme_1, Id_Homonyme_2);
                            New_Line (2);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- AFFICHER INFORMATIONS --
                        when 11 =>
                            Put_Line ("Veuillez saisir l'identifiant de l'individu dont vous");
                            Put_Line ("voulez connaître les informations détaillées");
                            Saisir_Identifiant (Id_Informations, Num_AG_Courant, AG);
                            New_Line;
                            Put_Line ("Les informations de l'individu sont : ");
                            Afficher_Informations_Detaillee (AG, Id_Informations);
                            New_Line (2);
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- MODIFIER INFORMATIONS --
                        when 12 =>
                            Put_Line ("Veuillez saisir l'identifiant de l'individu dont vous");
                            Put_Line ("voulez modifier les informations détaillées");
                            Saisir_Identifiant (Id_Modifications, Num_AG_Courant, AG);
                            New_Line;
                            Put_Line ("Actuellement, les informations de cet individus sont");
                            Put_Line ("les suivantes :");
                            Afficher_Informations_Detaillee (AG, Id_Modifications);
                            Informations := Obtenir_Informations (AG, Id_Modifications);
                            loop
                                begin
                                    Quitter_Boucle := False;
                                    -- Menu de modifications --
                                    Put_Line ("Vous souhaitez modifier ");
                                    Put_Line ("    1. le nom");
                                    Put_Line ("    2. le prénom");
                                    Put_Line ("    3. la date de naissance");
                                    Put_Line ("    4. le lieu de naissance");
                                    Put_Line ("    5. la date de décès");
                                    Put_Line ("    6. l'ensemble des informations");
                                    loop
                                        New_Line;
                                        Put_Line ("Veuillez saisir le nombre entier correspondant à");
                                        Put      ("la valeur que vous souhaitez modifier ou 0 pour ");
                                        Put      ("quitter : ");
                                        begin
                                            Get (Num_Modification);
                                            Skip_Line;
                                            if Num_Modification < 0 or Num_Modification > 7 then
                                                Put_Line ("Modification inexistante, veuillez recommencer");
                                            end if;
                                        exception
                                            when Ada.IO_Exceptions.Data_Error =>
                                                Put_Line ("Erreur, veuillez saisir un nombre entier");
                                                Skip_Line;
                                        end;
                                        exit when Num_Modification >= 0 and Num_Modification <= 7;
                                    end loop;
                                    case Num_Modification is
                                    when 0 =>
                                        Quitter_Boucle := True;
                                    when 1 =>
                                        Put      ("Nouveau nom               : ");
                                        Get_Line (Informations.Nom);
                                    when 2 =>
                                        Put      ("Nouveau prénom           : ");
                                        Get_Line (Informations.Prenom);
                                    when 3 =>
                                        Put_Line ("Nouvelle date de naissance");
                                        Put      ("     - Jour  (JJ)         : ");
                                        Get (Informations.Date_Naissance.Jour);
                                        Put      ("     - Mois  (MM)         : ");
                                        Get (Informations.Date_Naissance.Mois);
                                        Put      ("     - Annee (AAAA)       : ");
                                        Get (Informations.Date_Naissance.Annee);
                                    when 4 =>
                                        Put      ("Nouveau lieu de naissance : ");
                                        Get_Line (Informations.Lieu_Naissance);
                                        Skip_Line;
                                    when 5 =>
                                        loop
                                            Put      ("Personne décédée ? (O/N)  : ");
                                            Get (Decede);
                                            Skip_Line;
                                            if Decede = 'O' or Decede = 'o' then
                                                Put_Line  ("Nouvelle date de décès");
                                                Put      ("     - Jour  (JJ)         : ");
                                                Get (Informations.Date_Deces.Jour);
                                                Put      ("     - Mois  (MM)         : ");
                                                Get (Informations.Date_Deces.Mois);
                                                Put      ("     - Annee (AAAA)       : ");
                                                Get (Informations.Date_Deces.Annee);
                                            elsif Decede = 'N' or Decede = 'n' then
                                                Informations.Date_Deces := (01,01,10000);
                                            else
                                                Put_Line ("Charactère saisi non valide, veuillez recommencer");
                                            end if;
                                            exit when Decede = 'O' or Decede = 'o' or Decede = 'N' or Decede = 'n';
                                        end loop;
                                    when 6 =>
                                        Put_Line ("Nouvelles informations : ");
                                        Saisir_Informations (Informations, True);
                                    when others =>
                                        null;
                                    end case;
                                exception
                                    when Ada.IO_Exceptions.Data_Error =>
                                        Put_Line ("Saisie d'un caractère invalide, veuillez recommencer");
                                        Skip_Line;
                                end;
                                New_Line;
                                Modifier (AG, Id_Modifications, Informations);
                                Put_Line ("Les informations de l'individu sont maintenant : ");
                                Afficher_Informations_Detaillee (AG, Id_Modifications);
                                exit when Quitter_Boucle;
                            end loop;

                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;

                            -- QUITTER --
                        when 0 =>
                            Quitter := True;
                        when others =>
                            null;
                        end case;

                    exception
                        when ID_Absent_Exception =>
                            Put_Line ("L'identifiant saisi n'existe pas, ou vous voulez");
                            Put_Line ("accéder à un individu qui n'existe pas");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when M_AG.M_TH.Cle_Absente_Exception =>
                            Put_Line ("L'identifiant saisi n'existe pas");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when M_AG.Date_Naissance_Impossible_Exception =>
                            Put_Line ("La date de naissance saisie n'est pas valide");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when M_AG.M_R.Date_Deces_Impossible_Exception =>
                            Put_Line ("La date de décès saisie n'est pas valide");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when M_AG.Ordre_Date_Incoherent_Exception =>
                            Put_Line ("La date de décès saisie est antérieure à la date de");
                            Put_Line ("naissance");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when M_AG.Date_Fils_Incoherente_Exception =>
                            Put_Line ("Les dates saisies sont incohérentes avec la date de");
                            Put_Line ("naissance de l'enfant depuis lequel il a été ajouté");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when M_AG.Suppression_Racine_Exception =>
                            Put_Line ("Vous ne pouvez pas supprimer le noeud racine");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when M_AG.M_AB.Noeud_Present_Exception =>
                            Put_Line ("Le parent que vous voulez ajouté est déjà renseigné");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                        when others =>
                            Put_Line ("Une erreur est survenue durant la saisie");
                            Put_Line ("Veuillez recommencer");
                            Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                            Skip_Line;
                            Skip_Line;
                    end;

                    exit when Quitter;
                end loop;
                -- Fin sous-boucle --

            else
                Put_Line ("Veuillez entrer un nombre entier valide");
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;
            end if;
        exception
            when Ada.IO_Exceptions.Data_Error =>
                Put_Line ("Erreur, veuillez saisir un nombre entier");
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;
                Skip_Line;
        end;

        exit when Quitter_Programme;
    end loop;

    Detruire (AG);

end Programme_Principal;
