-- PROGRAMME PRINCIPAL --
--      PARTIE 1       --

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

    -- PROCEDURES DE SAISIES --

    procedure Saisir_Informations (Informations : in out T_Informations; Modification : in Boolean) is
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

    procedure Saisir_Identifiant (Identifiant : in out Integer;
                                  Arbre : in T_AG) is
    begin
        Put_Line ("Si vous souhaitez consulter l'arbre généalogique, tapez 0.");
        Put ("Identifiant (ou 0 pour affichage) : ");
        Get (Identifiant);
        Skip_Line;
        if Identifiant = 0 then
            Afficher (Arbre);
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
            Saisir_Identifiant (Identifiant, Arbre);
    end Saisir_Identifiant;

    -- PROCEDURES LIEES AU CHARGEMENT DE L'EXEMPLE --

    procedure Charger_Exemple (AG : out T_AG) is
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
        Initialiser (AG, Informations(1));
        for i in 1..17 loop
            Ajouter (AG, Identifiants(i), Parents(i), Informations(i+1));
        end loop;
    end Charger_Exemple;

    procedure Choisir_Exemple (AG : in out T_AG) is
        Choix : Character;
        Informations : T_Informations;
    begin
        loop
            Put      ("Charger exemple (O/N)               : ");
            Get (Choix);
            Skip_Line;
            if Choix = 'O' or Choix = 'o' then
                Charger_Exemple (AG);
            elsif Choix = 'N' or Choix = 'n' then
                -- Ajout du premier élément --
                Put_Line ("Pour commencer, veuillez saisir les informations de");
                Put_Line ("la première personne à ajouter à l'arbre.");
                New_Line (2);
                Saisir_Informations (Informations, False);
                Initialiser (AG, Informations);
            else
                Put_Line ("Charactère saisi non valide, veuillez recommencer");
            end if;
            exit when Choix = 'O' or Choix = 'o' or Choix = 'N' or Choix = 'n';
        end loop;
    end;

    -- VARIABLES --

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
    Taille_Retour       : Integer := 0; -- Initialisés à 0 pour éviter les warnings
    AG1                 : T_AG;
    --Parent_Ajouter      : T_Parent;
    Informations        : T_Informations;
    Choix,
    Decede              : Character;

    -- IMPLEMANTATION --

begin

    Quitter := False;

    New_Line (4);
    Put_Line ("Bienvenue dans le gestionnaire d'arbre généalogique");
    Put_Line ("             Tom Pillot, Quentin Thuet             ");
    New_Line (4);

    -- Boucle principale

    Choisir_Exemple(AG1);
    Afficher(AG1);

    loop
        begin
            -- Affichage du menu principal
            New_Line (4);
            Put_Line("                    MENU PRINCIPAL                 ");
            New_Line;
            Put_Line ("  1.  Ajouter un parent");
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
            Put_Line ("  13. Quitter le programme");
            New_Line;
            loop
                Put_Line ("Veuillez saisir le nombre correspondant à l'action");
                Put      ("désirée : ");
                begin
                    Get (Num_Action);
                    Skip_Line;
                    if Num_Action < 1 or Num_Action > 13 then
                        Put_Line ("Action inexistante, veuillez recommencer");
                    end if;
                exception
                    when Ada.IO_Exceptions.Data_Error =>
                        Put_Line ("Erreur, veuillez saisir un nombre entier");
                        Skip_Line;
                end;
                exit when Num_Action >= 1 and Num_Action <= 13;
            end loop;
            case Num_Action is

                -- AJOUTER --
            when 1 =>
                New_Line (4);
                Put_Line ("Veuillez saisir l'identifiant de la personne à qui");
                Put_Line ("vous voulez ajouter un parent.");
                Saisir_Identifiant (Id_Ajouter, AG1);
                New_Line;
                Saisir_Informations (Informations, False);
                if Informations.Sexe = HOMME then
                    Ajouter (AG1, Id_Ajouter, PERE, Informations);
                else
                    Ajouter (AG1, Id_Ajouter, MERE, Informations);
                end if;

                -- NOMBRE D'ANCETRES --
            when 2 =>
                New_Line (4);
                Put_Line ("Veuillez saisir l'identifiant de la personne dont");
                Put_Line ("vous voulez connaitre le nombre d'ancêtres connus");
                Saisir_Identifiant (Id_Ancetres, AG1);
                Taille_Retour := Taille (Ag1, Id_Ancetres);
                New_Line;
                Put      ("Cette personne a ");
                Put      (Taille_Retour - 1, 0);
                Put_Line (" ancêtres connus.");
                New_Line (2);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- ANCETRES A UNE GENERATION DONNEE --
            when 3 =>
                New_Line (4);
                Put_Line ("Veuillez saisir l'identifiant de la personne dont");
                Put_Line ("vous voulez connaitre les ancêtres");
                Saisir_Identifiant (Id_Ancetres_Gen, AG1);
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
                Afficher_Ancetres_Generation (AG1, Id_Ancetres_Gen, Num_Generation);
                New_Line (2);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- AFFICHAGE --
            when 4 =>
                New_Line (4);
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
                    Afficher (AG1);
                else
                    Afficher (AG1, id_racine => Id_Afficher);
                end if;
                New_Line (2);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- SUPPRIMER --
            when 5 =>
                New_Line (4);
                Put_Line ("Veuillez saisir l'identifiant à supprimer");
                Put_Line ("!!Attention, cela supprimera aussi ses ancêtres!!");
                Saisir_Identifiant (Id_Supprimer, AG1);
                New_Line (2);
                Put_Line ("Les individus suivants seront supprimés :");
                Afficher (AG1, -1, Id_Supprimer);
                loop
                    Put      ("Confirmer ? (O/N) : ");
                    Get (Choix);
                    Skip_Line;
                    if Choix = 'O' or Choix = 'o' then
                        Taille_Retour := Taille (AG1);
                        Supprimer (AG1, Id_Supprimer);
                        New_Line (2);
                        Put      (Taille_Retour - Taille (AG1), 1);
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
                Put_Line ("Les individus qui n'ont qu'un parent renseigné sont ");
                Mono_Parent (AG1);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- ID DEUX PARENTS --
            when 7 =>
                Put_Line ("Les individus qui ont deux parents renseignés sont ");
                Deux_Parents (AG1);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- ID AUCUN PARENT --
            when 8 =>
                Put_Line ("Les individus qui n'ont aucun parent renseigné sont ");
                Sans_Parent (AG1);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- AFFICHER N GENERATIONS --
            when 9 =>
                New_Line (4);
                Put_Line ("Veuillez saisir l'identifiant de la personne à");
                Put_Line ("partir duquel vous voulez afficher l'arbre");
                Saisir_Identifiant (Id_Ancetres_n_Gen, AG1);
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
                Afficher (AG1, Num_Generations, Id_Ancetres_n_Gen);
                New_Line (2);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- AFFICHER HOMONYMES --
            when 10 =>
                Put_Line ("Veuillez saisir l'identifiant d'un premier individu");
                Saisir_Identifiant (Id_Homonyme_1, AG1);
                Put_Line ("Veuillez saisir l'identifiant d'un second individu");
                Saisir_Identifiant (Id_Homonyme_2, AG1);
                New_Line;
                Put_Line ("Les ancêtres homonymes aux deux individus saisis sont");
                Afficher_Homonymes (AG1, Id_Homonyme_1, Id_Homonyme_2);
                New_Line (2);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- AFFICHER INFORMATIONS --
            when 11 =>
                Put_Line ("Veuillez saisir l'identifiant de l'individu dont vous");
                Put_Line ("voulez connaître les informations détaillées");
                Saisir_Identifiant (Id_Informations, AG1);
                New_Line;
                Put_Line ("Les informations de l'individu sont : ");
                Afficher_Informations_Detaillee (AG1, Id_Informations);
                New_Line (2);
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- MODIFIER INFORMATIONS --
            when 12 =>
                New_Line (4);
                Put_Line ("Veuillez saisir l'identifiant de l'individu dont vous");
                Put_Line ("voulez modifier les informations détaillées");
                Saisir_Identifiant (Id_Modifications, AG1);
                New_Line;
                Put_Line ("Actuellement, les informations de cet individus sont");
                Put_Line ("les suivantes :");
                Afficher_Informations_Detaillee (AG1, Id_Modifications);
                Informations := Obtenir_Informations (AG1, Id_Modifications);
                loop
                    begin
                        Quitter_Boucle := False;
                        -- Menu
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
                    Modifier (AG1, Id_Modifications, Informations);
                    Put_Line ("Les informations de l'individu sont maintenant : ");
                    Afficher_Informations_Detaillee (AG1, Id_Modifications);
                    exit when Quitter_Boucle;
                end loop;

                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;

                -- QUITTER --
            when 13 =>
                Quitter := True;
            when others =>
                null;
            end case;

        exception
            when ID_Absent_Exception =>
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
            when others =>
                Put_Line ("Une erreur est survenue durant la saisie");
                Put_Line ("Veuillez recommencer");
                Put_Line ("Appuyez sur Entrée pour revenir au menu principal");
                Skip_Line;
                Skip_Line;
        end;

        exit when Quitter;
    end loop;

    Detruire(AG1);

end Programme_Principal;
