----------------------------- TEST DE LISTE_CHAINEE ----------------------------

with Ada.Text_IO;   use Ada.Text_IO;
with Liste_Chainee;


procedure Test_Liste_Chainee is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    package M_LC is new Liste_Chainee (Character);
    use M_LC;

    -- PROCEDURES DE TESTS --

    M_Donnees : constant array (1..7) of Character
            := ('f','j','M','a','h','h','m');

    M_Indices_Supprime : constant array (1..7) of Integer
            := (7, 6, 1, 3, 2, 2, 1);


    procedure Afficher_Liste is new
            Afficher(Afficher_Donnee => Put);


    procedure Tester_Initialiser_Vider is

        LC1 : T_LC;

    begin

        Put_Line ("Test Initialiser Vider");
        Initialiser (LC1);
        pragma Assert (Taille (LC1) = 0);
        Vider (LC1);

    end;

    procedure Tester_Enregistrer is

        LC1 : T_LC;

    begin

        Put_Line ("Test Enregistrer");
        Initialiser (LC1);
        pragma Assert (Taille (LC1) = 0);
        for i in 1..7 loop
            Enregistrer (LC1, M_Donnees(i));
            Afficher_Liste (LC1);
            pragma Assert (Element (LC1, i) = M_Donnees(i));
            pragma Assert (Taille (LC1) = i);
        end loop;
        Vider (LC1);

    end Tester_Enregistrer;


    procedure Tester_Supprimer is

        LC1 : T_LC;

    begin

        Put_Line ("Test Supprimer");
        Initialiser (LC1);
        pragma Assert (Taille (LC1) = 0);
        for i in 1..7 loop
            Enregistrer (LC1, M_Donnees(i));
            pragma Assert (Element (LC1, i) = M_Donnees(i));
            pragma Assert (Taille (LC1) = i);
        end loop;
        Afficher_Liste (LC1);
        for i in 1..7 loop
            Supprimer (LC1, M_Indices_Supprime(i));
            Afficher_Liste (LC1);
        end loop;
        Vider (LC1);

    end Tester_Supprimer;

    -- EXECUTION DES PROGRAMMES DE TEST --

begin

    Put_Line ("Test du module Liste_Chainee");
    Tester_Initialiser_Vider;
    Tester_Enregistrer;
    Tester_Supprimer;
    Put_Line ("Tous les tests ont été passés avec succès !");

end Test_Liste_Chainee;
