----------------------- TEST DE LISTE_CHAINEE_ASSOCIATIVE ----------------------

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Liste_Chainee_Associative;

procedure Test_Liste_Chainee_Associative is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    package M_LCA is new Liste_Chainee_Associative (Integer, Character);
    use M_LCA;

    -- PROCEDURES DE TESTS --

    M_Cles : constant array (1..7) of Integer
            := (4,8,1,2,7,14,6);

    M_Cles_Sup : constant array (1..7) of Integer
            := (14,1,4,8,6,2,7);

    M_Donnees : constant array (1..7) of Character
            := ('f','j','M','a','h','h','m');

    M_Nouvelles_Donnees : constant array (1..7) of Character
            := ('a','b','c','j','r','g','q');



    procedure Afficher_Entier (entier : in Integer) is
    begin
        Put (" --> ");
        Put (entier,1);
    end Afficher_Entier;

    procedure Afficher_Caractere (c : in Character) is
    begin
        Put(c);
    end Afficher_Caractere;

    procedure Afficher_Liste is new
            Afficher(Afficher_Cle    => Afficher_Entier,
                     Afficher_Donnee => Afficher_Caractere);


    procedure Tester_Initialiser_Vider is
        LCA1 : T_LCA;
    begin
        Put_Line ("Test Initialiser Vider");
        Initialiser (LCA1);
        pragma Assert (Taille (LCA1) = 0);
        Vider (LCA1);
    end;

    procedure Tester_Enregistrer is
        LCA1 : T_LCA;
    begin
        Put_Line ("Test Enregistrer");
        Initialiser (LCA1);
        pragma Assert (Taille (LCA1) = 0);
        for i in 1..7 loop
            Enregistrer (LCA1, M_Cles(i), M_Donnees(i));
            pragma Assert (Element (LCA1, M_Cles(i)) = M_Donnees(i));
            pragma Assert (Taille (LCA1) = i);
            Afficher_Liste (LCA1);
        end loop;
        Vider (LCA1);
    end Tester_Enregistrer;


    procedure Tester_Supprimer is
        LCA1 : T_LCA;
    begin
        Put_Line ("Test Supprimer");
        Initialiser (LCA1);
        pragma Assert (Taille (LCA1) = 0);
        for i in 1..7 loop
            Enregistrer (LCA1, M_Cles(i), M_Donnees(i));
            pragma Assert (Element (LCA1, M_Cles(i)) = M_Donnees(i));
            pragma Assert (Taille (LCA1) = i);
        end loop;
        Afficher_Liste (LCA1);
        for i in 1..7 loop
            Supprimer (LCA1, M_Cles_Sup(i));
            pragma Assert (not (Est_Cle_Utilisee (LCA1, M_Cles_Sup(i))));
            pragma Assert (Taille (LCA1) = 7 - i);
            Afficher_Liste (LCA1);
        end loop;
        Vider (LCA1);
    end Tester_Supprimer;


    procedure Tester_Modifier is
        LCA1 : T_LCA;
    begin
        Put_Line ("Test Modifier");
        Initialiser (LCA1);
        pragma Assert (Taille (LCA1) = 0);
        for i in 1..7 loop
            Enregistrer (LCA1, M_Cles(i), M_Donnees(i));
            pragma Assert (Element (LCA1, M_Cles(i)) = M_Donnees(i));
            pragma Assert (Taille (LCA1) = i);
        end loop;
        Afficher_Liste (LCA1);
        for i in 1..7 loop
            Modifier (LCA1, M_Cles_Sup(i), M_Nouvelles_Donnees(i));
            pragma Assert (Element (LCA1, M_Cles_Sup(i)) = M_Nouvelles_Donnees(i));
            pragma Assert (Taille (LCA1) = 7);
            Afficher_Liste (LCA1);
        end loop;
        Vider (LCA1);
    end Tester_Modifier;

    -- EXECUTION DES PROGRAMMES DE TEST --

begin
    Put_Line ("Test du module Liste_Chainee_Associative");
    Tester_Initialiser_Vider;
    Tester_Enregistrer;
    Tester_Supprimer;
    Tester_Modifier;
    Put_Line ("Tous les tests ont été passés avec succès !");
end Test_Liste_Chainee_Associative;
