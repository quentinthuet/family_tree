---------------------------- TEST DE TABLE_DE_HACHAGE --------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Table_De_Hachage;


procedure Test_Table_De_Hachage is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    function M_Hachage (Cle : in Integer) return Integer is
    begin
        return Cle;
    end M_Hachage;

    package M_Table_De_Hachage is new Table_De_Hachage (Integer,
                                                        Character,
                                                        3,
                                                        M_Hachage);
    use M_Table_De_Hachage;

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

    procedure Afficher_Table is new
            Afficher(Afficher_Cle    => Afficher_Entier,
                     Afficher_Donnee => Afficher_Caractere);


    procedure Tester_Initialiser_Vider is
        TH1 : T_TH;
    begin
        Put_Line ("Test Initialiser Vider");
        Initialiser (TH1);
        pragma Assert (Taille (TH1) = 0);
        Vider (TH1);
    end;


    procedure Tester_Enregistrer is
        TH1 : T_TH;
    begin
        Put_Line ("Test Enregistrer");
        Initialiser (TH1);
        pragma Assert (Taille (TH1) = 0);
        Afficher_Table (TH1);
        for i in 1..7 loop
            Enregistrer (TH1, M_Cles(i), M_Donnees(i));
            pragma Assert (Element (TH1, M_Cles(i)) = M_Donnees(i));
            pragma Assert (Taille (TH1) = i);
            Afficher_Table (TH1);
        end loop;
        Vider (TH1);
    end Tester_Enregistrer;


    procedure Tester_Supprimer is
        TH1 : T_TH;
    begin
        Put_Line ("Test Supprimer");
        Initialiser (TH1);
        pragma Assert (Taille (TH1) = 0);
        for i in 1..7 loop
            Enregistrer (TH1, M_Cles(i), M_Donnees(i));
            pragma Assert (Element (TH1, M_Cles(i)) = M_Donnees(i));
            pragma Assert (Taille (TH1) = i);
        end loop;
        Afficher_Table (TH1);
        for i in 1..7 loop
            Supprimer (TH1, M_Cles_Sup(i));
            pragma Assert (not (Est_Cle_Utilisee (TH1, M_Cles_Sup(i))));
            pragma Assert (Taille (TH1) = 7 - i);
            Afficher_Table (TH1);
        end loop;
        Vider (TH1);
    end Tester_Supprimer;


    procedure Tester_Modifier is
        TH1 : T_TH;
    begin
        Put_Line ("Test Modifier");
        Initialiser (TH1);
        pragma Assert (Taille (TH1) = 0);
        for i in 1..7 loop
            Enregistrer (TH1, M_Cles(i), M_Donnees(i));
            pragma Assert (Element (TH1, M_Cles(i)) = M_Donnees(i));
            pragma Assert (Taille (TH1) = i);
        end loop;
        Afficher_Table (TH1);
        for i in 1..7 loop
            Modifier (TH1, M_Cles_Sup(i), M_Nouvelles_Donnees(i));
            pragma Assert (Element (TH1, M_Cles_Sup(i)) = M_Nouvelles_Donnees(i));
            pragma Assert (Taille (TH1) = 7);
            Afficher_Table (TH1);
        end loop;
        Vider (TH1);
    end Tester_Modifier;

    -- EXECUTION DES PROGRAMMES DE TEST --

begin
    Put_Line ("Test du module Table_De_Hachage");
    Tester_Initialiser_Vider;
    Tester_Enregistrer;
    Tester_Supprimer;
    Tester_Modifier;
    Put_Line ("Tous les tests ont été passés avec succès !");
end Test_Table_De_Hachage;
