----------------------------- TEST DE ARBRE_BINAIRE ----------------------------
-- Le programme est défini dans arbre_binaire.adb pour nous permettre
-- de récupérer les adresses des noeuds tout en gardant le type T_AB limited
-- private
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Arbre_Binaire;


procedure Test_Arbre_Binaire is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    package M_AB is new Arbre_Binaire (Integer);
    use M_AB;

    -- PROCEDURES DE TESTS --

    procedure Afficher_Entier (Valeur_Noeud : in Integer) is
    begin
        Put (Valeur_Noeud,0);
    end Afficher_Entier;

    procedure Afficher_Entier_Test is
            new M_AB.Afficher (Afficher_Entier);

    procedure Test is
            new M_AB.Test (Afficher_Entier_Test);

    -- EXECUTION DES PROGRAMMES DE TEST --

    AB : T_AB;
begin
    Put_Line ("Test du module Arbre_Binaire");
    Test (AB,1,2,3,4,5);
end Test_Arbre_Binaire;
