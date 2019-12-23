----------------------- IMPLEMENTATION DE TABLE_DE_HACHAGE ---------------------

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Table_De_Hachage is

    -- FONCTIONS ET PROCEDURES --

    procedure Initialiser (Table : out T_TH) is
    begin
        for i in 1..Capacite loop
            M_LCA.Initialiser (Table(i));
        end loop;
    end Initialiser;


    procedure Enregistrer (Table  : in out T_TH;
                           Cle    : in     T_Cle;
                           Donnee : in     T_Donnee) is
        i : Integer;
    begin
        i := 1 + Hachage (Cle) mod Capacite;
        M_LCA.Enregistrer (Table(i), Cle, Donnee);
    exception
        when M_LCA.Cle_Presente_Exception => raise Cle_Presente_Exception;
    end Enregistrer;


    procedure Modifier (Table  : in out T_TH;
                        Cle    : in     T_Cle;
                        Donnee : in     T_Donnee) is
        i : Integer;
    begin
        i := 1 + Hachage (Cle) mod Capacite;
        M_LCA.Modifier (Table(i), Cle, Donnee);
    exception
        when M_LCA.Cle_Absente_Exception => raise Cle_Absente_Exception;
    end Modifier;


    procedure Supprimer (Table : in out T_TH;
                         Cle   : in     T_Cle) is
        i : Integer;
    begin
        i := 1 + Hachage (Cle) mod Capacite;
        M_LCA.Supprimer (Table(i), Cle);
    exception
        when M_LCA.Cle_Absente_Exception => raise Cle_Absente_Exception;
    end Supprimer;


    function Element (Table : in T_TH;
                      Cle   : in T_Cle)
                      return T_Donnee is
        i : Integer;
    begin
        i := 1 + Hachage (Cle) mod Capacite;
        return M_LCA.Element (Table(i), Cle);
    exception
        when M_LCA.Cle_Absente_Exception => raise Cle_Absente_Exception;
    end Element;


    function Taille (Table : in T_TH)
                     return Integer is
        t : Integer := 0;
    begin
        for i in 1..Capacite loop
            t := t + M_LCA.Taille (Table(i));
        end loop;
        return t;
    end Taille;


    procedure Vider (Table : in out T_TH) is
    begin
        for i in 1..Capacite loop
            M_LCA.Vider (Table(i));
        end loop;
    end Vider;


    function Est_Cle_Utilisee (Table : in T_TH;
                               Cle   : in T_Cle)
                               return Boolean is
        Resultat : T_Donnee;
        pragma Unreferenced (Resultat); -- Pour éviter les warnings inutiles
    begin
        Resultat := Element (Table, Cle);
        return True;
    exception
        when Cle_Absente_Exception => return False;
    end Est_Cle_Utilisee;


    procedure Afficher (Table : in T_TH) is
        procedure Afficher_Liste is
                new M_LCA.Afficher(Afficher_Cle, Afficher_Donnee);
    begin
        for i in 1..Capacite loop
            Put("["); Put(i,1); Put("] : ");
            Afficher_Liste (Table(i));
        end loop;
        New_Line;
    end Afficher;


end Table_De_Hachage;
