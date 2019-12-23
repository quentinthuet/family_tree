------------------ IMPLEMENTATION DE LISTE_CHAINEE_ASSOCIATIVE -----------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Liste_Chainee_Associative is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    procedure Free is
		new Ada.Unchecked_Deallocation (T_Cellule, T_LCA);

    -- FONCTIONS ET PROCEDURES --

    procedure Initialiser (Liste : out T_LCA) is
    begin
        Liste := null;
    end Initialiser;


    procedure Enregistrer (Liste  : in out T_LCA ;
                           Cle    : in     T_Cle ;
                           Donnee : in     T_Donnee) is
    begin
        if Est_Cle_Utilisee (Liste, Cle) then
            raise Cle_Presente_Exception;
        end if;
        Liste := new T_Cellule'(Cle, Donnee, Liste);
    end Enregistrer;


    procedure Modifier (Liste  : in out T_LCA ;
                        Cle    : in     T_Cle ;
                        Donnee : in     T_Donnee) is
    begin
        if Liste = null then
            raise Cle_Absente_Exception;
        elsif Liste.Cle = Cle then
            Liste.Donnee := Donnee;
        else
            Modifier (Liste.Suivant, Cle, Donnee);
        end if;
    end Modifier;


    procedure Supprimer (Liste : in out T_LCA;
                         Cle   : in     T_Cle) is
        tmp : T_LCA;
    begin
        if Liste = null then
            raise Cle_Absente_Exception;
        elsif Liste.Cle = Cle then
            tmp := Liste;
            Liste := Liste.Suivant;
            Free (tmp);
        else
            Supprimer (Liste.Suivant, Cle);
        end if;
    end Supprimer;


    function Element (Liste : in T_LCA;
                      Cle   : in T_Cle)
                      return T_Donnee is
    begin
        if Liste = null then
            raise Cle_Absente_Exception;
        elsif Liste.Cle = Cle then
            return Liste.Donnee;
        else
            return Element (Liste.Suivant, Cle);
        end if;
    end Element;


    function Taille (Liste : in T_LCA)
                     return Integer is
    begin
        if Liste = null then
            return 0;
        else
            return Taille (Liste.Suivant) + 1;
        end if;
    end Taille;


    procedure Vider (Liste : in out T_LCA) is
    begin
        if Liste /= null then
            Vider (Liste.Suivant);
            Free (Liste);
        else
            null;
        end if;
    end Vider;


    function Est_Cle_Utilisee (Liste : in T_LCA;
                               Cle   : in T_Cle)
                               return Boolean is
        Resultat : T_Donnee;
        pragma Unreferenced (Resultat); -- Pour éviter les warnings inutiles
    begin
        Resultat := Element (Liste, Cle);
        return True;
    exception
        when Cle_Absente_Exception => return False;
    end Est_Cle_Utilisee;


    procedure Afficher (Liste : in T_LCA) is
    begin
        if Liste /= null then
            Afficher_Cle (Liste.Cle);
            Put(" : ");
            Afficher_Donnee (Liste.Donnee);
            Afficher (Liste.Suivant);
        else
            Put_Line (" --[");
        end if;
    end Afficher;

end Liste_Chainee_Associative;
