------------------------ IMPLEMENTATION DE LISTE_CHAINEE -----------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Liste_Chainee is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    procedure Free is
            new Ada.Unchecked_Deallocation (T_Cellule, T_LC);

    -- FONCTIONS ET PROCEDURES --

    procedure Initialiser (Liste : out T_LC) is
    begin

        Liste := null;

    end Initialiser;


    procedure Enregistrer (Liste  : in out T_LC;
                           Donnee : in     T_Donnee) is

        Curseur : T_LC;   -- Curseur qui parcourt les éléments de la liste

    begin

        if Liste = null then
            Liste := new T_Cellule'(Donnee, null);
        else
            Curseur := Liste;
            -- Atteindre le dernier élément de la liste
            while Curseur.Suivant /= null loop
                Curseur := Curseur.Suivant;
            end loop;
            -- Ajouter le nouvel élément à la fin de la liste
            Curseur.Suivant := new T_Cellule'(Donnee, null);
        end if;

    end Enregistrer;


    procedure Supprimer (Liste  : in out T_LC;
                         Indice : in     Integer) is

        tmp : T_LC; -- Variable temporaire pour stocker la cellule à supprimer

    begin

        if Liste = null or Indice < 1 then
            raise Indice_Absent_Exception;
        elsif Indice = 1 then
            tmp := Liste;
            Liste := Liste.Suivant;
            Free (tmp);
        else
            Supprimer (Liste.Suivant, Indice - 1);
        end if;

    end Supprimer;


    function Element (Liste  : in T_LC;
                      Indice : in Integer)
                      return T_Donnee is
    begin

        if Liste = null or Indice < 1 then
            raise Indice_Absent_Exception;
        elsif Indice = 1 then
            return Liste.Donnee;
        else
            return Element (Liste.Suivant, Indice - 1);
        end if;

    end Element;


    function Taille (Liste : in T_LC)
                     return Integer is
    begin

        if Liste = null then
            return 0;
        else
            return Taille (Liste.Suivant) + 1;
        end if;

    end Taille;


    procedure Vider (Liste : in out T_LC) is
    begin

        if Liste /= null then
            Vider (Liste.Suivant);
            Free (Liste);
        else
            null;
        end if;

    end Vider;


    procedure Afficher (Liste : in T_LC) is

        Curseur : T_LC;   -- Curseur qui parcourt les éléments de la liste

    begin

        Curseur := Liste;
        -- Parcourir la liste
        while Curseur /= null loop
            -- Afficher chaque élément
            Put (" --> ");
            Afficher_Donnee (Curseur.Donnee);
            -- Passer à l'élément suivant
            Curseur := Curseur.Suivant;
        end loop;
        Put_Line (" --[");

    end Afficher;

end Liste_Chainee;
