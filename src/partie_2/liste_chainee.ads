------------------------ SPECIFICATION DE LISTE_CHAINEE ------------------------
-- Ce module définit un type liste chainée T_LC et les
-- opérations associées.

-- PARAMETRES DE GENERICITE --

generic
    type T_Donnee is private;


package Liste_Chainee is

    -- DECLARATION DES TYPES PRIVES --

    type T_LC is private;

    -- EXCEPTIONS --

    Indice_Absent_Exception : Exception;
    -- L'indice est plus grand que la taille de la liste ou est négatif ou nul

    -- FONCTIONS ET PROCEDURES --

    -- Initialiser une LC
    -- Paramètres :
    --     Liste : La liste à initialiser
    procedure Initialiser (Liste : out T_LC) with
            Post => Taille (Liste) = 0;


    -- Enregistrer un élément dans la LC
    -- Paramètres :
    --     Liste  : La liste dans laquelle on veut enregistrer la donnée
    --     Donnee : Donnée à enregistrer
    procedure Enregistrer (Liste  : in out T_LC ;
                           Donnee : in     T_Donnee) with
            Post => Taille (Liste) = Taille (Liste)'Old + 1;


    -- Supprimer un élément dans la LC
    -- Paramètres :
    --     Liste  : La liste dans laquelle on veut supprimer la donnée
    --     Indice : Indice de l'élément à supprimer
    -- Exception : Indice_Absent_Exception si l'indice dépasse la taille de
    -- la liste ou si l'indice est négatif ou nul
    procedure Supprimer (Liste  : in out T_LC;
                         Indice : in     Integer);


    -- Obtenir un élément de la liste
    -- Paramètres :
    --     Liste  : La liste dans laquelle on veut obtenir la donnée
    --     Indice : Indice de l'élément
    -- Exception : Indice_Absent_Exception si l'indice dépasse la taille de
    -- la liste ou si l'indice est négatif ou nul
    -- Retourne la donnée
    function Element (Liste  : in T_LC;
                      Indice : in Integer)
                      return T_Donnee;


    -- Obtenir la taille de la LC
    -- Paramètres :
    --     Liste : La liste dont on veut obtenir la taille
    -- Retourne la taille
    function Taille (Liste : in T_LC)
                     return Integer;


    -- Vider la LC, toutes les informations enregistrées sont supprimées
    -- Paramètres :
    --     Liste : La liste qu'on veut vider
    procedure Vider (Liste : in out T_LC);


    -- Afficher la LC en affichant chaque élément avec Afficher_Donnee
    -- Paramètres :
    --     Liste : La liste à afficher
    generic
        with procedure Afficher_Donnee (Donnee : in T_Donnee);
    procedure Afficher (Liste : in T_LC);

    -- IMPLEMENTATION DES TYPES PRIVES --

private

    type T_Cellule;

    type T_LC is access T_Cellule;

    type T_Cellule is
        record
            Donnee  : T_Donnee; -- La donnee stockée dans la cellule
            Suivant : T_LC;     -- Le pointeur sur la cellule suivante
         end record;

end Liste_Chainee;
