------------------ SPECIFICATION DE LISTE_CHAINEE_ASSOCIATIVE ------------------
-- Ce module définit un type liste chainée associative T_LCA et les
-- opérations associées.

-- PARAMETRES DE GENERICITE --

generic
    type T_Cle is private;
    type T_Donnee is private;


package Liste_Chainee_Associative is

    -- DECLARATION DES TYPES PRIVES --

    type T_LCA is private;

    -- EXCEPTIONS --

    Cle_Absente_Exception  : Exception;
    -- Clé absente dans la liste

    Cle_Presente_Exception : Exception;
    -- Clé déjà présente dans la liste

    -- FONCTIONS ET PROCEDURES --

    -- Initialiser une LCA
    -- Paramètres :
    --     Liste : la liste à initialiser
    procedure Initialiser (Liste : out T_LCA) with
            Post => Taille (Liste) = 0;

    -- Enregistrer dans la LCA une donnée en précisant la clé
    -- Paramètres :
    --     Liste  : La liste dans laquelle on veut enregistrer la donnée
    --     Cle    : Clé associée à la donnée
    --     Donnee : Donnée à enregistrer
    -- Exception :
    --     Cle_Presente_Exception : La clé existe déjà
    procedure Enregistrer (Liste  : in out T_LCA;
                           Cle    : in     T_Cle;
                           Donnee : in     T_Donnee) with
            Post => Taille (Liste) = Taille (Liste)'Old + 1
            and Element (Liste, Cle) = Donnee
            and Est_Cle_Utilisee (Liste, Cle);

    -- Modifier dans la LCA une donnée en précisant la clé
    -- Paramètres :
    --     Liste  : La liste dans laquelle on veut modifier la donnée
    --     Cle    : Clé associée à la donnée
    --     Donnee : Nouvelle donnée à enregistrer
    -- Exception :
    --     Cle_Absente_Exception : La clé n'existe pas
    procedure Modifier (Liste  : in out T_LCA;
                        Cle    : in     T_Cle;
                        Donnee : in     T_Donnee) with
            Post => Taille (Liste) = Taille (Liste)'Old
            and Element (Liste, Cle) = Donnee
            and Est_Cle_Utilisee (Liste, Cle);

    -- Supprimer dans la LCA la donnée associée à une clé
    -- Paramètres :
    --     Liste : La liste dans laquelle on veut supprimer la donnée
    --     Cle   : Clé associée à la donnée
    -- Exception : Cle_Absente_Exception si la clé n'existe pas
    procedure Supprimer (Liste : in out T_LCA;
                         Cle   : in     T_Cle);

    -- Obtenir la donnée associée à une clé
    -- Paramètres :
    --     Liste : la liste dans laquelle on veut obtenir la donnée
    --     Cle   : Clé associée à la donnée
    -- Exception :
    --     Cle_Absente_Exception : La clé n'existe pas
    -- Retourne la donnée
    function Element (Liste : in T_LCA;
                      Cle   : in T_Cle)
                      return T_Donnee;

    -- Obtenir la taille de la LCA
    -- Paramètres :
    --     Liste : la liste dont on veut obtenir la taille
    -- Retourne la taille
    function Taille (Liste : in T_LCA)
                     return Integer;

    -- Vider la LCA, toutes les informations enregistrées dans la LCA sont
    -- supprimées
    -- Paramètres :
    --     Liste : La liste qu'on veut vider
    procedure Vider (Liste : in out T_LCA);

    -- Est-ce qu'une clé est utilisé ?
    -- Paramètres :
    --     Liste : La liste dans laquelle on cherche la clé
    --     Cle   : La clé à chercher
    function Est_Cle_Utilisee (Liste: in T_LCA;
                               Cle  : in T_Cle)
                               return Boolean;

    -- Afficher la liste
    -- Paramètre :
    --    Liste : La liste à afficher
    generic
        with procedure Afficher_Cle (Cle : in T_Cle);
        with procedure Afficher_Donnee (Donnee : in T_Donnee);
    procedure Afficher (Liste : in T_LCA);

    -- IMPLEMENTATION DES TYPES PRIVES --

private

    type T_Cellule;

    type T_LCA is access T_Cellule;

    type T_Cellule is
        record
            Cle     : T_Cle;    -- La clé identifiant la cellule
            Donnee  : T_Donnee; -- La donnée associée à la cellule
            Suivant : T_LCA;    -- Le pointeur sur la cellule suivante
         end record;

end Liste_Chainee_Associative;
