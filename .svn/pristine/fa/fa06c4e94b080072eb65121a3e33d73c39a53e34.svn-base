----------------------- SPECIFICATION DE TABLE_DE_HACHAGE ----------------------
-- Ce module définit un type table de hachage T_TH et les opérations associées.

with Liste_Chainee_Associative;

-- PARAMETRES DE GENERICITE --

generic
    type T_Cle is private;
    type T_Donnee is private;
    Capacite : Integer;
    with function Hachage (Cle : in T_Cle) return Integer;


package Table_De_Hachage is

    -- DECLARATION DES TYPES PRIVES --

    type T_TH is limited private;

    -- EXCEPTIONS --

    Cle_Absente_Exception  : Exception;
    -- clé absente dans la table

    Cle_Presente_Exception : Exception;
    -- clé déjà présente dans la table

    -- FONCTIONS ET PROCEDURES --

    -- Initialiser une TH
    -- Paramètres :
    --     Table : la table à initialiser
    procedure Initialiser (Table : out T_TH) with
            Post => Taille (Table) = 0;


    -- Enregistrer dans la TH une donnée en précisant la clé
    -- Paramètres :
    --     Table  : la table dans laquelle on veut enregistrer la donnée
    --     cle    : clé associée à la donnée
    --     donnee : donnée à enregistrer
    -- Exception : Cle_Presente_Exception si la clé existe déjà
    procedure Enregistrer (Table  : in out T_TH;
                           Cle    : in     T_Cle;
                           Donnee : in     T_Donnee) with
            Post => Taille (Table) = Taille (Table)'Old + 1
            and Element (Table, Cle) = Donnee
            and Est_Cle_Utilisee (Table, Cle);


    -- Modifier dans la TH une donnée en précisant la clé
    -- Paramètres :
    --     Table  : la table dans laquelle on veut modifier la donnée
    --     cle    : clé associée à la donnée
    --     donnee : nouvelle donnée à enregistrer
    -- Exception : Cle_Absente_Exception si la clé n'existe pas
    procedure Modifier (Table  : in out T_TH;
                        Cle    : in     T_Cle;
                        Donnee : in     T_Donnee) with
            Post => Taille (Table) = Taille (Table)'Old
            and Element (Table, Cle) = Donnee
            and Est_Cle_Utilisee (Table, Cle);


    -- Supprimer dans la TH la donnée associée à une clé
    -- Paramètres :
    --     Table : la table dans laquelle on veut supprimer la donnée
    --     cle   : clé associée à la donnée
    -- Exception : Cle_Absente_Exception si la clé n'existe pas
    procedure Supprimer (Table : in out T_TH;
                         Cle   : in     T_Cle) with
            Post => not(Est_Cle_Utilisee (Table, Cle));


    -- Obtenir la donnée associée à une clé
    -- Paramètres :
    --     Table : la table dans laquelle on veut obtenir la donnée
    --     cle   : clé associée à la donnée
    -- Exception : Cle_Absente_Exception si la clé n'existe pas
    -- Retourne la donnée
    function Element (Table : in T_TH;
                      Cle   : in T_Cle) return
            T_Donnee;


    -- Obtenir la taille de la TH
    -- Paramètres :
    --     Table : la table dont on veut obtenir la taille
    -- Retourne la taille
    function Taille (Table : in T_TH)
                     return Integer;

    -- Vider la TH, toutes les informations enregistrées dans la TH sont
    -- supprimées
    -- Paramètres :
    --     Table : la table qu'on veut vider
    procedure Vider (Table : in out T_TH);


    -- Est-ce qu'une clé est utilisé ?
    -- Paramètres :
    --     Table : la table dans laquelle on cherche la clé
    --     Cle   : la clé à chercher
    function Est_Cle_Utilisee (Table : in T_TH;
                               Cle   : in T_Cle)
                               return Boolean;


    -- Afficher la table de hachage
    generic
        with procedure Afficher_Cle (Cle : in T_Cle);
        with procedure Afficher_Donnee (Donnee : in T_Donnee);
    procedure Afficher (Table : in T_TH);

    -- IMPLEMENTATION DES TYPES PRIVES --

private

    package M_LCA is new Liste_Chainee_Associative(T_Cle, T_Donnee);
    use M_LCA;

    type T_TH is array (1..Capacite) of T_LCA;


end Table_De_Hachage;
