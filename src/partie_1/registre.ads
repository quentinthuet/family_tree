------------------------ SPECIFICATION DE ARBRE_BINAIRE ------------------------
-- Ce module définit un type Registre T_R et les opérations associées.

with Table_De_Hachage;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- PARAMETRES DE GENERICITE --

generic
    Capacite : Integer;
package Registre is

    -- IMPLEMENTATION DES TYPES PUBLICS --

    type T_Sexe is (HOMME, FEMME);

    type T_Date is
        record
            Jour  : Integer;
            Mois  : Integer;
            Annee : Integer;
            -- Invariant
            --    Jour entre 1 et 28, 29, 30 ou 31 suivant le mois et l'annEe
            --    Mois entre 1 et 12
            --    Annee inferieur a l'annee courante
        end record;

    type T_Informations is
        record
            Nom            : Unbounded_String;
            Prenom         : Unbounded_String;
            Sexe           : T_Sexe;
            Date_Naissance : T_Date;
            Lieu_Naissance : Unbounded_String;
            Date_Deces     : T_Date;
        end record;

    -- INSTANTIATION DES MODULES EXTERIEURS --

    function F_Hachage (Cle : in Integer) return Integer;

    package M_TH is new Table_De_Hachage (Integer,
                                          T_Informations,
                                          Capacite,
                                          F_Hachage);
    use M_TH;

    -- HERITAGE --

    type T_R is new T_TH;

    -- EXCEPTIONS --

    Date_Fils_Incoherente_Exception     : Exception;
    -- La date de naissance du fils est antérieure à celle de la nouvelle entrée
    -- ou postérieur à sa date

    Ordre_Date_Incoherent_Exception     : Exception;
    -- La date de décès est antérieure à la date de naissance

    Date_Naissance_Impossible_Exception : Exception;
    -- La date de naissance ne correspond pas à une date réelle

    Date_Deces_Impossible_Exception     : Exception;
    -- La date de décès ne correspond pas à une date réelle

    Cle_Absente_Exception               : Exception;
    -- L'id entrée est déjà présent

    Cle_Presente_Exception              : Exception;
    -- L'id entrée n'est pas encore présent

    -- FONCTIONS ET PROCEDURES --

    -- Ajouter une nouvelle entrée au registre
    -- Paramètres :
    --     Registre            : Le registre auquel ajouter la nouvelle entrée
    --     Id                  : L'id de la nouvelle entrée
    --     Informations        : Les informations de la nouvelle entrée
    --     Date_Naissance_Fils : La date de naissance du fils de la nouvelle
    --                           entrée
    -- Exception :
    --     Date_Fils_Incoherente_Exception     : La date de naissance du fils
    --                                           est antérieure à celle de la
    --                                           nouvelle entrée ou postérieur à
    --                                           sa date de décès
    --     Ordre_Date_Incoherent_Exception     : La date de décès est antérieure
    --                                           à la date de naissance
    --     Date_Naissance_Impossible_Exception : La date de naissance ne
    --                                           correspond pas à une date
    --                                           réelle
    --     Date_Deces_Impossible_Exception     : La date de décès ne correspond
    --                                           pas à une date réelle
    --     Cle_Presente_Exception              : L'id entrée est déjà présent
    procedure Enregistrer (Registre            : in out T_R;
                           Id                  : in     Integer;
                           Informations        : in     T_Informations;
                           Date_Naissance_Fils : in     T_Date);

    -- Modifier une entrée du registre
    -- Paramètres :
    --     Registre            : Le registre auquel modifier une entrée
    --     Id                  : L'id de l'entrée à modifier
    --     Informations        : Les nouvelles informations de l'entrée à
    --                           modifier
    --     Date_Naissance_Fils : La date de naissance du fils de la nouvelle
    --                           entrée
    -- Exception :
    --     Date_Fils_Incoherente_Exception     : La date de naissance du fils
    --                                           est antérieure à celle de la
    --                                           nouvelle entrée ou postérieur à
    --                                           sa date de décès
    --     Ordre_Date_Incoherent_Exception     : La date de décès est antérieure
    --                                           à la date de naissance
    --     Date_Naissance_Impossible_Exception : La date de naissance ne
    --                                           correspond pas à une date
    --                                           réelle
    --     Date_Deces_Impossible_Exception     : La date de décès ne correspond
    --                                           pas à une date réelle
    --     Cle_Absente_Exception               : L'id entrée n'est pas encore
    --                                           présent
    procedure Modifier (Registre            : in out T_R;
                        Id                  : in     Integer;
                        Informations        : in     T_Informations;
                        Date_Naissance_Fils : in     T_Date);


end Registre;
