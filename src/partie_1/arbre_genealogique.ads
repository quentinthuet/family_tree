---------------------- SPECIFICATION DE ARBRE_GENEALOGIQUE ---------------------
-- Ce module définit un type arbre généalogique T_AG et les opérations associées.

with Table_De_Hachage;
with Registre;
with Arbre_Binaire;

-- PARAMETRES DE GENERICITE --

generic
    Capacite : Integer;

package Arbre_Genealogique is
    
    -- INSTANTIATION DES MODULES EXTERIEURS --
    
    -- Défini public pour avoir accès à T_Informations dans le programme principal
    package M_R is new Registre (Capacite);    

    -- IMPLEMENTATION DES TYPES PUBLICS --
    
    type T_Parent is (PERE, MERE);

    -- DECLARATION DES TYPES PRIVES --
    
    type T_AG is limited private;
    
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
    
    ID_Absent_Exception                 : Exception;      
    -- l'id n'est pas présent dans l'arbre
    
    Parent_Present_Exception            : Exception; 
    -- le parent à ajouter est déjà présent
    
    Suppression_Racine_Exception        : Exception; 
    -- l'utilisateur tente de supprimer le premier individu de l'arbre

    -- FONCTIONS ET PROCEDURES --
    
    -- Initialiser un arbre généalogique avec une personne
    -- Paramètres :
    --     Arbre        : l'arbre à initialiser
    --     Informations : Informations sur la personne
    procedure Initialiser (Arbre        : out T_AG ; 
                           Informations : in  M_R.T_Informations) with 
            Post => Taille (Arbre) = 1;


    -- Détruire l'arbre
    -- Paramètres :
    --     Arbre : l'arbre à détruire
    procedure Detruire (Arbre : in out T_AG);


    -- Ajouter un parent dans un arbre à partir de l'identifiant d'une personne
    -- Ajoute également un pointeur vers ce noeud dans le registre
    -- Paramètres :
    --     Arbre        : Arbre dans lequel on veut ajouter un parent
    --     id           : Identifiant de l'enfant
    --     Parent       : Parent à ajouter (PERE ou MERE)
    --     Informations : Informations à enregistrer dans le registre
    -- Exception :
    --     ID_Absent_Exception                 : l'id n'a pas été trouvé dans 
    --                                           l'arbre
    --     Parent_Present_Exception            : le parent à ajouter est déjà 
    --                                           présent
    --     Date_Fils_Incoherente_Exception     : La date de naissance du fils 
    --                                           est antérieure à celle de la 
    --                                           nouvelle entréee ou postérieur
    --                                           à sa date de décès
    --     Ordre_Date_Incoherent_Exception     : La date de décès est antérieure 
    --                                           à la date de naissance
    --     Date_Naissance_Impossible_Exception : La date de naissance ne 
    --                                           correspond pas à une date 
    --                                           réelle
    --     Date_Deces_Impossible_Exception     : La date de décès ne correspond 
    --                                           pas à une date réelle
    procedure Ajouter (Arbre        : in out T_AG ;
                       id           : in Integer ;
                       Parent       : in T_Parent ;
                       Informations : in M_R.T_Informations) with 
            Post => Taille (Arbre) = Taille (Arbre)'Old + 1;


    -- Modifier les informations sur une personne avec son identifiant
    -- Paramètres :
    --     Arbre : Arbre dans lequel on veut effectuer la modification
    --     id : Identifiant de la personne
    --     Informations : Informations à enregistrer dans le registre
    -- Exception :
    --     ID_Absent_Exception                 : l'id n'a pas été trouvé dans 
    --                                           l'arbre
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
    procedure Modifier (Arbre        : in out T_AG;
                        id           : in     Integer;
                        Informations : in     M_R.T_Informations) with
            Post => Taille (Arbre) = Taille (Arbre)'Old;


    -- Supprimer une personne et l'ensemble de ses parents dans l'arbre à
    -- partir de son identifiant
    -- Paramètres :
    --     Arbre : Arbre dans lequel on souhaite supprimer 
    --     id    : Identifiant de la personne
    -- Exception :
    --     ID_Absent_Exception : l'id n'a pas été trouvé
    procedure Supprimer (Arbre : in out T_AG; 
                         id : in Integer) with
            Post => Taille (Arbre) = Taille (Arbre)'Old - Taille (Arbre, id)'Old;



    -- Afficher l'arbre généalogique (sur gen générations si ce paramètre est
    -- donné) (à partir d'un individu désigné par son identifiant)
    -- Paramètres :
    --     Arbre                  : L'arbre à afficher
    --     gen (facultatif)       : Nombre de générations à afficher (illimité 
    --                              par défaut)
    --     id_racine (facultatif) : Identifiant de l'individu à partir duquel
    --                              afficher l'arbre (premier individu de 
    --                              l'arbre par défaut)
    -- Exception :
    --     ID_Absent_Exception : l'id n'a pas été trouvé
    procedure Afficher (Arbre     : in T_AG; 
                        gen       : in Integer := -1;
                        id_racine : in Integer := 1);


    -- Afficher l'ensemble des ancêtres d'une personne à une génération donné
    -- Paramètres :
    --     Arbre : L'arbre généalogique
    --     id    : Identifiant de la personne
    --     gen   : génération que l'on veut afficher
    -- Exception :
    --     ID_Absent_Exception : l'id n'a pas été trouvé
    procedure Afficher_Ancetres_Generation (Arbre : in T_AG ;
                                           id     : in Integer ;
                                           gen    : in Integer);


    -- Afficher l'ensemble des personnes qui n'ont qu'un parent dans l'arbre
    -- Paramètres :
    --     Arbre : L'arbre généalogique
    procedure Mono_Parent (Arbre : in T_AG);


    -- Afficher l'ensemble des personnes qui ont deux parents dans l'arbre
    -- Paramètres :
    --     Arbre : L'arbre généalogique
    procedure Deux_Parents (Arbre : in T_AG);


    -- Afficher l'ensemble des personnes qui n'ont aucun parent dans l'arbre
    -- Paramètres :
    --     Arbre : L'arbre généalogique
    procedure Sans_Parent (Arbre : in T_AG);


    -- Vérifier que deux individus n et m ont un ou plusieurs ancêtres
    -- homonymes et les afficher
    -- Paramètres :
    --     Arbre : L'arbre généalogique dans lequel rechercher
    --     id_n  : Identifiant de l'individu n
    --     id_m  : Identifiant de l'individu m
    -- Excepton :
    --     ID_Absent_Exception : l'id n'a pas été trouvé
    procedure Afficher_Homonymes (Arbre : in T_AG ; 
                                  id_n  : in Integer ;
                                  id_m  : in Integer);
    
    
    -- Obtenir la taille de l'arbre et vérifier que l'arbre binaire, le registre
    -- et la table de hachage des noeuds ont la même taille
    -- Paramètre :
    --     Arbre : Arbre dont on veut connaître la taille
    -- Retourne la taille de l'arbre
    function Taille (Arbre : in T_AG) 
                     return Integer;
    
    -- Obtenir la taille d'un sous arbre à partir d'un id
    -- Paramètres :
    --     Arbre : l'arbre contenant le sous arbre
    --     id    : l'id racine du sous arbre dont on veut connaître la taille
    -- Retourne la taille du sous arbre
    -- Excepton :
    --     ID_Absent_Exception : l'id n'a pas été trouvé
    function Taille (Arbre : in T_AG;
                     id : in Integer) 
                     return Integer;
    

    -- Afficher les informations de façon détaillée
    -- Paramètres :
    --     Arbre : L'arbre qui contient les informations
    --     id    : Identifiant de la personne dont on veut afficher les informations
    -- Exception :
    --     ID_Absent_Exception : l'id n'a pas été trouvé
    procedure Afficher_Informations_Detaillee (Arbre : in T_AG; 
                                               id    : in Integer);
    
    
    -- Obtenir les informations sur une personne
    -- Paramètres :
    --     Arbre : L'arbre dans lequel se trouve la personne
    --     id    : Identifiant de la personne
    -- Exceptions :
    --     ID_Absent_Exception : l'id n'a pas été trouvé
    function Obtenir_Informations (Arbre : in T_AG; 
                                   id    : in Integer) 
                                   return M_R.T_Informations;
    
    -- IMPLEMENTATION DES TYPES PRIVES --

private
    
    use M_R;

    package M_AB is new Arbre_Binaire (Integer);
    use M_AB;

    package M_TH is new Table_De_Hachage (Integer,
                                          T_AB,
                                          Capacite,
                                          F_Hachage);
    use M_TH;

    
    type T_AG is record
        AB       : T_AB;
        Registre : T_R;
        Noeuds   : T_TH;
    end record;
    
end Arbre_Genealogique;
