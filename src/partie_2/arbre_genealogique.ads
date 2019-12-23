---------------------- SPECIFICATION DE ARBRE_GENEALOGIQUE ---------------------
-- Ce module définit un type arbre généalogique T_AG et les opérations associées

with Table_De_Hachage;
with Registre;
with Arbre_Binaire;
with Liste_Chainee;

-- PARAMETRES DE GENERICITE --

generic
    Capacite : Integer;           -- La capacité maximale des tables de hachages 
                                  -- pour un accès en temps constant (elle peut
                                  -- donc être dépassée)

package Arbre_Genealogique is
 
    -- INSTANTIATION DES MODULES EXTERIEURS --
    
    package M_R is new Registre (Capacite);
    
    use M_R;

    package M_AB is new Arbre_Binaire (Integer);
    use M_AB;

    package M_TH is new Table_De_Hachage (Integer,
                                          T_AB,
                                          Capacite,
                                          F_Hachage);
    use M_TH;
    
    package M_LC_Integer is new Liste_Chainee (Integer);
    use M_LC_Integer;
    
    package M_LC_AB is new Liste_Chainee (T_AB);
    use M_LC_AB;
    
    -- IMPLEMENTATION DES TYPES PUBLICS --
    
    -- Type Parent
    type T_Parent is (PERE, MERE);
    
    
    -- Type Arbre Généalogique
    type T_AG is record -- Le type Arbre Généalogique contient :
        AB : M_LC_AB.T_LC;             -- Une liste d'arbres binaires contenant  
                                       -- la structure de la famille
        Id_Racine : M_LC_Integer.T_LC; -- Les identifiants des racines des 
                                       -- arbres
        Registre : T_R;                -- Un registre unique contenant toutes
                                       -- les informations sur chaque individus
        Noeuds   : T_TH;               -- Une liste unique qui contient pour
                                       -- chaque individu un pointeur sur le 
                                       -- noeud d'arbrebinaire sur lequel il se 
                                       -- trouve
    end record;
    
    -- EXCEPTIONS --
    
    Date_Fils_Incoherente_Exception      : Exception; 
    -- La date de naissance du fils est antérieure à celle de la nouvelle entrée 
    -- ou postérieur à sa date
    
    Ordre_Date_Incoherent_Exception      : Exception; 
    -- La date de décès est antérieure à la date de naissance
    
    Date_Naissance_Impossible_Exception  : Exception; 
    -- La date de naissance ne correspond pas à une date réelle
    
    Date_Deces_Impossible_Exception      : Exception; 
    -- La date de décès ne correspond pas à une date réelle
    
    Cle_Absente_Exception                : Exception; 
    -- L'id entrée est déjà présent
    
    Cle_Presente_Exception               : Exception; 
    -- L'id entrée n'est pas encore présent
    
    ID_Absent_Exception                  : Exception;      
    -- L'id n'est pas présent dans l'arbre
    
    Parent_Present_Exception             : Exception; 
    -- Le parent à ajouter est déjà présent
    
    Suppression_Racine_Exception         : Exception; 
    -- L'utilisateur tente de supprimer le premier individu de l'arbre

    -- FONCTIONS ET PROCEDURES --
    
    -- Initialiser un arbre généalogique avec une personne
    -- Paramètres :
    --     Arbre                   : L'arbre à initialiser
    --     Num_Arbre               : Le numéro de l'arbre à initaliser
    --     Informations            : Informations sur la première personne de 
    --                               l'arbre
    --     Premiere_initialisation : (facultatif) Est-ce que c'est la première 
    --                               initialisation de cette arbre ? (Non par 
    --                               défaut)
    procedure Initialiser (Arbre                   : out T_AG; 
                           Num_Arbre               : in  Integer;
                           Informations            : in  M_R.T_Informations;
                           Premiere_Initialisation : in  Boolean := False) with 
            Post => Taille (Arbre, Element (Arbre.Id_Racine, Num_Arbre)) = 1; 
    
    -- Détruire entièrement l'arbre généalogique
    -- Paramètres :
    --     Arbre     : L'arbre à détruire
    procedure Detruire (Arbre : in out T_AG);

    -- Ajouter un parent dans un arbre à partir de l'identifiant d'une personne
    -- Ajoute également un pointeur vers ce noeud dans le registre
    -- Paramètres :
    --     Arbre        : Arbre dans lequel on veut ajouter un parent
    --     id           : Identifiant de l'enfant
    --     Parent       : Parent à ajouter (PERE ou MERE)
    --     Informations : Informations à enregistrer dans le registre
    -- Exceptions :
    --     ID_Absent_Exception                 : L'id n'a pas été trouvé dans 
    --                                           l'arbre
    --     Parent_Present_Exception            : le parent à ajouter est déjà 
    --                                           présent
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
    procedure Ajouter (Arbre        : in out T_AG;
                       id           : in     Integer;
                       Parent       : in     T_Parent;
                       Informations : in     M_R.T_Informations);
    
    -- Ajouter un frère ou une soeur à un individu
    -- Paramètres :
    --     Arbre        : Arbre dans lequel on veut ajouter un frère / une soeur
    --     id           : Identifiant de l'individu auquel on veut ajouter un
    --                    frère / une soeur
    --     Informations : Informations à enregistrer dans le registre
    -- Exceptions :
    --     ID_Absent_Exception  : L'id n'a pas été trouvé dans l'arbre ou les 
    --                            parents n'existent pas 
    procedure Ajouter_Frere (AG           : in out T_AG ;
                             id           : in Integer ;
                             Informations : in T_Informations); 
    
    -- Ajouter un demi-frère ou une demi-soeur à un individu
    -- Paramètres :
    --     Arbre        : Arbre dans lequel on veut ajouter quelqu'un
    --     id           : Identifiant de l'individu auquel on veut ajouter un
    --                    demi-frère / une demi-soeur
    --     Informations : Informations à enregistrer dans le registre
    --     Parent       : Parent en commun (PERE ou MERE)
    -- Exceptions :
    --     ID_Absent_Exception  : L'id n'a pas été trouvé dans l'arbre ou le 
    --                            parent commun n'existe pas 
    procedure Ajouter_Demi_Frere (AG           : in out T_AG ;
                                  id           : in Integer ;
                                  Informations : in T_Informations ;
                                  Parent       : in T_Parent);  
    
    -- Modifier les informations sur une personne avec son identifiant
    -- Paramètres :
    --     Arbre        : Arbre dans lequel on veut effectuer la modification
    --     id           : Identifiant de la personne
    --     Informations : Informations à enregistrer dans le registre
    -- Exceptions :
    --     ID_Absent_Exception                 : L'id n'a pas été trouvé dans 
    --                                           l'arbre
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
    procedure Modifier (Arbre        : in out T_AG ;
                        id           : in     Integer ;
                        Informations : in     M_R.T_Informations);

    -- Supprimer une personne et l'ensemble de ses parents dans l'arbre à
    -- partir de son identifiant
    -- Paramètres :
    --     Arbre : Arbre dans lequel on veut effectuer la suppression
    --     id    : Identifiant de la personne
    --     Num_Arbre : Le numéro de l'arbre dans lequel on veut supprimer
    -- Exception :
    --     ID_Absent_Exception : L'id n'a pas été trouvé
    procedure Supprimer (Arbre     : in out T_AG ; 
                         id        : in     Integer ;
                         Num_Arbre : in Integer);

    -- Afficher l'arbre généalogique (sur gen générations si ce paramètre est
    -- donné) (à partir d'un individu désigné par son identifiant)
    -- Paramètres :
    --     Arbre     : L'arbre à afficher
    --     Num_Arbre : Le numéro de l'arbre à afficher
    --     gen       : (facultatif) Nombre de générations à afficher (illimité 
    --                              par défaut)
    --     id_racine : (facultatif) Identifiant de l'individu à partir duquel
    --                              afficher l'arbre (premier individu de 
    --                              l'arbre par défaut)
    -- Exception :
    --     ID_Absent_Exception : L'id n'a pas été trouvé
    procedure Afficher (Arbre     : in T_AG; 
                        Num_Arbre : in Integer;
                        gen       : in Integer := -1;
                        id_racine : in Integer := 0);

    -- Afficher l'ensemble des ancêtres d'une personne à une génération donné
    -- Paramètres :
    --     Arbre : L'arbre généalogique
    --     id    : Identifiant de la personne
    --     gen   : Génération que l'on veut afficher
    -- Exception :
    --     ID_Absent_Exception : L'id n'a pas été trouvé
    procedure Afficher_Ancetres_Generation (Arbre : in T_AG ;
                                            id    : in Integer;
                                            gen   : in Integer);

    -- Afficher l'ensemble des personnes qui n'ont qu'un parent dans l'arbre
    -- Paramètres :
    --     Arbre     : L'arbre généalogique
    --     Num_Arbre : Le numéro de l'arbre dans lequel rechercher
    procedure Mono_Parent (Arbre     : in T_AG; 
                           Num_Arbre : in Integer);

    -- Afficher l'ensemble des personnes qui ont deux parents dans l'arbre
    -- Paramètres :
    --     Arbre     : L'arbre généalogique
    --     Num_Arbre : Le numéro de l'arbre dans lequel rechercher
    procedure Deux_Parents (Arbre     : in T_AG; 
                            Num_Arbre : in Integer);

    -- Afficher l'ensemble des personnes qui n'ont aucun parent dans l'arbre
    -- Paramètres :
    --     Arbre     : L'arbre généalogique
    --     Num_Arbre : Le numéro de l'arbre dans lequel rechercher
    procedure Sans_Parent (Arbre     : in T_AG; 
                           Num_Arbre : in Integer);

    -- Vérifier que deux individus n et m ont un ou plusieurs ancêtres
    -- homonymes et les afficher
    -- Paramètres :
    --     Arbre : L'arbre généalogique dans lequel rechercher
    --     id_n  : Identifiant de l'individu n
    --     id_m  : Identifiant de l'individu m
    -- Excepton :
    --     ID_Absent_Exception : L'id n'a pas été trouvé
    procedure Afficher_Homonymes (Arbre : in T_AG; 
                                  id_n  : in Integer;
                                  id_m  : in Integer);
    
    -- Obtenir la taille d'un sous arbre à partir d'un id
    -- Paramètres :
    --     Arbre : L'arbre contenant le sous arbre
    --     id    : L'id racine du sous arbre dont on veut connaître la taille
    -- Retourne la taille du sous arbre
    -- Exception :
    --     ID_Absent_Exception : L'id n'a pas été trouvé
    function Taille (Arbre : in T_AG; 
                     id    : in Integer) 
                     return Integer;
  
    -- Afficher les informations de façon détaillée
    -- Paramètres :
    --     Arbre : L'arbre qui contient les informations
    --     id    : Identifiant de la personne dont on veut afficher les informations
    -- Exception :
    --     ID_Absent_Exception : L'id n'a pas été trouvé
    procedure Afficher_Informations_Detaillee (Arbre : in T_AG; 
                                               id    : in Integer);
    
    -- Obtenir les informations sur une personne
    -- Paramètres :
    --     Arbre : L'arbre dans lequel se trouve la personne
    --     id    : Identifiant de la personne
    -- Exception :
    --     ID_Absent_Exception : L'id n'a pas été trouvé
    function Obtenir_Informations (Arbre : in T_AG; 
                                   id    : in Integer)
                                   return M_R.T_Informations;
    
    -- Lier deux arbres par un individu commun
    -- Paramètres :
    --     Arbre         : L'arbre dans lequel on veut effectuer le lien
    --     Id_Courant    : L'id depuis on veut effectuer le lien
    --     Id_A_Lier     : L'id que l'on veut lier au premier
    procedure Lier (Arbre      : in out T_AG; 
                    Id_Courant : in     Integer ; 
                    Id_A_Lier  : in     Integer);

end Arbre_Genealogique;
