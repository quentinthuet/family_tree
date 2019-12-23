------------------------ SPECIFICATION DE ARBRE_BINAIRE ------------------------
-- Ce module définit un type arbre binaire T_AB et les opérations associées.

-- PARAMETRES DE GENERICITE --

generic
    type T_Valeur is private;

package Arbre_Binaire is

    -- IMPLEMENTATION DES TYPES PUBLICS --

    type T_Direction is (GAUCHE, DROITE, PREMIER);

    -- DECLARATION DES TYPES PRIVES --

    type T_AB is private;

    -- EXCEPTIONS --

    Noeud_Present_Exception : exception;
    -- Noeud déjà présent dans l'arbre

    Noeud_Absent_Exception  : exception;
    -- Noeud absent de l'arbre

    Non_Racine_Exception    : exception;
    -- Usage de Detruire sur un noeud autre que le noeud racine

    -- FONCTIONS ET PROCEDURES --

    -- Initialiser un arbre binaire
    -- Paramètres :
    --     Arbre : l'arbre à initialiser
    procedure Initialiser (Arbre : out T_AB) with
            Post => Taille (Arbre) = 0;

    -- Détruire l'arbre (ne peut s'appliquer que sur le noeud racine)
    -- Paramètres :
    --     Arbre         : L'arbre à détruire
    --     Premier_Appel : (facultatif) Permet de s'assurer qu'on ne vérifie
    --                     qu'une seule fois que l'appel de la fonction est bien
    --                     sur le noeud racine de l'arbre
    -- Exception :
    --     Non_Racine_Exception : Arbre ne pointe pas vers la racine de l'arbre
    procedure Detruire (Arbre         : in out T_AB;
                        Premier_Appel : in     Boolean := True);

    -- Ajouter une valeur dans l'arbre à droite ou à gauche d'un noeud valeur
    -- Paramètres :
    --     Noeud           : Pointeur vers le noeud de l'arbre à ajouter la
    --                       valeur
    --     Nouvelle_Valeur : Donnée à ajouter
    --     Direction       : La direction dans laquelle ajouter la valeur
    -- Exception :
    --     Noeud_Present_Exception : On tente d'ajouter un noeud qui a déjà été
    --                               ajouté précédemment
    --     Noeud_Absent_Exception  : Noeud n'existe pas
    procedure Ajouter (Noeud           : in out T_AB ;
                       Nouvelle_Valeur : in     T_Valeur ;
                       Direction       : in     T_Direction) with
            Post => Taille (Noeud) = Taille (Noeud)'Old + 1;

    -- Ajouter un noeud dans l'arbre à droite ou à gauche d'un noeud valeur
    -- Paramètres :
    --     Noeud         : Pointeur vers le noeud de l'arbre à ajouter la valeur
    --     Nouveau_Noeud : Noeud à ajouter
    --     Direction     : La direction dans laquelle ajouter la valeur
    -- Exception :
    --     Noeud_Present_Exception : On tente d'ajouter un noeud qui a déjà été
    --                               ajouté précédemment
    --     Noeud_Absent_Exception  : Noeud n'existe pas
    procedure Ajouter (Noeud         : in out T_AB ;
                       Nouveau_Noeud : in     T_AB ;
                       Direction     : in     T_Direction);

    -- Supprimer un noeud et tous ses fils dans l'arbre
    -- Paramètres :
    --     Noeud : Pointeur vers le noeud que l'on veut supprimer
    -- Exception :
    --     Noeud_Absent_Exception : Noeud n'existe pas
    procedure Supprimer (Noeud : in out T_AB);

    -- Supprimer un noeud
    -- Paramètres :
    --     Noeud : Pointeur vers le noeud que l'on veut supprimer
    -- Exception :
    --     Noeud_Absent_Exception : Noeud n'existe pas
    procedure Supprimer_Noeud (Noeud : in out T_AB);

    -- Afficher l'arbre en affichant chaque noeud avec la procédure
    -- Afficher_Noeud (sur Profondeur_Max niveaux de profondeur si ce paramètre
    -- est donné)
    -- Paramètres :
    --     Noeud          : Pointeur vers le noeud à partir duquel on veut
    --                      afficher
    --     Profondeur_Max : (facultatif) Profondeur jusqu'à laquelle afficher
    --                      l'arbre, illimitée par défaut
    generic
        with procedure Afficher_Noeud (Valeur : in T_Valeur);
    procedure Afficher (Noeud          : in T_AB;
                        Profondeur_Max : in Integer := -1);

    -- Obtenir le nombre d'éléments sous un noeud
    -- Paramètres :
    --     Noeud : Pointeur vers le noeud choisi
    -- Retourne :
    --     La taille de l'arbre à partir de Noeud
    function Taille (Noeud : in T_AB)
                     return Integer;

    -- Obtenir la valeur d'un noeud
    -- Paramètres :
    --      Noeud : Le noeud dont on veut connaitre la valeur
    -- Retourne :
    --      La valeur du noeud
    -- Exception :
    --      Noeud_Absent_Exception : Noeud n'existe pas
    function Valeur (Noeud : in T_AB)
                     return T_Valeur;

    -- Obtenir le nombre d'éléments sous un noeud à un niveau donnée
    -- Paramètres :
    --     Noeud  : Pointeur vers le noeud choisi
    --     Niveau : Le niveau dont on veut savoir la taille
    function Taille_Niveau (Noeud  : in T_AB ;
                            Niveau : in Integer)
                            return Integer;

    -- Obtenir une feuille de l'arbre à partir d'un noeud
    -- Paramètres :
    --     Noeud     : Noeud dont on veut obtenir la feuille
    --     Direction : Direction de la feuille
    -- Exception :
    --     Noeud_Absent_Exception : Noeud n'existe pas
    function Feuille (Noeud     : in T_AB;
                      Direction : in T_Direction)
                      return T_AB;

    -- Obtenir la racine d'un noeud
    -- Paramètres
    --     Noeud : Noeud dont on veut obtenir la racine (à 1 niveau supérieur)
    -- Exception :
    --     Noeud_Absent_Exception : Noeud n'existe pas
    function Racine (Noeud : in T_AB)
                     return T_AB;

    -- Obtenir la profondeur d'un arbre
    -- Paramètre
    --     Arbre : Arbre dont on veut connaitre la profondeur
    function Profondeur (Arbre : in T_AB)
                         return Integer;

    -- Tester le module
    -- Paramètres :
    --     Arbre     : Un arbre non initialisé à tester
    --     Val_Test* : Les valeurs à insérer dans l'arbre pour le test
    generic
        with procedure Afficher_Test (Arbre : in T_AB;
                                      Profondeur_Max : Integer := -1);
    procedure Test (Arbre : in out T_AB;
                    Val_Test1 : in T_Valeur;
                    Val_Test2 : in T_Valeur;
                    Val_Test3 : in T_Valeur;
                    Val_Test4 : in T_Valeur;
                    Val_Test5 : in T_Valeur);

    -- IMPLEMENTATION DES TYPES PRIVES --

private

    type T_Noeud;

    type T_AB is access T_Noeud;

    type T_Noeud is
        record
            Valeur            : T_Valeur; -- La valeur stockée au noeud
            Sous_Arbre_Gauche : T_AB;     -- Le pointeur sur la feuille gauche
            Sous_Arbre_Droit  : T_AB;     -- Le pointeur sur la feuille droite
            Parent            : T_AB;     -- Le pointeur sur le noeud parent
        end record;

end Arbre_Binaire;
