--------------------- IMPLEMENTATION DE ARBRE_GENEALOGIQUE ---------------------

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;


package body Arbre_Genealogique is
   
    -- VARIABLE GLOBALE --

    val_id : Integer := 1; -- Valeur actuelle de l'id à ajouter dans l'arbre

    -- FONCTIONS ET PROCEDURES --

    procedure Initialiser (Arbre        : out T_AG ; 
                           Informations : in  M_R.T_Informations) is
    begin
        val_id := 1;
        -- Initialiser l'arbre binaire et y ajouter la première personne
        M_AB.Initialiser (Arbre.AB);
        M_AB.Ajouter (Arbre.AB, val_id, PREMIER);
        -- Initialiser le registre et y ajouter les informations de la 
        -- première personne
        M_R.Initialiser (Arbre.Registre);
        M_R.Enregistrer (Arbre.Registre, val_id, Informations, 
                         Informations.Date_Naissance);
        -- Initialiser la table de hachage des noeuds et y ajouter le 
        -- premier noeud
        M_TH.Initialiser (Arbre.Noeuds);
        M_TH.Enregistrer (Arbre.Noeuds, val_id, Arbre.AB);
        -- Incrémenter la valeur de val_id
        val_id := val_id + 1;
    end Initialiser;


    procedure Detruire (Arbre : in out T_AG) is
    begin  
        -- Détruire l'arbre binaire
        M_AB.Detruire (Arbre.AB);
        -- Vider le registre
        M_R.Vider (Arbre.Registre);
        -- Vider la table des noeuds
        M_TH.Vider (Arbre.Noeuds);
    end Detruire;


    procedure Ajouter (Arbre        : in out T_AG ;
                       id           : in     Integer ;
                       Parent       : in     T_Parent ;
                       Informations : in     M_R.T_Informations) is
        Date_Naissance_Fils : T_Date;
        Noeud_Fils          : T_AB;
        Noeud_Parent        : T_AB;
    begin
        -- Ajouter au noeud fils un nouveau parent dans l'arbre
        Noeud_Fils := M_TH.Element(Arbre.Noeuds, id);
        if Parent = PERE then
            M_AB.Ajouter (Noeud_Fils, val_id, DROITE);
            Noeud_Parent := M_AB.Feuille (Noeud_Fils, DROITE);
        elsif Parent = MERE then
            M_AB.Ajouter (Noeud_Fils, val_id, GAUCHE);
            Noeud_Parent := M_AB.Feuille (Noeud_Fils, GAUCHE);
        end if;
        -- Enregistrer les informations du parent dans le registre
        Date_Naissance_Fils := M_R.Element(Arbre.Registre, id).Date_Naissance;
        M_R.Enregistrer (Arbre.Registre, val_id, Informations,
                         Date_Naissance_Fils);
        -- Enregistrer le noeud du parent ajouté dans la table des noeuds
        M_TH.Enregistrer (Arbre.Noeuds, val_id, Noeud_Parent);
        -- Incrémenter val_id
        val_id := val_id + 1;
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
        when M_AB.Noeud_Present_Exception
            => raise Parent_Present_Exception;
        when M_R.Date_Fils_Incoherente_Exception 
            => raise Date_Fils_Incoherente_Exception;
        when M_R.Ordre_Date_Incoherent_Exception 
            => raise Ordre_Date_Incoherent_Exception;
        when M_R.Date_Naissance_Impossible_Exception 
            => raise Date_Naissance_Impossible_Exception;
        when M_R.Date_Deces_Impossible_Exception
            => raise Date_Deces_Impossible_Exception;
    end Ajouter;


    procedure Modifier (Arbre        : in out T_AG ;
                        id           : in     Integer ;
                        Informations : in     M_R.T_Informations) is
        Noeud_A_Modifier    : T_AB;
        Noeud_Fils          : T_AB;
        id_Fils             : Integer;
        Date_Naissance_Fils : T_Date;
    begin
        Noeud_A_Modifier := M_TH.Element(Arbre.Noeuds, id);
        if id /= 1 then -- La personne à modifier n'est pas la 
                                   -- racine de l'arbre
            Noeud_Fils := M_AB.Racine(Noeud_A_Modifier);
            id_Fils := M_AB.Valeur (Noeud_Fils);
            Date_Naissance_Fils := 
                    M_R.Element(Arbre.Registre, id_Fils).Date_Naissance;
        else  -- La personne à modifier est la racine de l'arbre
            Date_Naissance_Fils := Informations.Date_Naissance;
        end if;
        -- Modifier les informations du registre
        M_R.Modifier (Arbre.Registre, id, Informations, Date_Naissance_Fils);
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
        when M_R.Date_Fils_Incoherente_Exception
            => raise Date_Fils_Incoherente_Exception;
        when M_R.Ordre_Date_Incoherent_Exception
            => raise Ordre_Date_Incoherent_Exception;
        when M_R.Date_Naissance_Impossible_Exception
            => raise Date_Naissance_Impossible_Exception;
        when M_R.Date_Deces_Impossible_Exception
            => raise Date_Deces_Impossible_Exception;
    end Modifier;

    
    procedure Supprimer_Informations (Arbre : in out T_AG; 
                                      AB    : in out T_AB) is
        id   : Integer;
        Pere : T_AB;
        Mere : T_AB;
    begin
        -- Supprimer les informations de la personne et de ses ancêtres
        if M_AB.Taille(AB) /= 0 then
            id := M_AB.Valeur (AB);
            M_R.Supprimer (Arbre.Registre, id);
            M_TH.Supprimer (Arbre.Noeuds, id);
            Pere := M_AB.Feuille (AB, DROITE);
            Mere := M_AB.Feuille (AB, GAUCHE);
            Supprimer_Informations (Arbre, Pere);
            Supprimer_Informations (Arbre, Mere);
        else
            null;
        end if;
    end Supprimer_Informations;

    
    procedure Supprimer (Arbre : in out T_AG; 
                         id    : in     Integer) is
        Noeud_A_Supprimer : T_AB;
    begin
        if id = 1 then
            raise Suppression_Racine_Exception;
        end if;
        Noeud_A_Supprimer := M_TH.Element (Arbre.Noeuds, id);
        -- Supprimer les informations de la personne et de ses ancêtres
        Supprimer_Informations (Arbre, Noeud_A_Supprimer);
        -- Supprimer les noeuds dans l'arbre binaire
        M_AB.Supprimer (Noeud_A_Supprimer);
    exception 
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Supprimer;


    procedure Afficher (Arbre     : in T_AG; 
                        gen       : in Integer := -1;
                        id_racine : in Integer := 1) is
        procedure Afficher_Informations (id : in Integer) is
            Informations : T_Informations;
        begin
            Informations := M_R.Element (Arbre.Registre, id);
            Put (id, 1);
            Put ("  ");
            Unbounded_IO.Put (Informations.Prenom);
            Put (" ");
            Unbounded_IO.Put (Informations.Nom);
        end Afficher_Informations;
        procedure Afficher_Test is new M_AB.Afficher (Afficher_Informations);
        
        Arbre_A_Afficher : T_AB;
    begin   
        Arbre_A_Afficher := M_TH.Element (Arbre.Noeuds, id_racine);
        Afficher_Test (Arbre_A_Afficher, gen);
    exception
        when Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end;
    
    
    -- Affiche l'id, le prénom et le nom d'une personne
    procedure Afficher_Nom (Informations : in T_Informations; 
                            id           : in Integer) is
        Prenom : Unbounded_String;
        Nom    : Unbounded_String;
    begin
        Prenom := Informations.Prenom;
        Nom := Informations.Nom;
        Put (id, 1); Put (" : ");
        Put (To_String(Prenom)); Put ("  "); Put (To_String(Nom));
        New_Line;
    end Afficher_Nom;
    
    
    procedure Recherche_Ancetres (Arbre : in T_AG; 
                                  AB    : in T_AB ;
                                  gen   : in Integer) is
        Pere         : T_AB;
        Mere         : T_AB;
        Informations : T_Informations;
        id           : Integer;
    begin
        if M_AB.Taille (AB) /= 0 then
            if gen = 0 then
                id := M_AB.Valeur(AB);
                Informations := M_R.Element (Arbre.Registre, id);
                Afficher_Nom (Informations, id);
            else
                Pere := M_AB.Feuille (AB, DROITE);
                Mere := M_AB.Feuille (AB, GAUCHE);
                Recherche_Ancetres (Arbre, Pere , gen - 1);
                Recherche_Ancetres (Arbre, Mere, gen - 1);
            end if;
        else
            null;
        end if;
    end Recherche_Ancetres;
    
    procedure Afficher_Ancetres_Generation (Arbre : in T_AG ;
                                            id    : in Integer ;
                                            gen   : in Integer) is
        Arbre_A_Parcourir : T_AB;
    begin
        Arbre_A_Parcourir := M_TH.Element (Arbre.Noeuds, id);
        Recherche_Ancetres (Arbre, Arbre_A_Parcourir , gen);
        New_Line;
    exception
        when Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Afficher_Ancetres_Generation;


    -- Afficher les individus avec nb_Parents connus dans AB
    procedure Recherche_n_Parents (Arbre      : in T_AG;
                                   AB         : in T_AB; 
                                   nb_Parents : in Integer) is
        Pere         : T_AB;
        Mere         : T_AB;
        Informations : T_Informations;
        id           : Integer;
    begin
        if Taille(AB) /= 0 then
            if M_AB.Taille_Niveau(AB, 1) = nb_Parents then
                id := M_AB.Valeur(AB);
                Informations := M_R.Element (Arbre.Registre, id);
                Afficher_Nom (Informations, id);
            else
                null;
            end if;
            Pere := M_AB.Feuille (AB, DROITE);
            Mere := M_AB.Feuille (AB, GAUCHE);
            Recherche_n_Parents (Arbre, Pere, nb_Parents);
            Recherche_n_Parents (Arbre, Mere, nb_Parents);
        else
            null;
        end if;
    end Recherche_n_Parents;
    
    
    procedure Mono_Parent (Arbre : in T_AG) is
    begin
        Recherche_n_Parents (Arbre, Arbre.AB, 1);
        New_Line;
    end Mono_Parent;


    procedure Deux_Parents (Arbre : in T_AG) is
    begin
        Recherche_n_Parents (Arbre, Arbre.AB, 2);
        New_Line;
    end Deux_Parents;


    procedure Sans_Parent (Arbre : in T_AG) is
    begin
        Recherche_n_Parents (Arbre, Arbre.AB, 0);
        New_Line;
    end Sans_Parent;


    -- Rechercher les ancetres d'un individus s'appelant Prenom Nom
    -- Retourne true si au moins un homonyme a été trouvé, false sinon
    function Recherche_Homonymes (AB       : in T_AB; 
                                  Prenom   : in Unbounded_String;
                                  Nom      : in Unbounded_String; 
                                  Registre : in T_R) 
                                  return Boolean is
        Pere          : T_AB;
        Mere          : T_AB;
        Informations  : T_Informations;
        id            : Integer;
        Prenom_Actuel : Unbounded_String;
        Nom_Actuel    : Unbounded_String;
        Trouve        : Boolean;
        Trouve_Pere   : Boolean;
        Trouve_Mere   : Boolean;
    begin
        Trouve := False;
        if Taille(AB) /= 0 then
            id := M_AB.Valeur (AB);
            Informations := M_R.Element (Registre, id);
            Prenom_Actuel := Informations.Prenom;
            Nom_Actuel := Informations.Nom;
            if Prenom_Actuel = Prenom and then Nom_Actuel = Nom then
                Afficher_Nom (Informations, id);
                Trouve := True;
            end if;
            Pere := M_AB.Feuille (AB, DROITE);
            Mere := M_AB.Feuille (AB, GAUCHE);
            Trouve_Pere := Recherche_Homonymes (Pere, Prenom, Nom, Registre);
            Trouve_Mere := Recherche_Homonymes (Mere, Prenom, Nom, Registre);
            -- Retourne true si un homonyme a été trouvé à cet appel ou dans
            -- les appels récursifs suivant
            return (Trouve or Trouve_Mere or Trouve_Pere);
        else
            return False;
        end if;
    end Recherche_Homonymes;
    
    
    -- Rechercher tous les noms et prénoms des ancêtres
    procedure Recherche_Noms (AB1 : in T_AB;
                              AB2 : in T_AB;
                              Registre : in T_R) is
        Pere         : T_AB;
        Mere         : T_AB;
        Informations : T_Informations;
        id           : Integer;
        Prenom       : Unbounded_String;
        Nom          : Unbounded_String;
    begin
        if Taille(AB1) /= 0 then
            id := M_AB.Valeur (AB1);
            Informations := M_R.Element (Registre, id);
            Prenom := Informations.Prenom;
            Nom := Informations.Nom;
            -- Rechercher les ancêtres de l'individu 2 s'appelant Prenom Nom
            if Recherche_Homonymes (AB2, Prenom, Nom, Registre) then
                Afficher_Nom (Informations, id);
            end if;
            Pere := M_AB.Feuille (AB1, DROITE);
            Mere := M_AB.Feuille (AB1, GAUCHE);
            Recherche_Noms (Pere, AB2, Registre);
            Recherche_Noms (Mere, AB2, Registre);
        else
            null;
        end if;
    end Recherche_Noms;
    
    procedure Afficher_Homonymes (Arbre : in T_AG ; 
                                  id_n  : in Integer ;
                                  id_m  : in Integer) is
        AB_n : T_AB;
        AB_m : T_AB;
    begin
        AB_n := M_TH.Element (Arbre.Noeuds, id_n);
        AB_m := M_TH.Element (Arbre.Noeuds, id_m);
        Recherche_Noms (AB_n, AB_m, Arbre.Registre);
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Afficher_Homonymes;
    
    
    function Taille (Arbre : in T_AG) 
                     return Integer is
        t : Integer;
    begin
        t := M_AB.Taille (Arbre.AB);
        -- Vérifier que la taille de l'arbre binaire est égale à celle du 
        -- registre
        pragma Assert (t = M_R.Taille (Arbre.Registre));
        -- Vérifier que la taille de l'arbre bianire est égale à celle de la
        -- table des noeuds
        pragma Assert (t = M_TH.Taille (Arbre.Noeuds));
        return t;
    end Taille;    
    
    function Taille (Arbre : in T_AG; 
                     id    : in Integer) 
                     return Integer is
    begin
        return M_AB.Taille (M_TH.Element (Arbre.Noeuds, id));
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Taille;

    
    procedure Afficher_Date (Date : in T_Date) is
    begin 
        if Date.Jour < 10 then
            Put ("0");
        end if;
        Put (Date.Jour, 1); Put ("/");
        if Date.Mois < 10 then
            Put ("0");
        end if;
        Put (Date.Mois, 1); Put("/");
        Put (Date.Annee, 1); New_Line;
    end Afficher_Date;

    procedure Afficher_Informations_Detaillee (Arbre : in T_AG; 
                                               id : in Integer) is
        Informations : T_Informations;
    begin
        Informations := M_R.Element (Arbre.Registre, id);
        Put ("Prenom : "); Put (Informations.Prenom); New_Line;
        Put ("Nom : "); Put (Informations.Nom); New_Line;
        Put ("Sexe : "); Put (Informations.Sexe'Image); New_Line;
        Put ("Date de naissance : ");
        Afficher_Date (Informations.Date_Naissance);
        Put ("Lieu de naissance : "); Put (Informations.Lieu_Naissance);
        New_Line;
        if Informations.Date_Deces.Annee < 10000 then
            Put ("Date de décès : ");
            Afficher_Date (Informations.Date_Deces);
        end if;
        New_Line;
    exception
        when M_R.Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end;

    
    function Obtenir_Informations (Arbre : in T_AG; 
                                   id    : in Integer) 
                                   return M_R.T_Informations is
    begin
        return M_R.Element (Arbre.Registre, id);
    exception
        when M_R.Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Obtenir_Informations;
            
            
end Arbre_Genealogique;
