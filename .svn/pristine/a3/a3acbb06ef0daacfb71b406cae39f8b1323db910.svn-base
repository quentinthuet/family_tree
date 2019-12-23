--------------------- IMPLEMENTATION DE ARBRE_GENEALOGIQUE ---------------------

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;


package body Arbre_Genealogique is
    
    -- VARIABLE GLOBALE --
    
    val_id : Integer := 1; -- Valeur de l'identifiant du prochain individu à 
    -- ajouter
    
    -- FONCTIONS ET PROCEDURES --
    
    procedure Initialiser (Arbre                   : out T_AG ; 
                           Num_Arbre               : in  Integer;
                           Informations            : in  M_R.T_Informations;
                           Premiere_Initialisation : in  Boolean := False) is
        New_AB : T_AB;
    begin
        
        -- Initialiser l'arbre binaire et y ajouter la première personne
        M_AB.Initialiser (New_AB);
        M_AB.Ajouter (New_AB, val_id, PREMIER);
        Enregistrer (Arbre.AB, New_AB);
        
        -- Ajouter l'indice racine de l'arbre à la liste
        Enregistrer(Arbre.Id_Racine, val_id);
        
        -- Initialiser le registre et y ajouter les informations de la 
        -- première personne
        if Premiere_Initialisation then
            M_R.Initialiser (Arbre.Registre);
        else
            null;
        end if;
        M_R.Enregistrer (Arbre.Registre, val_id, Informations, 
                         Informations.Date_Naissance);
        
        -- Initialiser la table de hachage des noeuds et y ajouter le 
        -- premier noeud        
        if Premiere_Initialisation then
            M_TH.Initialiser (Arbre.Noeuds);
        else
            null;
        end if;
        M_TH.Enregistrer (Arbre.Noeuds, val_id, Element (Arbre.AB, Num_Arbre));
        
        -- Incrémenter la valeur de val_id
        val_id := val_id + 1;
        
    end Initialiser;


    procedure Detruire (Arbre : in out T_AG) is
        
        AB : T_AB; -- Arbre à détruire
        
    begin  
        
        -- Vider le registre et la table des noeuds
        M_R.Vider (Arbre.Registre);
        M_TH.Vider (Arbre.Noeuds);
        -- Détruire l'arbre principal
        AB := M_LC_AB.Element (Arbre.AB, 1);
        M_AB.Detruire (AB);
        for i in 2..M_LC_AB.Taille (Arbre.AB) loop
            AB := M_LC_AB.Element (Arbre.AB, i);
            M_AB.Supprimer_Noeud (AB);
        end loop;
        
        -- Vider les listes chainées
        M_LC_AB.Vider (Arbre.AB);
        M_LC_Integer.Vider (Arbre.Id_Racine);
        
    end Detruire;


    procedure Ajouter (Arbre        : in out T_AG;
                       id           : in     Integer;
                       Parent       : in     T_Parent;
                       Informations : in     M_R.T_Informations) is
        
        Date_Naissance_Fils : T_Date; -- La date de naissance de la personne
                                      -- depuis laquelle on ajoute le parent
        Noeud_Fils          : T_AB;   -- Le noeud où est stocké
                                      -- l'identifiant de la personne depuis
                                      -- laquelle on ajoute le parent
        Noeud_Parent        : T_AB;   -- Le noeud où est stocké
                                      -- l'identifiant du parent
        
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
    

    procedure Ajouter_Frere (AG           : in out T_AG;
                             id           : in Integer;
                             Informations : in T_Informations) is
        
        Nb_Arbres : Integer; -- Nombres d'arbres actuel
        AB        : T_AB;    -- Noeud de l'arbre binaire correspondant à l'id
        id_Pere   : Integer; -- Identifiant de la mère de l'individu
        id_Mere   : Integer; -- Identifiant du père de l'individu
        
    begin
        
        -- Initialiser un nouvel arbre
        Nb_Arbres := M_LC_AB.Taille (AG.AB);
        Initialiser (AG, Nb_Arbres + 1, Informations);
        
        -- Lier la nouvelle personne à ses parents
        AB := M_TH.Element (AG.Noeuds, id);
        -- Le nouvel individu à pour id val_id - 1
        id_Pere := M_AB.Valeur (M_AB.Feuille (AB, DROITE)); 
        Lier(AG, val_id-1, id_Pere); -- Ajouter son père
        
        id_Mere := M_AB.Valeur (M_AB.Feuille (AB, GAUCHE));
        Lier(AG, val_id-1, id_Mere); -- Ajouter sa mère
        
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
        when M_AB.Noeud_Absent_Exception
            => raise ID_Absent_Exception;
    end Ajouter_Frere;
    
    
    procedure Ajouter_Demi_Frere (AG           : in out T_AG;
                                  id           : in     Integer;
                                  Informations : in     T_Informations;
                                  Parent       : in     T_Parent) is
        
        Nb_Arbres : Integer; -- Nombres d'arbres actuel
        AB        : T_AB;    -- Noeud de l'arbre binaire correspondant à l'id
        id_Pere   : Integer; -- Identifiant de la mère de l'individu
        id_Mere   : Integer; -- Identifiant du père de l'individu
        
    begin
        
        -- Initialiser un nouvel arbre
        Nb_Arbres := M_LC_AB.Taille (AG.AB);
        Initialiser (AG, Nb_Arbres + 1, Informations);
        
        -- Lier la nouvelle personne à ses parents
        AB := M_TH.Element (AG.Noeuds, id);
        
        if Parent = PERE then
            id_Pere := M_AB.Valeur (M_AB.Feuille (AB, DROITE));
            Lier(AG, val_id-1, id_Pere); -- Ajouter son père
        else
            id_Mere := M_AB.Valeur (M_AB.Feuille (AB, GAUCHE));
            Lier(AG, val_id-1, id_Mere); -- Ajouter sa mère
        end if;
        
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
        when M_AB.Noeud_Absent_Exception
            => raise ID_Absent_Exception;
    end Ajouter_Demi_Frere;


    procedure Modifier (Arbre        : in out T_AG ;
                        id           : in     Integer ;
                        Informations : in     M_R.T_Informations) is
        
        Noeud_A_Modifier    : T_AB;    -- Le noeud où est stocké l'id
        Noeud_Fils          : T_AB;    -- Le noeud où est stocké l'id du 
                                       -- fils de l'individu à modifier
        id_Fils             : Integer; -- L'id du fils de l'individu à modifier
        Date_Naissance_Fils : T_Date;  -- La date de naissance du fils de 
                                       -- l'individu à modifier
                                       
    begin
        
        Noeud_A_Modifier := M_TH.Element(Arbre.Noeuds, id);
        
        if id /= 1 then 
            -- La personne à modifier n'est pas la racine de l'arbre
            Noeud_Fils := M_AB.Racine(Noeud_A_Modifier);
            id_Fils := M_AB.Valeur (Noeud_Fils);
            Date_Naissance_Fils := 
                    M_R.Element(Arbre.Registre, id_Fils).Date_Naissance;
        else  
            -- La personne à modifier est la racine de l'arbre
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
                                      AB    : in     T_AB) is
        
        id   : Integer; -- L'id de l'individu dont on veut supprimer les
        -- informations
        Pere : T_AB;    -- Le noeud où est stocké l'id du père de l'individu
        Mere : T_AB;    -- Le noeud où est stocké l'id de la mère de l'individu
        
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

    
    procedure Supprimer (Arbre     : in out T_AG ; 
                         id        : in     Integer ;
                         Num_Arbre : in Integer) is
        
        Noeud_A_Supprimer : T_AB; -- Le noeud où est stocké l'id
        
    begin
        
        -- Si la personne à supprimer est la racine d'un arbre
        if id = M_LC_Integer.Element (Arbre.Id_Racine, Num_Arbre) then
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
                        Num_Arbre : in Integer;
                        gen       : in Integer := -1;
                        id_racine : in Integer := 0) is
        
        -- Afficher les informations relatives à un individu
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
        
        Arbre_A_Afficher : T_AB; -- L'arbre binaire à afficher
        
    begin   
        
        if id_racine = 0 then
            Arbre_A_Afficher := M_TH.Element (Arbre.Noeuds, Valeur (Element (Arbre.AB, Num_Arbre)));
        else 
            Arbre_A_Afficher := M_TH.Element (Arbre.Noeuds, id_racine);
        end if;
        Afficher_Test (Arbre_A_Afficher, gen);
        
    exception
        when Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Afficher;
    
    
    -- Affiche l'id, le prénom et le nom d'une personne
    procedure Afficher_Nom (Informations : in T_Informations; 
                            id           : in Integer) is
        
        Prenom : Unbounded_String; -- Le prénom à afficher
        Nom    : Unbounded_String; -- Le nom à afficher 
        
    begin
        
        Prenom := Informations.Prenom;
        Nom := Informations.Nom;
        Put (id, 1); 
        Put (" : ");
        Put (To_String(Prenom)); 
        Put ("  "); 
        Put (To_String(Nom));
        New_Line;
        
    end Afficher_Nom;
    
    
    -- Afficher les ancêtres de génération gen d'un individu
    procedure Recherche_Ancetres (Arbre : in T_AG; 
                                  AB    : in T_AB ;
                                  gen   : in Integer) is
        
        Pere         : T_AB;           -- Père de l'individu
        Mere         : T_AB;           -- Mère de l'individu
        Informations : T_Informations; -- Informations sur l'individu
        id           : Integer;        -- Id de l'individu
        
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
    
    
    procedure Afficher_Ancetres_Generation (Arbre : in T_AG;
                                            id    : in Integer;
                                            gen   : in Integer) is
         
        Arbre_A_Parcourir : T_AB; -- L'arbre dont la racine contient la valeur
        -- id
        
    begin
        
        Arbre_A_Parcourir := M_TH.Element (Arbre.Noeuds, id);
        if Profondeur (Arbre_A_Parcourir) <= gen then
            Put_Line ("Aucun ancêtre à cette génération");
        else
            Recherche_Ancetres (Arbre, Arbre_A_Parcourir , gen);
        end if;
        New_Line;
        
    exception
        when Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Afficher_Ancetres_Generation;


    -- Afficher les individus avec nb_Parents connus dans AB
    -- Renvoie vrai si au moins un individu trouvé
    function Recherche_n_Parents (Arbre      : in T_AG;
                                  AB         : in T_AB; 
                                  nb_Parents : in Integer)
                                  return Boolean is
        
        Pere         : T_AB;             -- Le père de l'individu dont l'id est
        -- stocké au noeud AB
        Mere         : T_AB;             -- Le père de l'individu dont l'id est
        -- stocké au noeud AB
        Informations : T_Informations;   -- Les informations de l'individu id
        id           : Integer;          -- L'id de l'individu dont l'id est 
        -- stocké au noeud AB
        Trouve       : Boolean := False; -- Est-ce qu'un individu a été trouvé
    begin
        
        if Taille(AB) /= 0 then
            if M_AB.Taille_Niveau(AB, 1) = nb_Parents then
                id := M_AB.Valeur(AB);
                Informations := M_R.Element (Arbre.Registre, id);
                Afficher_Nom (Informations, id);
                Trouve := True;
            else
                null;
            end if;
            Pere := M_AB.Feuille (AB, DROITE);
            Mere := M_AB.Feuille (AB, GAUCHE);
            Trouve := (Trouve or Recherche_n_Parents (Arbre, Pere, nb_Parents));
            Trouve := (Trouve or Recherche_n_Parents (Arbre, Mere, nb_Parents));
        else
            null;
        end if;
        return Trouve;
    end Recherche_n_Parents;
    
    
    procedure Mono_Parent (Arbre     : in T_AG;
                           Num_Arbre : in Integer) is
    begin
        
        if not (Recherche_n_Parents (Arbre, Element (Arbre.AB, Num_Arbre), 1)) 
        then
            Put_Line ("Aucun individu ne correspond aux critères de recherche");
        else 
            null;
        end if;
        New_Line;
        
    end Mono_Parent;


    procedure Deux_Parents (Arbre     : in T_AG;
                            Num_Arbre : in Integer) is
    begin
        
        if not (Recherche_n_Parents (Arbre, Element (Arbre.AB, Num_Arbre), 2)) 
        then
            Put_Line ("Aucun individu ne correspond aux critères de recherche");
        else 
            null;
        end if;
        New_Line;
        
    end Deux_Parents;


    procedure Sans_Parent (Arbre     : in T_AG;
                           Num_Arbre : in Integer) is
    begin
        
        if not (Recherche_n_Parents (Arbre, Element (Arbre.AB, Num_Arbre), 0)) 
        then
            Put_Line ("Aucun individu ne correspond aux critères de recherche");
        else 
            null;
        end if;
        New_Line;
        
    end Sans_Parent;


    -- Rechercher les ancetres d'un individus s'appelant Prenom Nom
    -- Retourne true si au moins un homonyme a été trouvé, false sinon
    function Recherche_Homonymes (AB       : in T_AB; 
                                  Prenom   : in Unbounded_String;
                                  Nom      : in Unbounded_String; 
                                  Registre : in T_R) 
                                  return Boolean is
        
        Pere          : T_AB;             -- Le noeud où est stocké l'id du père
        -- de l'individu id
        Mere          : T_AB;             -- Le noeud où est stocké l'id de la 
        -- mère de l'individu id
        Informations  : T_Informations;   -- Les informations de l'individu id
        id            : Integer;          -- L'id stocké au noeud AB
        Prenom_Actuel : Unbounded_String; -- Le prénom de l'individu id
        Nom_Actuel    : Unbounded_String; -- Le nom de l'individu id
        Trouve        : Boolean;          -- Est-ce qu'un homonyme a été 
        -- trouvé ?
        Trouve_Pere   : Boolean;          -- Est-ce qu'un homonyme a été 
        -- trouvé dans l'arbre du père ?
        Trouve_Mere   : Boolean;          -- Est-ce qu'un homonyme a été 
        -- trouvé dans l'arbre de la mère ?
        
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
    -- Renvoie vrai si un individu a été trouvé
    function Recherche_Noms (AB1      : in T_AB; 
                             AB2      : in T_AB; 
                             Registre : in T_R) 
                             return Boolean is
        
        Pere         : T_AB;             -- Le noeud où est stocké l'id du père
        -- de l'individu id
        Mere         : T_AB;             -- Le noeud où est stocké l'id de la 
        -- mère de l'individu id
        Informations : T_Informations;   -- Les informations de l'individu id
        id           : Integer;          -- L'id stocké au noeud AB1
        Prenom       : Unbounded_String; -- Le prénom de l'individu id
        Nom          : Unbounded_String; -- Le nom de l'individu id
        Trouve       : Boolean := False; -- Est-ce qu'un individu a été trouvé
        
    begin
        
        if Taille(AB1) /= 0 then
            id := M_AB.Valeur (AB1);
            Informations := M_R.Element (Registre, id);
            Prenom := Informations.Prenom;
            Nom := Informations.Nom;
            
            -- Rechercher les ancêtres de l'individu 2 s'appelant Prenom Nom
            if Recherche_Homonymes (AB2, Prenom, Nom, Registre) then
                Afficher_Nom (Informations, id);
                Trouve := True;
            end if;
            
            Pere := M_AB.Feuille (AB1, DROITE);
            Mere := M_AB.Feuille (AB1, GAUCHE);
            Trouve := (Trouve or Recherche_Noms (Pere, AB2, Registre));
            Trouve := (Trouve or Recherche_Noms (Mere, AB2, Registre));
        else
            null;
        end if;
        return Trouve;
    end Recherche_Noms;
    
    
    procedure Afficher_Homonymes (Arbre : in T_AG; 
                                  id_n  : in Integer;
                                  id_m  : in Integer) is
        
        AB_n : T_AB; -- Le noeud où est stocké l'id_n
        AB_m : T_AB; -- Le noeud où est stocké l'id_m
        
    begin
        
        AB_n := M_TH.Element (Arbre.Noeuds, id_n);
        AB_m := M_TH.Element (Arbre.Noeuds, id_m);
        if not(Recherche_Noms (AB_n, AB_m, Arbre.Registre)) then
            Put_Line ("Aucun homonyme trouvé");
        else 
            null;
        end if;
        
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Afficher_Homonymes;
      
    
    function Taille (Arbre : in T_AG; 
                     id    : in Integer) 
                     return Integer is
    begin
        
        return M_AB.Taille (M_TH.Element (Arbre.Noeuds, id));
        
    exception
        when M_TH.Cle_Absente_Exception
            => raise ID_Absent_Exception;
    end Taille;

    
    -- Affichage d'une date au format JJ/MM/AAAA
    procedure Afficher_Date (Date : in T_Date) is
    begin 
        
        if Date.Jour < 10 then
            Put ("0");
        end if;
        Put (Date.Jour, 1); 
        Put ("/");
        if Date.Mois < 10 then
            Put ("0");
        end if;
        Put (Date.Mois, 1); 
        Put("/");
        Put (Date.Annee, 1); 
        New_Line;
        
    end Afficher_Date;

    
    procedure Afficher_Informations_Detaillee (Arbre : in T_AG; 
                                               id    : in Integer) is
        
        Informations : T_Informations; -- Informations de l'individu id
        
    begin
        
        Informations := M_R.Element (Arbre.Registre, id);
        Put ("Prenom : "); 
        Put (Informations.Prenom); 
        New_Line;
        Put ("Nom : "); 
        Put (Informations.Nom); 
        New_Line;
        Put ("Sexe : "); 
        Put (Informations.Sexe'Image); 
        New_Line;
        Put ("Date de naissance : ");
        Afficher_Date (Informations.Date_Naissance);
        Put ("Lieu de naissance : "); 
        Put (Informations.Lieu_Naissance);
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
            
    
    procedure Lier (Arbre      : in out T_AG; 
                    Id_Courant : in     Integer ; 
                    Id_A_Lier  : in     Integer) is 
        
        Noeud_Courant : T_AB; -- Le noeud où est stocké l'Id_Courant
        
    begin
        
        Noeud_Courant := Element(Arbre.Noeuds,Id_Courant);
        if Element(Arbre.Registre, Id_A_Lier).Sexe = HOMME then
            M_AB.Ajouter (Noeud_Courant, Element(Arbre.Noeuds,Id_A_Lier), DROITE);
        else
            M_AB.Ajouter (Noeud_Courant, Element(Arbre.Noeuds,Id_A_Lier), GAUCHE);
        end if;
        
    end Lier;
            
end Arbre_Genealogique;
