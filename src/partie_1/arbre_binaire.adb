------------------------ IMPLEMENTATION DE ARBRE_BINAIRE -----------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Arbre_Binaire is
    
    -- INSTANTIATION DES MODULES EXTERIEURS --

    procedure Free is
            new Ada.Unchecked_Deallocation (Object => T_Noeud, Name => T_AB);

    -- FONCTIONS ET PROCEDURES --
    
    procedure Initialiser (Arbre : out T_AB) is
    begin
        Arbre := null;
    end Initialiser;

    
    procedure Detruire (Arbre         : in out T_AB; 
                        Premier_Appel : in     Boolean := True) is
        Temp : T_AB;
    begin
        if Arbre /= null then
            if Premier_Appel and then Arbre.all.Parent /= null then
                raise Non_Racine_Exception;
            else
                Detruire (Arbre.all.Sous_Arbre_Gauche, False);
                Detruire (Arbre.all.Sous_Arbre_Droit, False);
                Temp := Arbre;
                Arbre := null;
                Free (Temp);
            end if;
        else
            null;
        end if;
    end Detruire;


    procedure Ajouter (Noeud           : in out T_AB ;
                       Nouvelle_Valeur : in     T_Valeur ;
                       Direction       : in     T_Direction) is
        Nouveau_Noeud : T_AB;    
    begin
        -- On créer le nouveau noeud à ajouter
        Nouveau_Noeud := new T_Noeud;
        Nouveau_Noeud.all.Valeur := Nouvelle_Valeur;
        Nouveau_Noeud.all.Sous_Arbre_Gauche := null;
        Nouveau_Noeud.all.Sous_Arbre_Droit := null;
        case Direction is
            when PREMIER =>
                if Noeud /= null then
                    -- Si on veut ajouter un premier noeud (noeud racine) à un 
                    -- arbre non vide, on soulève une exception et on libère la 
                    -- mémoire allouée pour le nouveau noeud
                    Free (Nouveau_Noeud);
                    raise Noeud_Present_Exception;
                else
                    -- Sinon, on ajoute ce premier noeud à l'arbre
                    Nouveau_Noeud.all.Parent := null;
                    Noeud := Nouveau_Noeud;
                end if;
                -- Dans les cas où la procédure est appelée avec GAUCHE ou
                -- DROITE, on soulève une exception si le noeud auquel on veut
                -- ajouter est null où si la branche à laquelle on veut ajouter
                -- est déjà occupée
            when GAUCHE =>
                if Noeud = null then
                    Free (Nouveau_Noeud);
                    raise Noeud_Absent_Exception;
                elsif Noeud.Sous_Arbre_Gauche /= null then
                    Free (Nouveau_Noeud);
                    raise Noeud_Present_Exception;
                else
                    Nouveau_Noeud.all.Parent := Noeud;
                    Noeud.Sous_Arbre_Gauche := Nouveau_Noeud;
                end if;
            when DROITE =>
                if Noeud = null then
                    Free (Nouveau_Noeud);
                    raise Noeud_Absent_Exception;
                elsif Noeud.Sous_Arbre_Droit /= null then
                    Free (Nouveau_Noeud);
                    raise Noeud_Present_Exception;
                else
                    Nouveau_Noeud.all.Parent := Noeud;
                    Noeud.Sous_Arbre_Droit := Nouveau_Noeud;
                end if;
        end case;
    end Ajouter;

    
    procedure Supprimer (Noeud : in out T_AB) is
    begin
        if Noeud /= null then 
            if Noeud.Parent /= null then
                -- Si le noeud que l'on veut supprimer n'est pas le noeud
                -- racine, on fait pointer son parent sur null
                if Noeud = Noeud.Parent.Sous_Arbre_Gauche then
                    Noeud.Parent.Sous_Arbre_Gauche := null;
                elsif Noeud = Noeud.Parent.Sous_Arbre_Droit then
                    Noeud.Parent.Sous_Arbre_Droit := null;
                else
                    null;
                end if;
                Noeud.Parent := null;
            else
                null;
            end if;
            -- On détruit récursivement le noeud et ses noeuds fils
            Detruire (Noeud);
        else 
            -- Si le noeud que l'on veut supprimer n'existe pas, on soulève une
            -- exception
            raise Noeud_Absent_Exception;
        end if;
    end Supprimer;
    

    procedure Afficher (Noeud          : in T_AB; 
                        Profondeur_Max : in Integer := -1) is
        
        procedure Indenter(Decalage : in Integer) is
        begin
            for I in 1..Decalage loop
                Put (' ');
            end loop;
        end Indenter;

        -- Afficher un arbre à la profondeur Profondeur et qui à du côté
        -- indiqué (Mère pour Gauche et Père pour droit, - pour la racine).
        procedure Afficher_Profondeur (Abr        : in T_AB; 
                                       Profondeur : in Integer;
                                       Cote       : in String) is
        begin
            if Profondeur_Max = -1 or Profondeur <= Profondeur_Max then
                if Abr = Null then
                    Null;
                else
                    Indenter (Profondeur * 4);
                    Put (Cote & ' ');
                    Afficher_Noeud (Abr.all.Valeur);
                    New_Line;

                    Afficher_Profondeur (Abr.all.Sous_Arbre_Gauche, 
                                         Profondeur + 1, "Mère :");
                    Afficher_Profondeur (Abr.all.Sous_Arbre_Droit, 
                                         Profondeur + 1, "Père :");
                end if;
            else
                null;
            end if;
        end Afficher_Profondeur;
        
    begin
        New_Line;
        Afficher_Profondeur (Noeud, 0, "-");
        New_Line;
    end Afficher;



    function Taille (Noeud : in T_AB) 
                     return Integer is
    begin
        if Noeud /= null then
            -- On somme récursivement le nombre de noeuds fils jusqu'aux feuilles
            return Taille (Noeud.all.Sous_Arbre_Gauche) + 
                    Taille (Noeud.all.Sous_Arbre_Droit) + 1;
        else
            return 0;
        end if;
    end;

    function Valeur (Noeud : in T_AB) 
                     return T_Valeur is
    begin
        -- On lève une exception si le noeud dont on veut connaitre la valeur
        -- est null
        if Noeud = null then
            raise Noeud_Absent_Exception;
        else
            return Noeud.all.Valeur;
        end if;
    end;

    function Taille_Niveau (Noeud  : in T_AB; 
                            Niveau : in Integer)
                            return Integer is
    begin
        if Niveau = 0 then
            -- Si on est au niveau voulu, on retourne 1 si le noeud n'est pas 
            -- null
            if Noeud = null then
                return 0;
            else 
                return 1;
            end if;
        else
            -- Si on n'est pas au niveau voulu et qu'il reste des noeuds fils,
            -- on somme la Taille_Niveau des deux sous arbres en décrémentant le
            -- niveau
            if Noeud = null then
                return 0;
            else
                return Taille_Niveau (Noeud.all.Sous_Arbre_Gauche, Niveau - 1)
                        + Taille_Niveau (Noeud.all.Sous_Arbre_Droit, Niveau - 1);
            end if;
        end if;
    end Taille_Niveau;
    
    
    function Feuille (Noeud     : in T_AB; 
                      Direction : in T_Direction)
                      return T_AB is
        Resultat : T_AB;
    begin
        if Noeud /= null then 
            if Direction = DROITE then
                Resultat := Noeud.Sous_Arbre_Droit;
            else
                Resultat := Noeud.Sous_Arbre_Gauche;
            end if;
        else
            raise Noeud_Absent_Exception;
        end if;
        return Resultat;
    end Feuille;
    
    
    function Racine (Noeud : in T_AB) 
                     return T_AB is
        Resultat : T_AB;
    begin
        if Noeud /= null and then Noeud.Parent /= null then
            Resultat := Noeud.Parent;
        else
            raise Noeud_Absent_Exception;
        end if;
        return Resultat;
    end Racine;
        
    
    procedure Test (Arbre     : in out T_AB; 
                    Val_Test1 : in     T_Valeur;
                    Val_Test2 : in     T_Valeur;
                    Val_Test3 : in     T_Valeur;
                    Val_Test4 : in     T_Valeur;
                    Val_Test5 : in     T_Valeur) is
       
        
        
        procedure Tester_Initialiser_Detruire (Arbre : in out T_AB) is
        begin
            Initialiser (Arbre);
            pragma Assert (Taille (Arbre) = 0);
            Detruire (Arbre);
        end Tester_Initialiser_Detruire;

        procedure Tester_Ajouter_Supprimer (Arbre     : in out T_AB;
                                            Val_Test1 : in     T_Valeur;
                                            Val_Test2 : in     T_Valeur;
                                            Val_Test3 : in     T_Valeur;
                                            Val_Test4 : in     T_Valeur;
                                            Val_Test5 : in     T_Valeur) is
            Liste_Noeuds : array(1..5) of T_AB;
            Val_Exception : T_Valeur;
            pragma Unreferenced (Val_Exception); -- Enlève l'exception unreferenced
        begin
            Initialiser (Arbre);

            pragma Assert (Taille (Arbre) = 0);
            pragma Assert (Taille_Niveau (Arbre, 0) = 0);
            pragma Assert (Taille_Niveau (Arbre, 1) = 0);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            
            Put_Line ("Initialisation");
            Afficher_Test (Arbre);
            
            Ajouter (Arbre, Val_Test1, PREMIER);
            Liste_Noeuds(1) := Arbre;

            pragma Assert (Taille (Arbre) = 1);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 0);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            
            Put_Line ("Ajout de la racine 1");
            Afficher_Test (Arbre);

            Ajouter (Arbre, Val_Test2, GAUCHE);
            Liste_Noeuds(2) := Arbre.Sous_Arbre_Gauche;

            pragma Assert (Taille (Arbre) = 2);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 1);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            pragma Assert (Valeur (Liste_Noeuds(2)) = Val_Test2);
            
            Put_Line ("Ajout de 2, noeud fils gauche de 1");
            Afficher_Test (Arbre);

            Ajouter (Arbre, Val_Test3, DROITE);
            Liste_Noeuds(3) := Arbre.Sous_Arbre_Droit;

            pragma Assert (Taille (Arbre) = 3);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 2);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            pragma Assert (Valeur (Liste_Noeuds(2)) = Val_Test2);
            pragma Assert (Valeur (Liste_Noeuds(3)) = Val_Test3);
            
            Put_Line ("Ajout de 3, noeud fils droit de 1");
            Afficher_Test (Arbre);

            Ajouter (Liste_Noeuds (2), Val_Test4, GAUCHE);
            Liste_Noeuds(4) := Liste_Noeuds(2).Sous_Arbre_Gauche;

            pragma Assert (Taille (Arbre) = 4);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 2);
            pragma Assert (Taille_Niveau (Arbre, 2) = 1);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            pragma Assert (Valeur (Liste_Noeuds(2)) = Val_Test2);
            pragma Assert (Valeur (Liste_Noeuds(3)) = Val_Test3);
            pragma Assert (Valeur (Liste_Noeuds(4)) = Val_Test4);
            
            Put_Line ("Ajout de 4, noeud fils gauche de 2");
            Afficher_Test (Arbre);

            Ajouter (Liste_Noeuds (2), Val_Test5, DROITE);
            Liste_Noeuds(5) := Liste_Noeuds(2).Sous_Arbre_Droit;

            pragma Assert (Taille (Arbre) = 5);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 2);
            pragma Assert (Taille_Niveau (Arbre, 2) = 2);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            pragma Assert (Valeur (Liste_Noeuds(2)) = Val_Test2);
            pragma Assert (Valeur (Liste_Noeuds(3)) = Val_Test3);
            pragma Assert (Valeur (Liste_Noeuds(4)) = Val_Test4);
            pragma Assert (Valeur (Liste_Noeuds(5)) = Val_Test5);
            
            Put_Line ("Ajout de 5, noeud fils droit de 2");
            Afficher_Test (Arbre);

            begin
                Ajouter (Liste_Noeuds (2), Val_Test5, DROITE);
                -- On vérifie que l'instruction suivante n'est pas exécutée
                -- (et donc que l'exception est bien levée)
                -- Ce processus est répété plusieurs fois dans le code qui suit
                pragma Assert (False);
            exception
                when Noeud_Present_Exception => null;
            end;
         
            Supprimer (Liste_Noeuds (5));
            Liste_Noeuds (5) := null;

            pragma Assert (Taille (Arbre) = 4);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 2);
            pragma Assert (Taille_Niveau (Arbre, 2) = 1);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            pragma Assert (Valeur (Liste_Noeuds(2)) = Val_Test2);
            pragma Assert (Valeur (Liste_Noeuds(3)) = Val_Test3);
            pragma Assert (Valeur (Liste_Noeuds(4)) = Val_Test4);
            begin
                Val_Exception := Valeur (Liste_Noeuds (2).Sous_Arbre_Droit);
                pragma Assert (False);
            exception
                when Noeud_Absent_Exception => null;
            end;   
            
            Put_Line ("Suppression de 5");
            Afficher_Test (Arbre);
         
            Supprimer (Liste_Noeuds (4));
            Liste_Noeuds (4) := null;

            pragma Assert (Taille (Arbre) = 3);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 2);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            pragma Assert (Valeur (Liste_Noeuds(2)) = Val_Test2);
            pragma Assert (Valeur (Liste_Noeuds(3)) = Val_Test3);
            for i in 4..5 loop
                begin
                    Val_Exception := Valeur (Liste_Noeuds (2).Sous_Arbre_Gauche);
                    pragma Assert (False);
                exception
                    when Noeud_Absent_Exception => null;
                end;
            end loop;
            
            Put_Line ("Suppression de 4");
            Afficher_Test (Arbre);
         
            Supprimer (Liste_Noeuds (3));
            Liste_Noeuds (3) := null;

            pragma Assert (Taille (Arbre) = 2);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 1);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            pragma Assert (Valeur (Liste_Noeuds(2)) = Val_Test2);
            for i in 3..5 loop
                begin
                    Val_Exception := Valeur (Arbre.Sous_Arbre_Droit);
                    pragma Assert (False);
                exception
                    when Noeud_Absent_Exception => null;
                end;
            end loop;
            
            Put_Line ("Suppression de 3");
            Afficher_Test (Arbre);

            Supprimer (Liste_Noeuds (2));
            Liste_Noeuds (2) := null;

            pragma Assert (Taille (Arbre) = 1);
            pragma Assert (Taille_Niveau (Arbre, 0) = 1);
            pragma Assert (Taille_Niveau (Arbre, 1) = 0);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            pragma Assert (Valeur (Liste_Noeuds(1)) = Val_Test1);
            for i in 2..5 loop
                begin
                    Val_Exception := Valeur (Arbre.Sous_Arbre_Gauche);
                    pragma Assert (False);
                exception
                    when Noeud_Absent_Exception => null;
                end;
            end loop;
            
            Put_Line ("Suppression de 2");
            Afficher_Test (Arbre);
         
            Supprimer (Arbre);
            Liste_Noeuds (1) := null;
            pragma Assert (Taille (Arbre) = 0);
            pragma Assert (Taille_Niveau (Arbre, 0) = 0);
            pragma Assert (Taille_Niveau (Arbre, 1) = 0);
            pragma Assert (Taille_Niveau (Arbre, 2) = 0);
            for i in 1..5 loop
                begin
                    Val_Exception := Valeur (Arbre);
                    pragma Assert (False);
                exception
                    when Noeud_Absent_Exception => null;
                end;
            end loop;
            
            Put_Line ("Suppression de 1");
            Afficher_Test (Arbre);

        end Tester_Ajouter_Supprimer;
      
    begin
        Tester_Initialiser_Detruire (Arbre);
        Tester_Ajouter_Supprimer (Arbre, 
                                  Val_Test1, 
                                  Val_Test2, 
                                  Val_Test3,
                                  Val_Test4, 
                                  Val_Test5);
    end Test;

end Arbre_Binaire;
