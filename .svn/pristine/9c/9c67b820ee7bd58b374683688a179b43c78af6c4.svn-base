--------------------------- IMPLEMENTATION DE REGISTRE -------------------------

package body Registre is

    -- FONCTIONS ET PROCEDURES --

    -- Est-ce que la Date1 est plus grande que la Date2 ?
    function Est_Plus_Grande (Date1 : in T_Date;
                              Date2 : in T_Date)
                              return Boolean is
    begin

        return Date1.Annee > Date2.Annee
                or (Date1.Annee = Date2.Annee and then Date1.Mois > Date2.Mois)
                or (Date1.Annee = Date2.Annee and then Date1.Mois = Date2.Mois
                    and then Date1.Jour > Date2.Jour);

    end Est_Plus_Grande;


    -- Est-ce que la Date est valide ?
    function Est_Valide (Date : in T_Date)
                         return Boolean is

        Jour_Valide : Boolean; -- Est-ce que le jour est valide ?

    begin

        Jour_Valide := (Date.Jour >= 1);
        case Date.Mois is
            when 1|3|5|7|8|10|12 =>
                if (Date.Jour > 31) then
                    Jour_Valide := False;
                else
                    null;
                end if;
            when 4|6|9|11 =>
                if (Date.Jour > 30) then
                    Jour_Valide := False;
                else
                    null;
                end if;
            when 2 =>
                if (Date.Annee mod 4 = 0 and Date.Annee mod 100 = 0)
                        or Date.Annee mod 400 = 0 then
                    if (Date.Jour > 29) then
                        Jour_Valide := False;
                    else
                        null;
                    end if;
                else
                    if (Date.Jour > 28) then
                        Jour_Valide := False;
                    else
                        null;
                    end if;
                end if;
            when others => null;
        end case;
        return Date.Mois > 0 and Date.Mois <= 12 and Jour_Valide;

    end;


    -- Fonction de hachage du régistre
    function F_Hachage (Cle : in Integer) return Integer is
    begin

        return (1 + Cle mod Capacite);

    end;


    procedure Enregistrer (Registre            : in out T_R;
                           Id                  : in     Integer;
                           Informations        : in     T_Informations;
                           Date_Naissance_Fils : in     T_Date) is
    begin

        if not (Est_Valide (Informations.Date_Naissance)) then
            raise Date_Naissance_Impossible_Exception;
        elsif not (Est_Valide (Informations.Date_Deces)) then
            raise Date_Deces_Impossible_Exception;
        elsif Est_Plus_Grande (Informations.Date_Naissance,
                               Informations.Date_Deces) then
            raise Ordre_Date_Incoherent_Exception;
        elsif Est_Plus_Grande (Date_Naissance_Fils, Informations.Date_Deces)
                or Est_Plus_Grande (Informations.Date_Naissance,
                                    Date_Naissance_Fils) then
            raise Date_Fils_Incoherente_Exception;
        else
            Enregistrer (Registre, Id, Informations);
        end if;

    exception
        when M_TH.Cle_Presente_Exception => raise Cle_Presente_Exception;
    end Enregistrer;


    procedure Modifier (Registre            : in out T_R;
                        Id                  : in     Integer;
                        Informations        : in     T_Informations;
                        Date_Naissance_Fils : in     T_Date) is
    begin

        if not (Est_Valide (Informations.Date_Naissance)) then
            raise Date_Naissance_Impossible_Exception;
        elsif not (Est_Valide (Informations.Date_Deces)) then
            raise Date_Deces_Impossible_Exception;
        elsif Est_Plus_Grande (Informations.Date_Naissance,
                               Informations.Date_Deces) then
            raise Ordre_Date_Incoherent_Exception;
        elsif Est_Plus_Grande (Date_Naissance_Fils, Informations.Date_Deces)
                or Est_Plus_Grande (Informations.Date_Naissance,
                                    Date_Naissance_Fils) then
            raise Date_Fils_Incoherente_Exception;
        else
            Modifier (Registre, Id, Informations);
        end if;

    exception
        when M_TH.Cle_Absente_Exception => raise Cle_Absente_Exception;
    end Modifier;

end Registre;
