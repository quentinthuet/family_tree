------------------------------- TEST DE REGISTRE -------------------------------
-- On ne teste pas les fonctions qui sont identiques dans Registre et
-- Table_De_Hachage

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Registre;


procedure Test_Registre is

    -- INSTANTIATION DES MODULES EXTERIEURS --

    package M_R is new Registre (3);
    use M_R;

    -- PROCEDURES DE TESTS --

    M_Informations : constant array (1..8) of T_Informations
            := ((To_Unbounded_String("aaa"),To_Unbounded_String("zzz"),HOMME,
                (01,01,1907),To_Unbounded_String("AAA"),(01,01,1983)),
                (To_Unbounded_String("ddd"),To_Unbounded_String("www"),FEMME,
                 (01,01,1891),To_Unbounded_String("DDD"),(01,01,1983)),
                (To_Unbounded_String("fff"),To_Unbounded_String("uuu"),FEMME,
                 (01,01,1907),To_Unbounded_String("FFF"),(01,01,1983)),
                (To_Unbounded_String("ggg"),To_Unbounded_String("ttt"),HOMME,
                 (01,01,1907),To_Unbounded_String("GGG"),(01,01,1983)),
                (To_Unbounded_String("bbb"),To_Unbounded_String("yyy"),FEMME,
                 (01,01,1907),To_Unbounded_String("BBB"),(01,01,1983)),
                (To_Unbounded_String("ccc"),To_Unbounded_String("xxx"),HOMME,
                 (01,01,1984),To_Unbounded_String("CCC"),(01,01,1983)),
                (To_Unbounded_String("eee"),To_Unbounded_String("vvv"),HOMME,
                 (31,06,1950),To_Unbounded_String("EEE"),(01,01,1983)),
                (To_Unbounded_String("hhh"),To_Unbounded_String("sss"),FEMME,
                 (19,06,1908),To_Unbounded_String("HHH"),(29,02,1983)));

    M_Nouvelles_Informations : constant array (1..5) of T_Informations
            := ((To_Unbounded_String("iii"),To_Unbounded_String("rrr"),HOMME,
                (01,01,1907),To_Unbounded_String("III"),(01,01,1983)),
                (To_Unbounded_String("jjj"),To_Unbounded_String("qqq"),FEMME,
                 (01,01,1907),To_Unbounded_String("JJJ"),(01,01,1983)),
                (To_Unbounded_String("kkk"),To_Unbounded_String("ppp"),HOMME,
                 (01,01,1907),To_Unbounded_String("KKK"),(01,01,1983)),
                (To_Unbounded_String("lll"),To_Unbounded_String("ooo"),FEMME,
                 (01,01,1907),To_Unbounded_String("LLL"),(01,01,1983)),
                (To_Unbounded_String("mmm"),To_Unbounded_String("nnn"),FEMME,
                 (01,01,1907),To_Unbounded_String("MMM"),(01,01,1983)));

    M_Dates_Naissance_Fils : constant array (1..8) of T_Date
            := ((01,01,1970),(01,01,1970),(01,01,1970), (01,01,1970),
                (01,01,1997),(01,01,1970),(01,01,1970), (01,01,1970));

    procedure Tester_Enregistrer is

        R1 : T_R;

    begin

        Initialiser (R1);

        for i in 1..9 loop
            begin
                Enregistrer (R1,
                             ((i-1) mod 8) + 1,
                             M_Informations (((i-1) mod 8) + 1),
                             M_Dates_Naissance_Fils (((i-1) mod 8) + 1));
                pragma Assert (i < 5);
                -- Les informations 5, 6, 7 et 8 sont incohérentes et 9 est clé
                -- présente
                -- On vérifie que ce qui suit Enregistrer dans la boucle n'est pas
                -- exécuté pour i = 5..9 (et donc que les exceptions sont bien
                -- levées)
            exception
                when Date_Fils_Incoherente_Exception =>
                    pragma Assert (i = 5);
                when Ordre_Date_Incoherent_Exception =>
                    pragma Assert (i = 6);
                when Date_Naissance_Impossible_Exception =>
                    pragma Assert (i = 7);
                when Date_Deces_Impossible_Exception =>
                    pragma Assert (i = 8);
                when Cle_Presente_Exception =>
                    pragma Assert (i = 9);
            end;
        end loop;
        pragma Assert (Taille (R1) = 4);
        for i in 1..4 loop
            pragma Assert (Element (R1, i) = M_Informations (i));
        end loop;
        Vider (R1);

    end Tester_Enregistrer;

    procedure Tester_Modifier is

        R1 : T_R;

    begin

        Initialiser (R1);

        for i in 1..4 loop
            Enregistrer (R1, i, M_Informations (i), M_Dates_Naissance_Fils (i));
        end loop;
        for i in 1..5 loop
            begin
                Modifier (R1, i, M_Nouvelles_Informations (i),
                          M_Dates_Naissance_Fils (((i-1) mod 4) + 1));
                pragma Assert (i /= 5);
                -- On vérifie que ce qui suit Modifier n'est jamais exécuté
                -- (et donc que l'exception est bien levée)
            exception
                when Cle_Absente_Exception =>
                    pragma Assert (i = 5);
            end;
        end loop;
        pragma Assert (Taille (R1) = 4);
        for i in 1..4 loop
            pragma Assert (Element (R1, i) = M_Nouvelles_Informations (i));
        end loop;
        for i in 1..4 loop
            begin
                Modifier (R1, i, M_Informations (i + 4),
                          M_Dates_Naissance_Fils (i + 4));
                pragma Assert (False);
                -- On vérifie que ce qui suit Modifier n'est jamais exécuté
                -- (et donc que les exceptions sont bien levées)
            exception
                when Date_Fils_Incoherente_Exception
                    => pragma Assert (i = 1);
                when Ordre_Date_Incoherent_Exception
                    => pragma Assert (i = 2);
                when Date_Naissance_Impossible_Exception
                    => pragma Assert (i = 3);
                when Date_Deces_Impossible_Exception
                    => pragma Assert (i = 4);
            end;
        end loop;
        pragma Assert (Taille (R1) = 4);
        for i in 1..4 loop
            pragma Assert (Element (R1, i) = M_Nouvelles_Informations (i));
        end loop;
        Vider (R1);

    end Tester_Modifier;

    -- EXECUTION DES PROGRAMMES DE TEST --

begin
    Tester_Enregistrer;
    Tester_Modifier;
    Put_Line ("Tous les tests ont été passés avec succès !");
end Test_Registre;
