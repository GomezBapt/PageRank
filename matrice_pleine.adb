with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;   use  Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

package body Matrice_Pleine is

    -- Lire sujet.net et construire la matrice d’adjacence associée
    procedure Lire_Sujet(sujet_net : in String; Matrice : out T_Matrice) is
        Taille : Integer;
        ligne : Unbounded_String;
        i : Integer;
        j : Integer;
        F : File_Type;
        indice : Integer;
    begin

        Open(F, In_File, sujet_net);
        Taille := Integer'Value(Get_Line(F)); 

        -- Initialiser la matrice
        for i in 1..Taille loop
            for j in 1..Taille loop
                Matrice(i,j) := 0.0;
            end loop;
        end loop;

        -- Lire le fichier sujet.net et faire la matrice associée
        while not End_Of_File(F) loop
            -- Récupérer la prochaine ligne
            ligne := To_Unbounded_String(Get_Line(F));
            indice := 1;
            while not (To_String(ligne)(indice..indice) = " ") loop
                indice := indice +1;
            end loop;
            i := Integer'Value(To_String(ligne)(1..(indice - 1)));
            j := Integer'Value(To_String(ligne)((indice+1)..(Length(ligne))));
            Matrice(i + 1,j + 1) := 1.0;

        end loop;
        Close(F);
    end Lire_Sujet;


    -- Créer H à partir de la matrice d’adjacence
    procedure CalculerH(H : in out T_Matrice) is
        somme : Integer;
    begin
        for i in H'Range loop

            -- Calculer le nombre d'occurrence sur une ligne
            somme := 0;
            for j in H'Range loop
                if H(i,j) = 1.0 then
                    somme := somme + 1;
                else
                    Null;
                end if;
            end loop;
            if somme = 0 then
                somme := 1;
            end if;

            -- Modifier la ligne i
            for j in H'Range loop
                H(i,j) := H(i,j)/T_Reel(somme);
            end loop;
        end loop;
    end CalculerH;


    -- Passer de H à S
    procedure CalculerS(S : in out T_Matrice) is
        vide : Boolean;
        Taille : Integer;
    begin
        Taille := S'Last;
        for i in S'Range loop

            -- Savoir si la ligne i a que des zéros
            vide := True;
            for j in S'Range loop
                if S(i,j) /= 0.0 then
                    Vide := False;
                else
                    Null;
                end if;
            end loop;

            -- Modifier la ligne i
            if Vide then
                for j in S'Range loop
                    S(i,j) := 1.0/T_Reel(Taille);
                end loop;
            else
                Null;
            end if;
        end loop;
    end CalculerS;


    -- Passer de S à G
    procedure CalculerG(G : in out T_Matrice; alpha : in Float) is
        Taille : Integer;
    begin
        Taille := G'Last;
        for i in G'Range loop
            for j in G'Range loop

                G(i,j) := T_Reel(alpha)*G(i,j) + (1.0 - T_Reel(alpha))/T_Reel(Taille);

            end loop;
        end loop;
    end CalculerG;


    -- Calculer pi à partir de G et d’un seuil
    procedure CalculerPi(G : in T_Matrice; seuil : in Float; pi : out T_Vecteur; Taille : in Integer; k : in Integer) is
        function norme(V1 : in T_Vecteur; V2 : in T_Vecteur) return T_Reel is
            norme1 : T_Reel;
            max : T_Reel;
        begin
            max := 0.0;
            norme1 := 0.0;
            for i in V1'Range loop
                norme1 := abs(V1(i)-V2(i));
                if norme1 > max then
                    max := norme1;
                end if;
            end loop;
            return max;
        end norme;
        pik : T_Vecteur (1..Taille);
        pik1 : T_Vecteur (1..Taille);
        compteur : Integer;

    begin
        compteur := 0;

        -- Initialiser pi 
        for i in 1..Taille loop
            pi(i) := 1.0/T_Reel(Taille);
        end loop;
        for i in 1..Taille loop
            pik(i) := pi(i);
            pik1(i) := pi(i);
        end loop;
        loop
            for i in 1..Taille loop
                pik(i) := pik1(i);
            end loop;

            -- Calculer pik1
            -- Mettre pik1 à 0
            for i in 1..Taille loop
                pik1(i) := 0.0;
            end loop;
            for i in 1..Taille loop
                for j in 1..Taille loop
                    pik1(i) := pik1(i) + pik(j)*G(j,i);
                end loop;
            end loop;
            compteur := compteur + 1;
            exit when (norme(pik1,pik) < T_Reel(seuil)) or compteur >= k;
        end loop;
        for i in 1..Taille loop
                pi(i) := pik1(i);
        end loop;
    end CalculerPi;


    -- Trier pi
    procedure Tri(pi : in out T_Vecteur; indices : out T_Vecteur) is
        Taille : Integer;
        max : T_Reel;
        indice_max : Integer;
        temp : T_Reel;
    begin
        Taille := pi'Last;
        for i in pi'Range loop
            indices(i) := T_Reel(i);
        end loop;
        for i in pi'Range loop
            max := 0.0;
            indice_max := 1;

            -- Obtenir l’indice du maximum entre les indices i et taille
            for j in i..Taille loop
                if pi(j) > max then
                    indice_max := j;
                    max := pi(j);
                else
                    Null;
                end if;
            end loop;

            -- Echanger les indices i et indice_max
            temp := pi(i);
            pi(i) := pi(indice_max);
            pi(indice_max) := temp;
            temp := indices(i);
            indices(i) := indices(indice_max);
            indices(indice_max) := temp;
        end loop;
    end Tri;


    -- Créer les fichiers sujet.pr et sujet.prw
    procedure EcrireSortie(indices : in T_Vecteur; pi : in T_Vecteur; alpha : in Float; i_max : in Integer; Taille : in Integer; Nom : in String) is
        ind : Integer;
        F_prw : File_Type;
        F_pr : File_Type;
    begin

        -- Créer le fichier sujet.prw
        Create(F_prw, Out_File, Name => Nom & ".prw");
        Put(F_prw, Integer'Image(Taille));
        Put(F_prw, " ");
        Put(F_prw, Float'Image(alpha));
        Put(F_prw, " ");
        Put_Line(F_prw, Integer'Image(i_max));
        for i in pi'Range loop
            Put_Line(F_prw, T_Reel'Image(pi(i)));
        end loop;
        Close(F_prw);

        -- Créer le fichier sujet.pr
        Create(F_pr, Out_File, Name  => Nom & ".pr");
        for i in pi'Range loop
            Put_Line(F_pr, Integer'Image(Integer(indices(i))-1));
        end loop;
        Close(F_pr);
    end EcrireSortie;

end Matrice_Pleine;
