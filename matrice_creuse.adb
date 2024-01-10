with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;   use  Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Vecteurs_Creux; use Vecteurs_Creux;

package body Matrice_Creuse is

    -- Lire sujet.net et construire la matrice d’adjacence associée
    procedure Lire_Sujet_creuse(sujet_net : in String; Matrice : out T_Matrice_creuse) is
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
                Initialiser(Matrice(i));
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
            Modifier(Matrice(i + 1),j + 1, 1.0);

        end loop;
        Close(F);
    end Lire_Sujet_creuse;


    -- Créer H à partir de la matrice d’adjacence
    procedure CalculerH_creuse(H : in out T_Matrice_creuse) is
        somme : Float;
        nv_valeur : Float;
    begin
        Put_Line("Starting H");
        New_Line;
        for i in H'Range loop

            -- Calculer le nombre d'occurrence sur une ligne
            if not (Est_Nul(H(i))) then
                somme := Float(Taille_creuse(H(i)));
                for j in H'Range loop
                    nv_valeur := Valeur(H(i),j)/somme;
                    Modifier(H(i),j,nv_valeur);
                end loop;
            else
                Null;
            end if;
            if i mod 10000 = 0 then
                Put_Line("Iteration :" & Integer'Image(i) & " sur" & Integer'Image(H'Last));
            end if;
        end loop;
    end CalculerH_creuse;

    -- Renvoi la valeur de G(i,j)
    function CalculerG_creuse(H : in T_Matrice_creuse; i : Integer; j : Integer; alpha : Float) return T_Reel is
        s_ij : T_Reel;


    begin
        if Est_Nul(H(i)) then
            -- La ligne est vide donc s_ij vaut 1/Taille (Correspond au calcul de S)
            s_ij := 1.0/T_Reel(H'Last);
        else
            -- Calcul de S si la ligne n'est pas vide
            s_ij := T_Reel(Valeur(H(i),j));
        end if;
        -- On calcule la valeur de G(i,j)
        return (T_Reel(alpha)*s_ij + (1.0-T_Reel(alpha))/T_Reel(H'Last));

    end CalculerG_creuse;

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

      -- Calculer pi à partir de G et d’un seuil
    procedure CalculerPi_creuse(H : in T_Matrice_creuse; seuil : in Float; pi : out T_Vecteur; Taille : in Integer; k : in Integer; alpha : Float) is
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
        New_Line; New_Line;
        Put_Line("Starting Pi");
        New_Line; New_Line;
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
                    pik1(i) := pik1(i) + pik(j)*CalculerG_creuse(H,j,i,alpha);
                end loop;
                if (i+1) mod 1000 = 0 then
                    Put_Line("Pi :" & Integer'Image(i+1) & " sur" & Integer'Image(Taille));
                end if;
            end loop;
            compteur := compteur + 1;
            New_Line;
            Put_Line("Iteration :" & Integer'Image(compteur) & " sur" & Integer'Image(k));
            Put_Line("Precision :" & T_Reel'Image(norme(pik1,pik)));
            New_Line;
            exit when (norme(pik1,pik) < T_Reel(seuil)) or compteur >= k;
        end loop;
        for i in 1..Taille loop
            pi(i) := pik1(i);
        end loop;
    end CalculerPi_creuse;

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
end Matrice_Creuse;
