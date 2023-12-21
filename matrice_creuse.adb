with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;   use  Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Vecteurs_Creux; use Vecteurs_Creux;
with Matrice_Pleine; use Matrice_Pleine;

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
        end loop;
    end CalculerH_creuse;

    function CalculerG_creuse(H : in T_Matrice_creuse; i : Integer; j : Integer; alpha : Float) return Float is
        s_ij : Float;


    begin
        if Est_Nul(H(i)) then
            s_ij := 1.0/Float(H'Last);
        else
            s_ij := Valeur(H(i),j);
        end if;
        return (alpha*s_ij + (1.0-alpha)/Float(H'Last));

    end CalculerG_creuse;

      -- Calculer pi à partir de G et d’un seuil
    procedure CalculerPi_creuse(H : in T_Matrice_creuse; seuil : in Float; pi : out T_Vecteur; Taille : in Integer; k : in Integer; alpha : Float) is
        function norme(V1 : in T_Vecteur; V2 : in T_Vecteur) return Float is
            norme1 : Float;
            max : Float;
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
            pi(i) := 1.0/Float(Taille);
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
                    pik1(i) := pik1(i) + pik(j)*CalculerG_creuse(H,j,i,alpha);
                end loop;
            end loop;
            compteur := compteur + 1;
            exit when (norme(pik1,pik) < seuil) or compteur >= k;
        end loop;
        for i in 1..Taille loop
                pi(i) := pik1(i);
        end loop;
    end CalculerPi_creuse;
end Matrice_Creuse;
