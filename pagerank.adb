with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Matrice_Pleine;        use Matrice_Pleine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;      use Ada.Command_Line;
with Vecteurs_Creux;        use Vecteurs_Creux;
with Matrice_Creuse;        use Matrice_Creuse;

procedure PageRank is
    sujet_net : String := Argument(Argument_Count);

    -- Affiche comment utiliser les options
    procedure Afficher_Usage is
    begin
        New_Line;
        Put_Line ("Guide d'usage");
        New_Line;
        Put_Line ("   Alpha  : appartient a l'intervalle [0,1]");
        Put_Line ("   k : un nombre entier positif");
        Put_Line ("   epsilon  : permet de definir la precision");
        Put_Line ("   P : utilise le programme matrice pleine");
        Put_Line ("   C  :utilise le programme matrice creuse");
        Put_Line ("   R : donne le nom des fichiers produits");
        New_Line;
    end Afficher_Usage;

    -- Lit et retourne la taille du graphe
    function Lire_Taille(sujet_net : in String) return Integer is
        Taille : Integer;
        F : File_Type;
    begin
        Open(F, In_File, sujet_net);
        Taille := Integer'Value(Get_Line(F));
        Close(F);
        return Taille;
    end Lire_Taille;


    Taille : constant Integer := Lire_Taille(sujet_net);
    --Adjacence : T_Matrice (1..Taille,1..Taille);
    pi : T_Vecteur (1..Taille);
    alpha : Float := 0.85;
    k : Integer := 150;
    seuil : Float := 0.0;
    P : Boolean := True;
    indice : Integer := 1;
    nb_argument : Integer;
    indices : T_Vecteur(1..Taille);
    prefixe : Unbounded_String := To_Unbounded_String("output");
    correct : Boolean := True;
    OutOfBounds_A : Exception;
    OutOfBounds_K : Exception;
    Adjacence_creuse : T_Matrice_creuse(1..Taille);

begin
    nb_argument := Argument_Count;
    begin

    -- Parcourt les arguments de la ligne de commande et met à jour les constantes en fonction
    while indice < nb_argument and correct loop
        if Argument(indice) = "-A" or Argument(indice) = "-a" then
            alpha := Float'Value(Argument(indice + 1));
            if alpha > 1.0 or alpha < 0.0 then
                raise OutOfBounds_A;
            end if;
            indice := indice + 2;
        elsif Argument(indice) = "-K" or Argument(indice) = "-k" then
            k := Integer'Value(Argument(indice + 1));
            if k < 0 then
                raise OutOfBounds_K;
            end if;
            indice := indice + 2;
        elsif Argument(indice) =  "-E" or Argument(indice) = "-e" then
            seuil := Float'Value(Argument(indice + 1));
            indice := indice + 2;
        elsif Argument(indice) = "-P" or Argument(indice) = "-p" then
            indice := indice + 1;
        elsif Argument(indice) = "-C" or Argument(indice) = "-c" then
            P := False;
            indice := indice + 1;
        elsif Argument(indice) = "-R" or Argument(indice) = "-r" then
            prefixe := To_Unbounded_String(Argument(indice + 1));
            indice := indice + 2;
        else
            Afficher_Usage;
            correct := False;
        end if;
    end loop;
    exception
        when others => Afficher_Usage;
    end;
    if correct then
        if P then
        -- Lance pagerank avec matrice pleine
            --begin
               -- Lire_Sujet(sujet_net,Adjacence);
                --CalculerH(Adjacence);
                --CalculerS(Adjacence);
                --CalculerG(Adjacence, alpha);
                --CalculerPi(Adjacence,seuil,pi,Taille,k);
                --Tri(pi,indices);
                --EcrireSortie(indices,pi,alpha,k,Taille,To_String(prefixe));
            --exception
              --  when others => Afficher_Usage;
            --end;
            Null;
        else
        -- Lance pagerank avec matrice creuse
            begin
                Lire_Sujet_creuse(sujet_net,Adjacence_creuse);
                New_Line;
                New_Line;
                Put_Line("Reading over");
                New_Line;
                New_Line;
                CalculerH_creuse(Adjacence_creuse);
                New_Line; New_Line;
                Put_Line("H over");
                New_Line; New_Line;
                CalculerPi_creuse(Adjacence_creuse,seuil,pi,Taille,k,alpha);
                New_Line; New_Line;
                Put_Line("Pi over");
                New_Line; New_Line;
                Tri(pi,indices);
                New_Line; New_Line;
                Put_Line("Tri over");
                New_Line; New_Line;
                EcrireSortie(indices,pi,alpha,k,Taille,To_String(prefixe));
            exception
                when others => Afficher_Usage;
            end;
        end if;
    else
        Null;
    end if;
end PageRank;
