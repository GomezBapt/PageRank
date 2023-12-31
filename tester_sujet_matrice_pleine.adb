with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Matrice_Pleine;        use Matrice_Pleine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Page_Rank_Matrice_Pleine is
    sujet_net : constant String := "sujet.net";
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
    Adjacence : T_Matrice (1..Taille,1..Taille);
    alpha : Float;
    seuil : Float;
    pi : T_Vecteur (1..Lire_Taille(sujet_net));
    indices : T_Vecteur (1..Lire_Taille(sujet_net));

begin
    Lire_Sujet(sujet_net,Adjacence);
    CalculerH(Adjacence);
    CalculerS(Adjacence);
    alpha := 0.85;
    CalculerG(Adjacence, alpha);
    CalculerPi(Adjacence,seuil,pi,Taille,150);
    Tri(pi, indices);
    EcrireSortie(indices, pi, alpha, 150, Taille, "sujet");
end Page_Rank_Matrice_Pleine;
