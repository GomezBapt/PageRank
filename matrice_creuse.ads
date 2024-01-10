with Vecteurs_Creux;

generic
    type T_Reel is digits <>;


package Matrice_Creuse is

    type T_Vecteur is array (Integer range <>) of T_Reel;

    type T_Matrice_creuse is array (Integer range <>) of Vecteurs_Creux.T_Vecteur_Creux;
    --type T_Vecteur is array (Integer range <>) of Float;
    
    procedure Lire_Sujet_creuse(sujet_net : in String; Matrice : out T_Matrice_creuse);

    procedure CalculerH_creuse(H : in out T_Matrice_creuse);
    
    function CalculerG_creuse(H : in T_Matrice_creuse; i : Integer; j : Integer; alpha : Float) return T_Reel;

    procedure Tri(pi : in out T_Vecteur; indices : out T_Vecteur);

    procedure CalculerPi_creuse(H : in T_Matrice_creuse; seuil : in Float; pi : out T_Vecteur; Taille : in Integer; k : in Integer; alpha : Float);

    procedure EcrireSortie(indices : in T_Vecteur; pi : in T_Vecteur; alpha : in Float; i_max : in Integer; Taille : in Integer; Nom : in String);

end Matrice_Creuse;
