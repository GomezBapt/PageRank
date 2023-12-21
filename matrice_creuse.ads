with Vecteurs_Creux;
with Matrice_Pleine;

package Matrice_Creuse is

    type T_Matrice_creuse is array (Integer range <>) of Vecteurs_Creux.T_Vecteur_Creux;
    --type T_Vecteur is array (Integer range <>) of Float;
    
    procedure Lire_Sujet_creuse(sujet_net : in String; Matrice : out T_Matrice_creuse);

    procedure CalculerH_creuse(H : in out T_Matrice_creuse);

    procedure CalculerPi_creuse(H : in T_Matrice_creuse; seuil : in Float; pi : out Matrice_Pleine.T_Vecteur; Taille : in Integer; k : in Integer);

end Matrice_Creuse;
