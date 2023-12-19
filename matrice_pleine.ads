package Matrice_Pleine is

    type T_Matrice is array (Integer range <>, Integer range <>) of Float;
    type T_Vecteur is array (Integer range <>) of Float;

    procedure Lire_Sujet(sujet_net : in String; Matrice : out T_Matrice);

    procedure CalculerH(H : in out T_Matrice);

    procedure CalculerS(S : in out T_Matrice);

    procedure CalculerG(G : in out T_Matrice; alpha : in Float);

    procedure CalculerPi(G : in T_Matrice; seuil : in Float; pi : out T_Vecteur; Taille : in Integer; k : in Integer);

    procedure Tri(pi : in out T_Vecteur; indices : out T_Vecteur);

    procedure EcrireSortie(indices : in T_Vecteur; pi : in T_Vecteur; alpha : in Float; i_max : in Integer; Taille : in Integer; Nom : in String);

end Matrice_Pleine;