with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Integer_Text_IO;         use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;

package body Vecteurs_Creux is


	procedure Free is
		new Ada.Unchecked_Deallocation (T_Cellule, T_Vecteur_Creux);


	procedure Initialiser (V : in out T_Vecteur_Creux) is
	begin
		V := Null;
	end Initialiser;


    procedure Detruire (V: in out T_Vecteur_Creux) is
        Suivant : T_Vecteur_Creux;
	begin
            if V /= Null then
                Suivant := V.all.Suivant;
                Free(V);
                Detruire(Suivant);
            else
                Null;
            end if;


	end Detruire;


	function Est_Nul (V : in T_Vecteur_Creux) return Boolean is
	begin
		return V = Null;
	end Est_Nul;


	function Valeur (V : in T_Vecteur_Creux ; Indice : in Integer) return Float is
    begin
        if V = Null or else V.Indice > Indice then
            return 0.0;
        elsif V.Indice = Indice then
            return V.Valeur;
        else
            return Valeur(V.Suivant,Indice);
        end if;
	end Valeur;


	procedure Modifier (V : in out T_Vecteur_Creux ;
				       Indice : in Integer ;
                     Valeur : in Float ) is
        Nouveau : T_Vecteur_Creux;
    begin
        if Valeur = 0.0 then
            return;
        else
            Nouveau := new T_Cellule;
            Nouveau.all.Indice := Indice;
            Nouveau.all.Valeur := Valeur;
            if  V = Null then
                V := Nouveau;
            elsif Indice = V.all.Indice then
                V.all.Valeur := Valeur;
            elsif Indice < V.Indice then
                Nouveau.Suivant := V.Suivant;
                V.Suivant := Nouveau;
            else
                Modifier(V.Suivant,Indice,Valeur);
            end if;
        end if;
	end Modifier;


	function Sont_Egaux (V1, V2 : in T_Vecteur_Creux) return Boolean is
	begin
        if V1 /= Null and V2 /= Null then
            return (V1.all.Valeur = V2.all.Valeur and then V1.all.Indice = V2.all.Indice and then Sont_Egaux(V1.all.Suivant,V2.all.Suivant));
        elsif V1 = Null and V2 = Null then
            return True;
        else
            return False;
        end if;
	end Sont_Egaux;


	procedure Afficher (V : T_Vecteur_Creux) is
	begin
		if V = Null then
			Put ("--E");
		else
			-- Afficher la composante V.all
			Put ("-->[ ");
			Put (V.all.Indice, 0);
			Put (" | ");
			Put (V.all.Valeur, Fore => 0, Aft => 1, Exp => 0);
			Put (" ]");

			-- Afficher les autres composantes
			Afficher (V.all.Suivant);
		end if;
	end Afficher;


	function Taille_creuse (V: in T_Vecteur_Creux) return Integer is
	begin
		if V = Null then
			return 0;
		else
			return 1 + Taille_creuse (V.all.Suivant);
		end if;
	end Taille_creuse;


end Vecteurs_Creux;
