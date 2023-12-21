-- Ce module définit un type Vecteur_Creux et les opérations associés. Un
-- vecteur creux est un vecteur qui contient essentiellement des 0. Aussi pour
-- économiser l'espace de stockage et les temps de calculs, on ne conserve que
-- les valeurs non nulles.

package Vecteurs_Creux is

	type T_Cellule;

	type T_Vecteur_Creux is access T_Cellule;

	type T_Cellule is
		record
			Indice : Integer;
			Valeur : Float;
			Suivant : T_Vecteur_Creux;
			-- Invariant :
			--   Indice >= 1;
			--   Suivant = Null or else Suivant.all.Indice > Indice;
			--   	-- cellules sont stockées dans l'ordre croissant des indices
		end record;


	-- Initialiser un vecteur creux.  Il est nul.
	procedure Initialiser (V : in out T_Vecteur_Creux) with
		Post => Est_Nul (V);


	-- Détruire le vecteur V.
	procedure Detruire (V: in out T_Vecteur_Creux);


	-- Est-ce que le vecteur V est nul ?
	function Est_Nul (V : in T_Vecteur_Creux) return Boolean;


	-- Récupérer la composante (valeur) du vecteur V à l'indice Indice.
	function Valeur (V : in T_Vecteur_Creux ; Indice : in Integer) return Float
		with Pre => Indice >= 1;


	-- Modifier une composante (Indice, Valeur) d'un vecteur creux.
	procedure Modifier (V : in out T_Vecteur_Creux ;
				       Indice : in Integer ;
					   Valeur : in Float ) with
		pre => Indice >= 1;


	-- Est-ce que deux vecteurs creux sont égaux ?
	function Sont_Egaux (V1, V2 : in T_Vecteur_Creux) return Boolean;


	-- Afficher le vecteur creux à des fins de mise au point.
	procedure Afficher (V : T_Vecteur_Creux);


	-- Nombre de composantes non nulles du vecteur V.
	function Taille_creuse (V: in T_Vecteur_Creux) return Integer with
		Post => Taille_creuse'Result >= 0;


end Vecteurs_Creux;
