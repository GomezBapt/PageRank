with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Matrice_Pleine;        use Matrice_Pleine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Matrice_Creuse; use Matrice_Creuse;
with Vecteurs_Creux; use Vecteurs_Creux;

procedure Tester_Matrice_Creuse is
    function Egalite(M1 : T_Matrice_creuse; M2 : T_Matrice_creuse) return Boolean is
        Egal : Boolean;
        seuil : Float;
    begin
        seuil := 0.1;
        Egal := M1'Last = M2'Last;
        if not Egal then
            return Egal;
        else
            for i in 1..M1'Last loop
                for j in 1..M2'Last loop
                    if (Valeur(M1(i),j)-Valeur(M2(i),j)) > seuil then
                        Egal := False;
                    else
                        Null;
                    end if;
                end loop;
            end loop;
            return Egal;
        end if;
    end Egalite;

    function Egalite_V(M1 : T_Vecteur; M2 : T_Vecteur) return Boolean is
        Egal : Boolean;
        seuil : Float;
    begin
        seuil := 0.01;
        Egal := M1'Last = M2'Last;
        if not Egal then
            return Egal;
        else
            for i in 1..M1'Last loop
                if (M1(i)-M2(i)) > seuil then
                    Egal := False;
                else
                    Null;
                end if;
            end loop;
            return Egal;
        end if;
    end Egalite_V;



    procedure Tester_Lire is
        file : Unbounded_String;
        Matrice : T_Matrice_creuse(1..3);
        Matrice_vrai : T_Matrice_creuse(1..3);
        F : File_Type;
    begin
        file := To_Unbounded_String("sujet_test.net");
        Create(F,Out_File,To_String(file));
        Put_Line(F,"3");
        Put_Line(F,"0 1");
        Put_Line(F,"0 2");
        Close(F);
        for i in 1..3 loop
            Initialiser(Matrice_vrai(i));
        end loop;
        Modifier(Matrice_vrai(1),2,1.0);
        Modifier(Matrice_vrai(2),1,1.0);
        Modifier(Matrice_vrai(1),3,1.0);
        Modifier(Matrice_vrai(3),1,1.0);
        Lire_Sujet_creuse(To_String(file),Matrice);
        Put("Tester_Lire : ");
        if Egalite(Matrice,Matrice_vrai) then
            Put("Passed");
        else

            Put("Failed");
        end if;
        New_Line;
    end Tester_Lire;

    procedure Tester_CalculerH is
        Matrice : T_Matrice_creuse(1..3);
        Matrice_vrai : T_Matrice_creuse(1..3);
    begin
        for i in 1..3 loop
            Initialiser(Matrice(i));
        end loop;
        Modifier(Matrice(1),2,1.0);
        Modifier(Matrice(2),1,1.0);
        Modifier(Matrice(1),3,1.0);
        Modifier(Matrice(3),1,1.0);
        CalculerH_creuse(Matrice);
        for i in 1..3 loop
            Initialiser(Matrice_vrai(i));
        end loop;
        Modifier(Matrice_vrai(1),2,0.5);
        Modifier(Matrice_vrai(2),1,1.0);
        Modifier(Matrice_vrai(1),3,0.5);
        Modifier(Matrice_vrai(3),1,1.0);
        Put("Tester_CalculerH : ");
        if Egalite(Matrice,Matrice_vrai) then
            Put("Passed");
        else
            Put("Failed");
        end if;
        New_Line;
    end Tester_CalculerH;

    procedure Tester_CalculerPi is
        Matrice : T_Matrice_creuse(1..3);
        Matrice_vrai : T_Vecteur(1..3);
        pi : T_Vecteur(1..3);
        alpha : Float;
    begin
        alpha := 0.85;
        for i in 1..3 loop
            Initialiser(Matrice(i));
            Matrice_vrai(i) := 0.0;
        end loop;
        Modifier(Matrice(1),2,1.0/2.0);
        Modifier(Matrice(1),3,1.0/2.0);
        Modifier(Matrice(2),3,1.0);
        Modifier(Matrice(3),1,1.0/3.0);
        Modifier(Matrice(3),2,1.0/3.0);
        Modifier(Matrice(3),3,1.0/3.0);
        CalculerPi_creuse(Matrice,0.1,pi,3,150,alpha);
        Matrice_vrai(1) := 0.211343;
        Matrice_vrai(2) := 0.272732;
        Matrice_vrai(3) := 0.515926;
        Put("Tester_CalculerPi : ");
        if Egalite_V(pi,Matrice_vrai) then
            Put("Passed");
        else
            Put("Failed");
        end if;
        New_Line;
    end Tester_CalculerPi;

begin
    Tester_Lire;
    Tester_CalculerH;
    Tester_CalculerPi;
end Tester_Matrice_Creuse;
