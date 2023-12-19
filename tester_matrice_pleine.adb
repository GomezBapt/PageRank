with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Matrice_Pleine;        use Matrice_Pleine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Tester_Matrice_Pleine is
    function Egalite(M1 : T_Matrice; M2 : T_Matrice) return Boolean is
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
                    if (M1(i,j)-M2(i,j)) > seuil then
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
        Matrice : T_Matrice(1..3,1..3);
        Matrice_vrai : T_Matrice(1..3,1..3);
        F : File_Type;
    begin
        file := To_Unbounded_String("sujet_test.net");
        Create(F,Out_File,To_String(file));
        Put_Line(F,"3");
        Put_Line(F,"0 1");
        Put_Line(F,"0 2");
        Close(F);
        for i in 1..3 loop
            for j in 1..3 loop
                Matrice_vrai(i,j) := 0.0;
            end loop;
        end loop;
        Matrice_vrai(1,2) := 1.0;
        Matrice_vrai(2,1) := 1.0;
        Matrice_vrai(1,3) := 1.0;
        Matrice_vrai(3,1) := 1.0;
        Lire_Sujet(To_String(file),Matrice);
        Put("Tester_Lire : ");
        if Egalite(Matrice,Matrice_vrai) then
            Put("Passed");
        else

            Put("Failed");
        end if;
        New_Line;
    end Tester_Lire;

    procedure Tester_CalculerH is
        Matrice : T_Matrice(1..3,1..3);
        Matrice_vrai : T_Matrice(1..3,1..3);
    begin
        for i in 1..3 loop
            for j in 1..3 loop
                Matrice(i,j) := 0.0;
            end loop;
        end loop;
        Matrice(1,2) := 1.0;
        Matrice(2,1) := 1.0;
        Matrice(1,3) := 1.0;
        Matrice(3,1) := 1.0;
        CalculerH(Matrice);
        for i in 1..3 loop
            for j in 1..3 loop
                Matrice_vrai(i,j) := 0.0;
            end loop;
        end loop;
        Matrice_vrai(1,2) := 0.5;
        Matrice_vrai(2,1) := 1.0;
        Matrice_vrai(1,3) := 0.5;
        Matrice_vrai(3,1) := 1.0;
        Put("Tester_CalculerH : ");
        if Egalite(Matrice,Matrice_vrai) then
            Put("Passed");
        else
            Put("Failed");
        end if;
        New_Line;
    end Tester_CalculerH;

    procedure Tester_CalculerS is
        Matrice : T_Matrice(1..3,1..3);
        Matrice_vrai : T_Matrice(1..3,1..3);
    begin
        for i in 1..3 loop
            for j in 1..3 loop
                Matrice(i,j) := 0.0;
                Matrice_vrai(i,j) := 0.0;
            end loop;
        end loop;
        Matrice(1,2) := 1.0/2.0;
        Matrice(1,3) := 1.0/2.0;
        Matrice(2,3) := 1.0;

        CalculerS(Matrice);

        Matrice_vrai(1,2) := 1.0/2.0;
        Matrice_vrai(1,3) := 1.0/2.0;
        Matrice_vrai(2,3) := 1.0;
        Matrice_vrai(3,1) := 1.0/3.0;
        Matrice_vrai(3,2) := 1.0/3.0;
        Matrice_vrai(3,3) := 1.0/3.0;
        Put("Tester_CalculerS : ");
        if Egalite(Matrice,Matrice_vrai) then
            Put("Passed");
        else
            Put("Failed");
        end if;
        New_Line;

    end Tester_CalculerS;

    procedure Tester_CalculerG is
        Matrice : T_Matrice(1..3,1..3);
        Matrice_vrai : T_Matrice(1..3,1..3);
        alpha : Float;
        N : Float;
    begin
        N := 3.0;
        alpha := 0.85;
        for i in 1..3 loop
            for j in 1..3 loop
                Matrice(i,j) := 0.0;
                Matrice_vrai(i,j) := 0.0;
            end loop;
        end loop;
        Matrice(1,2) := 1.0/2.0;
        Matrice(1,3) := 1.0/2.0;
        Matrice(2,3) := 1.0;
        Matrice(3,1) := 1.0/3.0;
        Matrice(3,2) := 1.0/3.0;
        Matrice(3,3) := 1.0/3.0;
        CalculerG(Matrice,alpha);

        Matrice_vrai(1,2) := (alpha * 1.0/2.0) + (1.0-alpha)/N;
        Matrice_vrai(1,3) := (alpha * 1.0/2.0) + (1.0-alpha)/N;
        Matrice_vrai(2,3) := (alpha * 1.0) + (1.0-alpha)/N;
        Matrice_vrai(3,1) := (alpha * 1.0/3.0) + (1.0-alpha)/N;
        Matrice_vrai(3,2) := (alpha * 1.0/3.0) + (1.0-alpha)/N;
        Matrice_vrai(3,3) := (alpha * 1.0/3.0) + (1.0-alpha)/N;
        Put("Tester_CalculerG : ");
        if Egalite(Matrice,Matrice_vrai) then
            Put("Passed");
        else
            Put("Failed");
        end if;
        New_Line;
    end Tester_CalculerG;

    procedure Tester_CalculerPi is
        Matrice : T_Matrice(1..3,1..3);
        Matrice_vrai : T_Vecteur(1..3);
        pi : T_Vecteur(1..3);
        alpha : Float;
    begin
        alpha := 0.85;
        for i in 1..3 loop
            for j in 1..3 loop
                Matrice(i,j) := 0.0;
                Matrice_vrai(i) := 0.0;
            end loop;
        end loop;
        Matrice(1,2) := 1.0/2.0;
        Matrice(1,3) := 1.0/2.0;
        Matrice(2,3) := 1.0;
        Matrice(3,1) := 1.0/3.0;
        Matrice(3,2) := 1.0/3.0;
        Matrice(3,3) := 1.0/3.0;
        CalculerG(Matrice,alpha);
        CalculerPi(Matrice,0.1,pi,3,150);
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

    procedure Tester_Tri is
        Matrice : T_Vecteur(1..3);
        Matrice_vrai : T_Vecteur(1..3);
        indices : T_Vecteur(1..3);
    begin
        Matrice(1) := 9.0;
        Matrice(2) := 5.0;
        Matrice(3) := 15.0;
        Tri(Matrice,indices);
        Matrice_vrai(1) := 15.0;
        Matrice_vrai(2) := 9.0;
        Matrice_vrai(3) := 5.0;
        Put("Tester_Tri : ");
        if Egalite_V(Matrice,Matrice_vrai) then
            Put("Passed");
        else
            Put("Failed");
        end if;
        New_Line;
    end Tester_Tri;

begin
    Tester_Lire;
    Tester_CalculerH;
    Tester_CalculerS;
    Tester_CalculerG;
    Tester_CalculerPi;
    Tester_Tri;
end Tester_Matrice_Pleine;
