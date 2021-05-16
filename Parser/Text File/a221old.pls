!! This program calculates the sum of 32767 even numbers.
!! The program is "lexically" correct
!! and should not generate any error
PROGRAM {
 
 a=+0.0;
 
 sum008 = 7.87050 ;
 INPUT(a,sum008);
 i=0; 
 WHILE TRUE (i < 32767 _OR_ i == 32767)DO{
   i = i + 2;
   a=
   a*i/0.5
   ;
   sum008 = sum008 + a - 1 ;
 };
 IF TRUE(text$ == "")THEN {
   text$ = "prog" $$ "ram";
 }
 ELSE {
   text$ = text$ $$ "ram";
 };
 OUTPUT("\* This is a platypus -:)-<-<-- \*");
 OUTPUT(text$);
 
 IF FALSE (text$ == "program"_OR_sum008<>8._AND_i>-10)THEN {
  OUTPUT(sum008);
  OUTPUT();
 }
 ELSE{};
}
