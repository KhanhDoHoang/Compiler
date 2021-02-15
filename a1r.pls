%% This program calculates the sum of 32767 even numbers.
%% The program is "lexically" correct
%% and should not generate any error
MAIN {
 
 a=+1.2;
 
 sum008 = 7.87050 ;
 READ(a,sum008);
 i=0; 
 WHILE TRUE (i < 32767 .OR. i == 32767)DO{
   i = i + 2;
   a=
   a*i/0.5
   ;
   sum008 = sum008 + a - 1 ;
 };
 IF TRUE($text$ == '')THEN {
   $text$ = 'prog' ++ 'ram';
 }
 ELSE {
   $text$ = $text$ ++ 'ram';
 };
 WRITE('\* This is a program -:)-<-<-- \*');
 WRITE($text$);
 
 IF FALSE ($text$ == 'program'.OR.sum008!=8..AND.i>10)THEN {
  WRITE(sum008);
  WRITE();
 }
 ELSE{};
}
