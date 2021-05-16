!! This program contains many lexical errors
!! It tests mainly your transition table
!! Use MY output to adjust
!! YOUR error state transitions in YOUR transition table 
!! You should create your own test file with more errors 
!1 Wrong comment
PROGRAm {
i=000;  !! legal zero decimal literal
n=001;  !! illegal decimal literal
j=0#;   !! syntax error
k=2I;   !! syntax error
k=0O;   !! digit 0 followed by a letter O
a% = 0.0L; !! syntax error
b. = 0.L   !! syntax error
USING> = .0 !! lexical errors,illegal floating point literal
Input(ElSe)
If=0.l  !! letter l not digit 1
o=0x1Ga  !! syntax error
b10 = 1O1.0; !! letter O follows 1
3C=7.0O2;    !! digit in VID and letter O precedes 2 
name#S = Last# ## " S.#" !! wrong string VID
WHILE FALSE(b10 > 0x00 _OR_ <= b10_AND_ a != o.o)DO{
  a=a+1..;
  c=001.1; !! leading zero not allowed in floating point
}
OUTPUT##("No  Luck  Today);
OUTPUT (""My Scanner has gone astray");
} ! !Wrong comment
"There is always one more bug