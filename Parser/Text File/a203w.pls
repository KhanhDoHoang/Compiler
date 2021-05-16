%%This program contains many lexical errors
%%It tests mainly your transition table
%% Use MY output to adjust
%% YOUR error state transitions in YOUR transition table 
%%You should create your own test file with more errors 
%1Wrong comment
Main {
i=000;       %% legal zero decimal literal
n=01.01;     %% legal float literal
j=0#;        %% syntax error
k=2I;        %% divided into integer and VID
k=0O;        %% digit 0 followed by a letter O
a% = 0.0L;   %% syntax error
b. = 0.L     %% divided into float and VID
USING> = .0  %% lexical errors,illegal floating point literal
Read(ElSe)   %% legal AVID (not keywords)
If=0.l       %% letter l not digit 1 (error in end)
o=0x1Ga      %% syntax error
b10 = 1O1.0; %% letter O follows 1
3C=7.0O2;    %% digit in VID and letter O precedes 2 
name$S = $Last$ ++ ' S.$' %% wrong string VID
WHILE FALSE(b10 > 0x00 OR. <= b10.AND a != o.o)LOOP{
  a=a+1..;
  c=001.1;   %% leading zero as floating point
}
WRITE$$('No  Luck  Today);
WRITE (''My Scanner has gone astray');
}            % %Wrong comment
'There is always one more bug