MAIN{
a = a .AND. a;
b = b .OR. b;
IF TRUE (a+a + b-b - c/c + d*d)
THEN { a=b;} ELSE{};
IF TRUE(c/c - d*d)
THEN {a=b;} ELSE{};
IF TRUE(a==a + b==b)
THEN {a=b;} ELSE{};
IF TRUE (c==c - d==d)
THEN {a=b;} ELSE{};
a = a == b;
b = b != b; 
}