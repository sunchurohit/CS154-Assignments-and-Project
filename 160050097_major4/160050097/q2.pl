
choose(A) :- A=[X,Y,Z],
             member(X,[g,y,r]),
             member(Y,[g,y,r]),
             member(Z,[g,y,r]),
             \==(A,[r,r,r]),
             \==(A,[y,y,y]).

choose1(A,B,C) :- choose(X),X=[A,B,C].

goal(Color,Combination) :- Color=C,
			   Combination=[A,B,C],
			   member(A,[r,y,g]),
			   member(B,[r,y,g]),
			   member(C,[r,y,g]),
			   choose1(A,B,C),
                           \==(Combination,[A,r,r]),
                           \==(Combination,[A,y,y]),
                           \==(Combination,[y,B,y]),
                           \==(Combination,[r,B,r]),
                           \==(Combination,[A,B,y]),
                           \==(Combination,[A,B,r]).
