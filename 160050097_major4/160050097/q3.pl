
state(Movements1,pair(X,Y),Final) :- length(Movements1,48),
				     not(member(pair(X,Y),Movements1)),
				     Final = [pair(X,Y)|Movements1],!.

state(Movements1,pair(X,Y),Final) :- abs2((X1-X),2),abs2((Y1-Y),1),
			             not(member(pair(X,Y),Movements1)),
			             Movements = [pair(X,Y)|Movements1],
			             state(Movements,pair(X1,Y1),Final).

state(Movements1,pair(X,Y),Final) :- abs2((X1-X),1),abs2((Y1-Y),2),
			             not(member(pair(X,Y),Movements1)),
			             Movements = [pair(X,Y)|Movements1],
			             state(Movements,pair(X1,Y1),Final).

tour(T) :- state([],pair(1,1),G),
	   reverse(G,T).

abs2((X-V),Y) :- (X is V+Y;X is V-Y),
		 >(X,0),
		 <(X,8).

 
		   
		     
