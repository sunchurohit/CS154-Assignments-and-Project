
state(Crossing,[0,0,right],FINAL) :- FINAL = [[0,0,right]|Crossing],!.

state(Crossing,[X,Y,left],Final) :-  ((X1 is X-1,Y1 = Y,
				       X2 is 3-X1,Y2 is 3-Y1,
				       (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				      (X1 = X,Y1 is Y-1,
				       X2 is 3-X1,Y2 is 3-Y1,
				       (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				      (X1 is X-1,Y1 is Y-1,
				       X2 is 3-X1,Y2 is 3-Y1,
				       (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				      (X1 is X-2,Y1 = Y,
				       X2 is 3-X1,Y2 is 3-Y1,
				       (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				      (X1 = X,Y1 is Y-2,
				       X2 is 3-X1,Y2 is 3-Y1,
				       (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0))),
				     member(X1,[0,1,2,3]),
				     member(Y1,[0,1,2,3]),
				     member(X2,[0,1,2,3]),
				     member(Y2,[0,1,2,3]),
				     not(member([X,Y,left],Crossing)),
				     Crossing1 = [[X,Y,left]|Crossing],
				     state(Crossing1,[X1,Y1,right],Final).

state(Crossing,[X,Y,right],Final) :-  ((X1 is X+1,Y1 = Y,
				        X2 is 3-X1,Y2 is 3-Y1,
				        (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				       (X1 = X,Y1 is Y+1,
				        X2 is 3-X1,Y2 is 3-Y1,
				        (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				       (X1 is X+1,Y1 is Y+1,
				        X2 is 3-X1,Y2 is 3-Y1,
				        (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				       (X1 is X+2,Y1 = Y,
				        X2 is 3-X1,Y2 is 3-Y1,
				        (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0));
				       (X1 = X,Y1 is Y+2,
				        X2 is 3-X1,Y2 is 3-Y1,
				        (X1 = 0;(X1 >= Y1,X2 >= Y2,\==(X1,0),\==(X2,0));X2 = 0))),
				     member(X1,[0,1,2,3]),
				     member(Y1,[0,1,2,3]),
				     member(X2,[0,1,2,3]),
				     member(Y2,[0,1,2,3]),
				     not(member([X,Y,right],Crossing)),
				     Crossing1 = [[X,Y,right]|Crossing],
				     state(Crossing1,[X1,Y1,left],Final).

safe(T) :- state([],[3,3,left],H),
	   reverse(H,T).
