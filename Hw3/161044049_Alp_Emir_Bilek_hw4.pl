:-dynamic
	rpath/2.      
flight(ankara,istanbul).
flight(van,istanbul).
flight(rize,istanbul).
flight(izmir,istanbul).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(edremit,erzincan).
flight(edremit,edirne).
flight(isparta,burdur).
flight(isparta,izmir).
flight(istanbul,van).
flight(istanbul,rize).
flight(van,rize).
flight(van,ankara).
flight(ankara,konya).
flight(konya,antalya).
flight(antalya,gaziantep).
flight(erzincan,edremit).
flight(edirne,edremit).
flight(burdur,isparta).
flight(izmir,isparta).
flight(istanbul,izmir).
flight(antalya,istanbul).
flight(gaziantep,istanbul).
flight(rize,van).
flight(ankara,van).
flight(konya,ankara).
flight(antalya,konya).
flight(gaziantep,antalya).
route(X, Y):- route(X, Y, [X]).
route(X, Y, _):- flight(X, Y).
route(X, Y, Visited):-  flight(X, Z),
                        not(member(Z, Visited)),
                        route(Z, Y, [Z|Visited]).



distance(isparta,izmir,308).
distance(izmir,istanbul,328).
distance(erzincan,edremit,1026).
distance(edirne,edremit,243).
distance(burdur,isparta,24).
distance(izmir,isparta,308).
distance(istanbul,izmir,328).
distance(antalya,istanbul,482).
distance(gaziantep,istanbul,847).
distance(rize,van,373).
distance(ankara,van,920).
distance(konya,ankara,227).
distance(antalya,konya,192).
distance(gaziantep,antalya,592).
distance(istanbul,antalya,482).
distance(istanbul,gaziantep,847).
distance(istanbul,ankara,351).
distance(istanbul,van,1262).
distance(istanbul,rize,967).
distance(van,rize,373).
distance(van,ankara,920).
distance(ankara,konya,227).
distance(konya,antalya,192).
distance(antalya,gaziantep,592).
distance(ankara,istanbul,351).
distance(van,istanbul,1262).
distance(edremit,erzincan,1026).
distance(edremit,edirne,243).
distance(isparta,burdur,24).
distance(rize,istanbul,967).


path(From,To,Dist) :- distance(From,To,Dist).
 
shorterPath([H|Path], Dist) :-		                  
	rpath([H|T], D), !, Dist < D,                      
	retract(rpath([H|_],_)),
	assert(rpath([H|Path], Dist)).
shorterPath(Path, Dist) :-		                                   
	assert(rpath(Path,Dist)).
 

sroute(From, To, X) :-
	traverse(From),                               
	rpath([To|RPath], Dist)->                           
	  reverse([To|RPath], Path),                        
      Distance is round(Dist),
      X is Distance.
	  
traverse(From, Path, Dist) :-		                  
	path(From, T, D),		                                     
	not(memberchk(T, Path)),	                         
	shorterPath([T,From|Path], Dist+D),                   
	traverse(T,[From|Path],Dist+D).	                     
 
traverse(From) :-
	retractall(rpath(_,_)),                          
	traverse(From,[],0).                             
traverse(_).







when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).
where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).
enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).
schedule(a,P,T) :- enroll(a,X),where(X,P),when(X,T).
usage(P,T) :- where(X,P),when(X,T).
conflictByPlace(X,Y) :-  where(X,A),where(Y,A),not(X == Y).
conflictByTime(X,Y) :- when(X,A),when(Y,A),not(X == Y).
conflict(X,Y) :-  conflictByPlace(X,Y) ; conflictByTime(X,Y).
meet(X,Y) :- not(X = Y),enroll(X,A),enroll(Y,A).





element1(X,[X|_]).
element1(X,[Y|T]) :- member(X,T).


union1([],S,S).
union1(S,[],S):-S\=[].
union1([X|TX],[X|TY],[X|TZ]):-
   union1(TX,TY,TZ).
union1([X|TX],[Y|TY],[X|TZ]):-
   (X < Y),
   union1(TX,[Y|TY],TZ).
union1([X|TX],[Y|TY],[Y|TZ]):-
   (Y < X),
   union1([X|TX],TY,TZ).

intersection1([],S,[]).
intersection1(S,[],[]):-S\=[].
intersection1([X|TX],[X|TY],[X|TZ]):-
   intersection1(TX,TY,TZ).
intersection1([X|TX],[Y|TY],TZ):-
   (X < Y),
   intersection1(TX,[Y|TY],TZ).
intersection1([X|TX],[Y|TY],TZ):-
   (Y < X),
   intersection1([X|TX],TY,TZ).
equal1([X|TX],[Y|TY]) :- 
    (X == Y );
    (element1(X,TY),
    element1(Y,TX)).
	
	
	
	




