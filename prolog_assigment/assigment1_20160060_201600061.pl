%first line
connected(new_elmarg,elmarg).
connected(elmarg,ezbet_elnakhl).
connected(ezbet_elnakhl,ain_shams).
connected(ain_shams,elmatareyya).
connected(elmatareyya,helmeyet_elzaitoun).
connected(helmeyet_elzaitoun,hadayeq_elzaitoun).
connected(hadayeq_elzaitoun,saray_elqobba).
connected(saray_elqobba,hammamat_elqobba).
connected(hammamat_elqobba,kobri_elqobba).
connected(kobri_elqobba,manshiet_elsadr).
connected(manshiet_elsadr,eldemerdash).
connected(eldemerdash,ghamra).
connected(ghamra,alshohadaa).%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(alshohadaa,urabi).
connected(urabi,nasser).
connected(nasser,sadat).
connected(sadat,saad_zaghloul).
connected(saad_zaghloul, alsayyeda_zeinab).
connected(alsayyeda_zeinab,elmalek_elsaleh).
connected(elmalek_elsaleh,margirgis).
connected(margirgis,elzahraa).
connected(elzahraa,dar_elsalam).
connected(dar_elsalam,hadayeq_elmaadi).
connected(hadayeq_elmaadi,maadi).
connected(maadi,thakanat_elmaadi).
connected(thakanat_elmaadi,tora_elbalad).
connected(tora_elbalad,kozzika).
connected(kozzika,tora_elasmant).
connected(tora_elasmant,elmaasara).
connected(elmaasara,hadayeq_helwan).
connected(hadayeq_helwan,wadi_hof).
connected(wadi_hof,helwan_university).
connected(helwan_university,ain_helwan).
connected(ain_helwan,helwan).
%second line
connected(shobra_elkheima,koliet_elzeraa).
connected(koliet_elzeraa,mezallat).
connected(mezallat,khalafawy).
connected(khalafawy,sainte_teresa).
connected(sainte_teresa,road_elfarag).
connected(road_elfarag,massara).
connected(massara,alshohadaa).
connected(alshohadaa,ataba).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(ataba,naguib).
connected(naguib,sadat).
connected(sadat,opera).
connected(opera,dokki).
connected(dokki,bohooth).
connected(bohooth,cairo_university).
connected(cairo_university,faisal).
connected(faisal,giza).
connected(giza,omm_elmisryeen).
connected(omm_elmisryeen,sakiat_mekki).
connected(sakiat_mekki,elmounib).






%Task 1:

path(X,Y,N,L):-
get_list(X,Y,N,L), writeln(''),printlist(L).


get_list(Y,Y,any,[]):-!.
get_list(Y,Y,N,[]):-!.
get_list(X,Y,N,L):-
((N=='any')->connected(X,Z),get_list(Z,Y,N,R),
L=[[X,Z]|R];
(N>0)->
connected(X,Z),Newn is N-1,
get_list(Z,Y,Newn,R),
L=[[X,Z]|R]).




printlist([]):-!.
printlist([First|Tail]) :-
  write(First),
  printlist(Tail).

%-------------------------------------------
%Task two:

nstation(Dist,Counter):-  %dist---> distnation
list_stations(Dist, [], List),count(List,Counter).

list_stations(Dist, List, L) :-
    connected(Another_station, Dist),
    \+ exist(Another_station, List),

    list_stations(Dist, [Another_station|List], L) ;
    connected(Dist,Another_station),
    \+ exist(Another_station, List),
    list_stations(Dist, [Another_station|List], L).% exit when all the station which connected to this station are in the list.

list_stations(_, L, L).% to make the list empty and fill the result

exist(X, [Y|T]) :- %like member funtion
X = Y; exist(X, T).

count([],0).%to count the number of station on list
count([_|T], N) :- count(T, N1), N is N1 + 1.

%------------------------------------------------------------
%Task three:

cost(X,Y,P):-
calcstations(X,Y,L),count(L,N),((N =< 7,not(findline(L)))->P is 3;((N > 7,N < 16);findline(L))->P is 5;(N >= 16) -> P is 7),!.
%-----------------------
calcstations(X,X,[]):-!.
calcstations(X,L,Y):-
connected(X,Z),calcstations(Z,L,R),Y=[X|R].

%--------------------------------------------
findline([]):-!.
findline([H,T|_]):-(H==sadat);findline(T);findchange(H,T).
%--------------------------------------------
linechange(X,Y):-
(X==nasser,Y==opera)->!.
linechange(X,Y):-
(X==opera,Y==nasser)->!.
linechange(X,Y):-
(X==saad_zaghloul,Y==naguib)->!.
linechange(X,Y):-
(X==naguib,Y==saad_zaghloul)->!.
linechange(X,Y):-
(X==saad_zaghloul,Y==opera)->!.
linechange(X,Y):-
(X==opera,Y==saad_zaghloul)->!.
linechange(X,Y):-
(X==nasser,Y==naguib)->!.
linechange(X,Y):-
(X==naguib,Y==nasser)->!.

%----------------------------------------------
findchange(X,Y):-
connected(X,sadat),connected(sadat,Y),linechange(X,Y).
findchange(X,Y):-
connected(sadat,X),connected(Y,sadat),linechange(X,Y).

%! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%part two:

word(zombifies, z,o,m,b,i,f,i,e,s).
word(akecabele, a,k,e,c,a,b,e,l,e).
word(brickwork, b,r,i,c,k,w,o,r,k).
word(backcheck, b,a,c,k,c,h,e,c,k).
word(acmrremad, a,c,m,r,r,e,m,a,d).
word(nhgwpfabz, n,h,g,w,p,f,a,b,z).
word(jellybean, j,e,l,l,y,b,e,a,n).
word(earreoded, e,a,r,r,e,o,d,e,d).

crossword(V1, V2, V3, V4, H1, H2, H3, H4) :-
       (word(V1, _, V1H1, _, V1H2, _, V1H3, _, V1H4, _),
	word(V2, _, V2H1, _, V2H2, _, V2H3, _, V2H4, _),
	word(V3, _, V3H1, _, V3H2, _, V3H3, _, V3H4, _),
	word(V4, _, V4H1, _, V4H2, _, V4H3, _, V4H4, _),
	word(H1, _, V1H1, _, V2H1, _, V3H1, _, V4H1, _),
	word(H2, _, V1H2, _, V2H2, _, V3H2, _, V4H2, _),
	word(H3, _, V1H3, _, V2H3, _, V3H3, _, V4H3, _),
	word(H4, _, V1H4, _, V2H4, _, V3H4, _, V4H4, _),

	not(H1 = V1),
	not(H2 = V2),
	not(H3 = V3),
	not(H4 = V4));writeln("no solution"),fail.
