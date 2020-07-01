% yagmur goktas
% 2017400018
% compiling: yes
% complete: yes

% artist(ArtistName, Genres, AlbumIds).
% album(AlbumId, AlbumName, ArtistNames, TrackIds).
% track(TrackId, TrackName, ArtistNames, AlbumName, [Explicit, Danceability, Energy,
%                                                    Key, Loudness, Mode, Speechiness,
%                                                    Acousticness, Instrumentalness, Liveness,
%                                                    Valence, Tempo, DurationMs, TimeSignature]).

features([explicit-0, danceability-1, energy-1,
          key-0, loudness-0, mode-1, speechiness-1,
       	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).
filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
    (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
        (Use is 0,
            FilteredFeatures = FilteredTail
        )
    ).

getArtistTracks(ArtistName,T,TrackNames):-artist(ArtistName,_,[X|AlbumList]),
                                        album(X,_,_,L),
                                        getList(AlbumList,L,T),
                                        match(T,TrackNames).

% concatanetas the trackids of albums.
getList([X|Restofalbum],L,T):- album(X,_,_,P),
                            append(L,P,S),
                            getList(Restofalbum,S,T).
getList([],L,L).

% matches trackids with names.
match([X],[Y]):-track(X,Y,_,_,_).
match([X|TrackIdList],[Y|TrackIdNames]):-track(X,Y,_,_,_),
                        match(TrackIdList,TrackIdNames).

% returns the features.
f(TrackId,L):-track(TrackId,_,_,_,L).

% sum of multiple lists.
sumlist([H|T], R) :- 
    f(H, P),
    sp(P, T, R).
    
% helper method to sum_list
sp(H1, [H|T], R) :-
    f(H,S),
    sum(H1, S, X),
    sp(X, T, R).

sp(H, [], H ).

% adding of two lists.
sum([H1|T1],[H2|T2],[X|L3]) :- 
    sum(T1,T2,L3), X is H1+H2.

sum([],[],[]).

% divides every element of a list to a number.
divd([],_,[]).
divd([X|L1],S,[Y|L2]):-  Y is X/S,
                    divd(L1,S,L2).
                



albumFeatures(AlbumId,Filtered):- album(AlbumId,_,_,TrackIds),
                                    sumlist(TrackIds,R),
                                    length(TrackIds,S),
                                    divd(R,S,AlbumFeatures),
                                    filter_features(AlbumFeatures,Filtered).
                                    

artistFeatures(ArtistName,Filtered):- findall(X,track(X,_,[ArtistName|_],_,_),Bag),
                                        sumlist(Bag,R),
                                        length(Bag,S),
                                        divd(R,S,ArtistFeatures),
                                        filter_features(ArtistFeatures,Filtered).

trackDistance(TrackId1,TrackId2,Score):- track(TrackId1,_,_,_,L1),
                                         filter_features(L1,Filtered1),
                                        track(TrackId2,_,_,_,L2),
                                       filter_features(L2,Filtered2),
                                        dist(Filtered1,Filtered2,Score).

% find Euclidian distance between two features list.
dist(L,L,0):- L = L.
dist(L1,L2,Distance):-  dif(L1,L2,L3),
                        square(L3,L4),
                        summ(L4,S),
                        Distance is sqrt(S).

% subtracts every element of lists from each other.
dif([],[],[]).
dif([X|L1],[Y|L2],[Z|L3]):- Z is X-Y,
                            dif(L1,L2,L3).

% returns a list with square of every element 
square([],[]).
square([X|L1],[Y|L2]):- Y is X*X,
                        square(L1,L2).

% finds sum of elements of the list.
summ([],0).
summ([X|L],S):-summ(L,S1), S is S1 + X.


albumDistance(AlbumId1,AlbumId2,Score) :-   albumFeatures(AlbumId1,L1),
                                            albumFeatures(AlbumId2,L2),
                                            dist(L1,L2,Score).

artistDistance(ArtistName1,ArtistName2,Score) :- artistFeatures(ArtistName1,L1),
                                                artistFeatures(ArtistName2,L2),
                                                dist(L1,L2,Score).

% makes pair of distance and trackid in order to sort.
make_pair(TrackId1,TrackId2,[X,Y]):-  Y = TrackId1,
                                    trackDistance(TrackId1,TrackId2,X).

% makes list from pairs of distance and trackid.
make_list([X|Bag],TrackId,[Y|Result]):-make_pair(X,TrackId,Y),
                                        make_list(Bag,TrackId,Result).

make_list([],_,[]).

% takes first N item from the list. 
take(_, 0, []).
take([[_,X]|T], N,[X | L1]) :-M is N-1,
   take(T,M, L1).

% deletes first item in list.
ilkisil([_|Yeni],Yeni).

% makes pair of distance and albumid.
make_pair2(AlbumId1,AlbumId2,[X,Y]):- Y = AlbumId1,
                                    albumDistance(AlbumId1,AlbumId2,X).

% makes list from pairs of albumid and distance.
make_list2([X|Bag],AlbumId,[Y|Result]):-make_pair2(X,AlbumId,Y),
                                        make_list2(Bag,AlbumId,Result).

make_list2([],_,[]).

% makes pair of distance and artist name in order to sort.
make_pair3(ArtistName1,ArtistName2,[X,Y]):- Y = ArtistName1,
                                    artistDistance(ArtistName1,ArtistName2,X).

% makes list from pairs of artistname and distance. 
make_list3([X|Bag],ArtistName,[Y|Result]):-make_pair3(X,ArtistName,Y),
                                        make_list3(Bag,ArtistName,Result).
make_list3([],_,[]).

% matches albumids with album names. 
match2([X],[Y]):-album(X,Y,_,_).
match2([X|SimIds],[Y|SimNames]):- album(X,Y,_,_),
                                    match2(SimIds,SimNames).

findMostSimilarTracks(TrackId,SimIds,SimNames):-
                                                findall(X,track(X,_,_,_,_),Bag),
                                                make_list(Bag,TrackId,List),
                                                sort(List,Sorted),
                                                take(Sorted,31,Yeni),
                                                ilkisil(Yeni,SimIds),
                                                match(SimIds,SimNames).

findMostSimilarAlbums(AlbumId,SimIds,SimNames):-
                                                findall(X,album(X,_,_,_),Bag),
                                                make_list2(Bag,AlbumId,List),
                                                sort(List,Sorted),
                                                take(Sorted,31,Yeni),
                                                ilkisil(Yeni,SimIds),
                                                match2(SimIds,SimNames).

findMostSimilarArtists(ArtistName,SimArtists) :-
                                                findall(X,artist(X,_,_),Bag),
                                                make_list3(Bag,ArtistName,List),
                                                sort(List,Sorted),
                                                take(Sorted,31,Yeni),
                                                ilkisil(Yeni,SimArtists).

filterExplicitTracks([X|TrackList],L):- track(X,_,_,_,[1|_]),
                                        filterExplicitTracks(TrackList,L).

filterExplicitTracks([X|TrackList],[X|L]):- track(X,_,_,_,[0|_]),
                                            filterExplicitTracks(TrackList,L).

filterExplicitTracks([],[]).

getGenres([],L,L).
getGenres([X|ArtistNames],L,Genres):-artist(X,P,_),
                                    union(L,P,S),
                                    getGenres(ArtistNames,S,Genres).

getTrackGenre(TrackId,Genres):-track(TrackId,_,ArtistNames,_,_),
                                getGenres(ArtistNames,[],Genres).

% method gives us trackıd and distance pairs in order to sort.
make_pair4(Features,TrackId,[X,Y]):-Y = TrackId,
                                    track(TrackId,_,_,_,L),
                                    filter_features(L,F),
                                    dist(Features,F,X).

% makes list of pairs. 
make_list4([X|Bag],Features,[Y|B]):-make_pair4(Features,X,Y),
                                    make_list4(Bag,Features,B).
make_list4([],_,[]).

mem(Y,[X|_],"T"):-sub_string(X,_,_,_,Y),!.
mem(Y,[_|G],A):-mem(Y,G,A).
mem(_,[],"F").

% this method gets the trackIds of tracks from Liked Genres.
getLike(L,[Y|Like],Son):-findall(X,(track(X,_,_,_,_),getTrackGenre(X,G),mem(Y,G,"T")),Bag),
                        union(L,Bag,S),
                        getLike(S,Like,Son).
                        
getLike(L,[],L).


% This method gets TrackIds of the disliked tracks from Liked tracks.
getDislike(L,[],L,_).
getDislike(L,[Y|Dislike],Result,Son):-  findall(X,(member(X,Son),getTrackGenre(X,G),mem(Y,G,"T")),Bag),
                                        union(L,Bag,S),
                                        getDislike(S,Dislike,Result,Son).

getArtists([],[]).
getArtists([X|Playlist],[Y|Artists]):-track(X,_,Y,_,_),
                                    getArtists(Playlist,Artists).

take2(_, 0, [],[]).
take2([[Y,X]|T], N,[X | L1],[Y| L2]) :-M is N-1,
   take2(T,M, L1,L2).



discoverPlaylist(LikedGenres,DislikedGenres,Features,FileName,Playlist):-
                                                                getLike([],LikedGenres,Son),
                                                                getDislike([],DislikedGenres,Result,Son),
                                                                subtract(Son,Result,Y),
                                                                make_list4(Y,Features,S),
                                                                sort(S,T),
                                                                take2(T,30,Playlist,Distances),
                                                                match(Playlist,Names),
                                                                getArtists(Playlist,Artists),
                                                                open(FileName,write,Stream),
                                                                write(Stream,Playlist), nl(Stream),
                                                                write(Stream,Names), nl(Stream),
                                                                write(Stream,Artists), nl(Stream),
                                                                write(Stream,Distances), nl(Stream),
                                                                close(Stream).
                                                                



