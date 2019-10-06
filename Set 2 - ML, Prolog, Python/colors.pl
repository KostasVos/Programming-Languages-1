%colors predicate


colors(File,Answer):-
    read_input(File,N,K,C),
    main(C,K,N,Answer).


%All reading reading predicates below


read_input(File, N, K, C) :-
    open(File, read, Stream),
    read_line(Stream, [N, K]),
    read_line(Stream, C).

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

%main predicate


main(Ribbon,K,N,Exit):-
    empty_assoc(EmptyDB),                               %creates an empty association list implemented with avl trees
    startTree(1,K,EmptyDB,ColorsDB),                    %initializes our Colors Counters Database (all counters = 0)
    subseq(Ribbon,Ribbon,ColorsDB,K,0,0,N,Exit).

%startTree predicate
%startTree(CurrentColor,MaximumColor,ColorsDataBase,ColorsDataBaseAfterInsertion)
startTree(K,K,ColorsDBIn,ColorsDBOut):-
    put_assoc(K,ColorsDBIn,0,ColorsDBOut).
startTree(Counter,K,ColorsDBIn,ColorsDBOut):-
    Counter < K,
    put_assoc(Counter,ColorsDBIn,0,ColorsDB),        %inserts Current Color in the association list
    Counter1 is Counter+1,                           
    startTree(Counter1,K,ColorsDB,ColorsDBOut).



%All ribbon editing related predicates below.


%subseq(RibbonToCut,RibbonToAdd,ColorsDataBase,K,ColorCounter,CurLen,CurMinLen,MinLen):-
%RibbonToCut and RibbonToAdd are copies of the original color ribbon that subseqCut and
%subseqAdd use respectively in order to cut or add colors to the desired part of the 
%ribbon. ColorDataBase is the tree in which we save the counter of each color. K is
%the number of the colors given. ColorCounter is the counter of the different colors
%in the desired part of the color ribbon. CurLen is the current length of the desired
%part of the ribbon. CurMinLen is the current minimum length of the desired part
%that contains all colors. MinLen is the minimum length output we are planning to
%compute.
%The subseq predicate repeatedly calls subseqAdd (which adds colors to the desired
%part until it reaches K different colors) and subseqCut (which cuts colors
%from the desired part) until it reaches K-1 different colors.
subseq(_,[],_,_,_,_,X,X).
subseq(RibbonToCut,RibbonToAdd,ColorsDB,K,ColorCounter,CurLen,CurMinLen,MinLen):-
    RibbonToAdd = [_|_],
    subseqAdd(RibbonToAdd,NewRibbonToAdd,ColorsDB,ColorsDB2,K,ColorCounter,NewColorCounter,CurLen,LenOut),
    subseqCut(RibbonToCut,NewRibbonToCut,ColorsDB2,ColorsOut,K,NewColorCounter,NewColorCounter2,LenOut,LenOut2),
    zeroOrN(K,NewColorCounter,LenOut2,CurMinLen,CurLen,NewCurMin),
    subseq(NewRibbonToCut,NewRibbonToAdd,ColorsOut,K,NewColorCounter2,LenOut2,NewCurMin,MinLen).

%subseqAdd(RibbonToAdd,NewRibbonToAdd,ColorsDataBase,ColorsDatabaseOut,K,ColorCounter,NewColorCounter,LenIn,LenOut):
%RibbonToAdd is a piece of the starting ribbon. In each iteration, subseqAdd adds the head of
%RibbonToAdd to the desired ribbon part until it contains K different colors. This is when 
%subseqAdd succeeds. NewRibbonToAdd is the output RibbonToAdd that remains to be used in the next
%iteration of subseq. ColorCounter is the counter of different colors in the desired ribbon part.
%NewColorCounter is the ColorCounter output after subseqAdd succeeds, LenIn is the current length
%of the desired ribbon part and LenOut is the length of the desired ribbon part after subseqAdd 
%succeeds.
subseqAdd(X,X,C,C,K,K,K,Y,Y).
subseqAdd([],[],C,C,K,L,L,Y,Y):-
    L < K.
subseqAdd([H|RestRibbonT],NewRestRib,ColorsDB,ColorsOut,K,CounterIn,NewColCount,LenIn,LenOut):-
    CounterIn \= K,
    get_assoc(H,ColorsDB,Val,NewColorsDB,NewVal),
    NewVal is Val + 1,
    (Val = 0 -> CounterInNew is CounterIn + 1;      %if a new color is added then CounterIn is incresed by 1
    CounterInNew is CounterIn),                     %else it remains the same
    NewLen is LenIn+1,
    subseqAdd(RestRibbonT,NewRestRib,NewColorsDB,ColorsOut,K,CounterInNew,NewColCount,NewLen,LenOut).


%subseqCut(RibbonToCut,NewRibbonToCut,ColorsDataBase,ColorsDataBaseOut,K,ColorCounter,NewColorCounter,LenIn,LenOut):
%RibbonToCut is a piece of the starting ribbon. In each iteration, subseqCut removes the head of
%RibbonToCut (which is also the head of the desired ribbon part) until 
%the desired ribbon part contains different colors < K. This is when subseqCut succeeds. 
%NewRibbonToCut is the output RibbonToCut that remains to be used in the next
%iteration of subseq. ColorCounter is the counter of different colors in the desired ribbon part.
%NewColorCounter is the ColorCounter output after subseqCut succeeds, LenIn is the current length
%of the desired ribbon part and LenOut is the length of the desired ribbon part after subseq succeeds.
subseqCut(X,X,C,C,K,ColorCounter,ColorCounter,Y1,Y2):-
    ColorCounter < K,
    Y2 is Y1+1.
subseqCut([H|MainRibT],NewMainRib,ColorsDB,ColorsDBOut,K,K,NewColCount,LenIn,LenOut):-
    get_assoc(H,ColorsDB,Val,NewColorsDB,NewVal),
    NewVal is Val - 1,
    (Val = 1 -> ColorCounter is K - 1;     %subtracting 1 from the color counter if a color 
    ColorCounter is K),                    %no longer exists in the desired part else ColorCounter remains the same
    NewLen is LenIn-1,                   
    subseqCut(MainRibT,NewMainRib,NewColorsDB,ColorsDBOut,K,ColorCounter,NewColCount,NewLen,LenOut).

%zeroOrN(K,ColorsCounterAfterAdd,CurrentLength,CurrentMinimumLength,PreviousLength,NewMinimumLength):
%ColorsCounterAfterAdd is the number of different colors after subseqAdd. If ColorsCounterAfterAdd
%== K,(which is the general case) then simply NewMinimumLength == min(CurrentLength, CurrentMinimumLength).

zeroOrN(K,K,X,Y,_,Z):- %ColorsCounterAfterAdd == K so as a result                       
    Z is min(X,Y).     %NewMinimumLength == min(CurrentLength, CurrentMinimumLength)

zeroOrN(K,L,_,_,0,0):- %ColorsCounterAfterAdd < K and also PreviousLength == 0
    L < K.             %so as a result NewMinimumLength == 0. This is the case in which subseqAdd counldn't
                       %find any desired ribbon parts because the whole starting ribbon contains
                       %less than K different colors.

zeroOrN(K,L,X,Y,C,Z):- %ColorsCounterAfterAdd < K but this time PreviousLength > 0 so as a result
    L < K,             %NewMinimumLength == min(CurrentLength, CurrentMinimumLength)
    C \= 0,
    Z is min(X,Y).        