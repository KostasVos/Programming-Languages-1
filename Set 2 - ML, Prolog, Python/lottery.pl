%main lottery predicate

lottery(File,L):-
    read_input(File,_,_,_,Tickets,LuckyNumbers),
    Trie = node(0,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil),
    insertTickets(Tickets,Trie),
    main(Trie,LuckyNumbers,L).


%All the input reading predicates below


read_input(File, K, N, Q, Tickets, LuckyNumbers):-
    open(File, read, Stream),
    read_line(Stream, [K, N, Q]),
    read_line_int(Stream,Tickets,N),
    read_line_int(Stream,LuckyNumbers,Q),!.

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

%read_line_int(Stream,TicketList,N): Is used for reading the N next lines
%in a txt file input. It stores each ticket that is read from a line in 
%the TicketList list. Each element of the TicketList is a reversed ticket
%stored as a list of digits. 

read_line_int(_,[],0).
read_line_int(Stream,[H|T],Count):-
    Count>0,
    read_line_to_codes(Stream,H1),
    maplist(plus(48),H2,H1), %converts codes to actual digit values by subtracting 48
    reverseElements(H2,H,[]),%reversing the list of digits
    Count1 is Count-1,
    read_line_int(Stream,T,Count1).


%List-reversing predicate below


%reverseElements(List,ReversedList,Accumulator): Reverses a list of ints
%using an Accumulator so that the reversing complexity is O(N)
reverseElements([],Y,Y).
reverseElements([H|T],Y,Acc):-
    reverseElements(T,Y,[H|Acc]).


%All trie-inserting predicates below


%insertTickets(ListOfTickets,Trie): Inserts a list of tickets (list of digit lists) 
%into the Trie database by applying the 
%insert predicate on every ticket of the ListOfTickets.

insertTickets([],_).
insertTickets([H|T],Trie):-
    insert(H,Trie),
    insertTickets(T,Trie).

%insert(Ticket,TrieIn,TrieOut) Inserts a ticket into the Trie Database and returns
%the corresponding trie output.
%Each node holds a counter for all the tickets that contain that specific digit
%in a specific position.
%For example if we insert 1234 and 1256 then the node that corresponds 
%to string '1' and '12' will have a counter of 2 and the nodes that correspond to strings:
%'123', '125', ... will have a counter of 1.

insert([],X):-                           %the case in which a leaf of the trie is reached
    X = node(0,_,_,_,_,_,_,_,_,_,_),
    setarg(1,X,1).                       %setting counter from 0 to 1

insert([H|T],Trie):-                     %general case
    Trie = node(B,_,_,_,_,_,_,_,_,_,_),
    B1 is B+1,
    setarg(1,Trie,B1),                   %Adding 1 to the current Trie Node counter
    H2 is H+2,
    arg(H2,Trie,ChildNode),              %ChildNode is the Hth child of current Trie Node
    (ChildNode \= nil ->
    insert(T,ChildNode),                 %Same process for the ChildNode
    setarg(H2,Trie,ChildNode);           %ChildNode is renewed after insertion
    ChildNode1 = node(0,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil),
    insert(T,ChildNode1),                %Same process for the ChildNode1 if ChildNode = nil
    setarg(H2,Trie,ChildNode1)).         %ChildNode1 is renewed after insertion

%The main predicate below


%main(Trie,ListOfLuckyNumbers,OutputList): Takes the Trie database and the list of
%all lucky numbers and computes the output list. Each element of the OutputList contains
%the number of winning tickets and the winning sum for a specific lucky number.

main(_,[],[]).
main(Trie,[LuckyH|LuckyT],[[WinningCount,WinningSumFinal]|OutT]):-
    calculateDriver(Trie,LuckyH,WinningCount,WinningSum),
    WinningSumFinal is WinningSum mod (10**9 + 7),
    main(Trie,LuckyT,OutT).


%All calculating predicates below


%calculateDriver(Trie,LuckyNumber,WinningCount,WinningSum): First, matches the given Trie 
%database and LuckyNumber to the number of the winning tickets (WinningCount) which is 
%actually the counter of the Hth child of the root. Then (in the general case) uses calculate
%to calculate WinningSum.

%Case in which the Hth child is nil. This means that there is no winning tickets since
%there are no tickets that have the same last digit with the lucky number. Therefore,
%WinningCount = 0 and FinalWinningSum = 0
calculateDriver(node(A,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9),[H|_],WinningCount,FinalWinningSum):-
    H2 is H + 2,
    arg(H2,node(A,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9),nil),
    WinningCount = 0,
    FinalWinningSum = 0.
%General case in which the Hth child node of the Trie root is not nil. The counter of the child (A1) is the 
%WinningCount. Since the winning amount for a single winning ticket is 2*M - 1, the FinalWinningSum
%would be equal to [(sum of all different 2^M for each different winning ticket) - WinningCount]
%The calculate predicate calculates sum of all different 2^M for each different winning ticket
%(which we refer to as WinningSum) and then we subtract WinningCount to get the FinalWinningSum.
calculateDriver(node(A,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9),[H|T],WinningCount,FinalWinningSum):-
    H2 is H + 2,
    arg(H2,node(A,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9),node(A1,N01,N11,N21,N31,N41,N51,N61,N71,N81,N91)),
    WinningCount = A1,
    calculate(node(A1,N01,N11,N21,N31,N41,N51,N61,N71,N81,N91),T,WinningCount,0,WinningSum,0),
    FinalWinningSum is WinningSum - WinningCount.


%calculate(node(...),LuckyNumberDigitList,PreviousCounter,CurrentSum,WinningSum,M):
%In this calculate predicate LuckyNumberDigitList contains all remaining digits (the number
%of which depends on how deep inside the Trie we are), PreviousCounter contains the
%counter value of the father of the current node(...), CurrentSum contains the sum of
%all 2^M so far, WinningSum is used for the computation of FinalWinningSum (in the
%calculateDriver predicate) and M is the number of current common digits between
%the lucky number and the winning tickets.
%Comment: ""I would recommend that you view the general case first and then the other
%two cases.""

%In this case, a (not nil) leaf of the Trie is reached and
%the list of Lucky Number Digits is empty. This means that there is
%one winning ticket that is the same as the lucky number. Since each ticket is unique
%there can be only one such ticket. So the counter of the current node is 1.
%M1 is actually the number of the digits of the lucky number. So in order
%to compute the WinningSum output we add the current sum (CurrSum) + 
%(number of winning tickets with M common digits)*2^M + 
%(the winning amount of the specific winning ticket we mentioned before).
calculate(node(1,_,_,_,_,_,_,_,_,_,_),[],PrevCount,CurrSum,WinningSum,M):-
    C is PrevCount - 1,
    M1 is M+1,
    WinningSum is CurrSum + C*(2**M) + (2**M1).
%In this case, M is the maximum number of commond digits since we reached a nil node
%As a result, the number of winning tickets with M common digits (with the lucky number)
%is the previous counter (PrevCount).
calculate(nil,_,PrevCount,CurrSum,WinningSum,M):-
    WinningSum is CurrSum + PrevCount*(2**M).
%This is the GENERAL CASE. New current sum (NewCurrSum) == current sum (CurrSum) +
%C*(2^M) ,where C is the number of winning tickets with M common digits with 
%the lucky number.
calculate(node(A,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9),[H|T],PrevCount,CurrSum,WinningSum,M):-
    C is PrevCount - A,                                      
    NewCurrSum is CurrSum + C*(2**M),
    NewM is M + 1,                     %We add 1 to the number of current common digits
    NewPrevCount is A,                 %We set the new current counter as the new previous counter
    H2 is H + 2,
    arg(H2,node(A,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9),ChildNode),        %get the Hth ChildNode
    calculate(ChildNode,T,NewPrevCount,NewCurrSum,WinningSum,NewM). %repeat the same process deeper into the Trie
















%testing predicates - not for evaluation

test(FileIn,FileOut):-
    lottery(FileIn,L1),
    open(FileIn, read, Stream),
    read_line(Stream,[_,_,Q]),
    close(Stream),
    open(FileOut,read,Stream1),
    read_line_int2(Stream1,L2,Q),
    equals(L1,L2,1).


read_line_int2(_,[],0).
read_line_int2(Stream,[[H1,H2]|T],Count):-
    Count>0,
    read_line(Stream,[H1,H2]),
    Count1 is Count-1,
    read_line_int2(Stream,T,Count1).  

equals([],[],_).
equals([H1|T1],[H2|T2],C):-
    H1 \= H2,
    C1 is C+1,
    equals(T1,T2,C1).
equals([H1|T1],[H1|T2],C):-
    C1 is C+1,
    equals(T1,T2,C1).        