%The ztalloc predicate below

ztalloc(File,Out)-
    read_input(File,Start,Goal),
    main(Start,Goal,Out).

%All reading predicates below

read_input(File, Start, Goal)-
    open(File, read, Stream),
    read_line(Stream, N),
    read_line_int(Stream,Start,Goal,N),!.

read_line(Stream, L) -
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

%read_line_int(Stream,TicketList,N) It is used for reading the N next lines
%in a txt file input.

read_line_int(_,[],[],0).
read_line_int(Stream,[[StartL,StartH]StartT],[[GoalL,GoalH]GoalT],Count)-
    Count0,
    read_line(Stream,[StartL,StartH,GoalL,GoalH]), %reading starting states and goals
    Count1 is Count-1,
    read_line_int(Stream,StartT,GoalT,Count1).


%The main predicate below


%main(StartingStates,Goals,Outputs) It uses bfs5 and tnh5 predicates
%to produce the shortest path for each StartingState-Goal couple.
main([],[],[]).
main([StartHStartT],[GoalHGoalT],[OutHOutT])-
    empty_assoc(Seen),                      %creating an empty assoc_list for Seen
    bfs(GoalH,[[StartH,[]]],Seen,Path,Imp),
    is_empty(Path,Empty),                   %Decides if program==empty or not
    tnh(Path,'',OutH,Empty,Imp),            %Produces the final sequence (thhtth...)
    main(StartT,GoalT,OutT).

%The bfs predicate below

%bfs(Goal,Queue,Seen,Path,Imp) This predicate implements a simple bfs solver.
%We use Queue to store the next valid states to be visited. We pop the head of 
%Queue each time and add an element at the end of Queue. Each element of the queue
%is a list that also contains 2 lists. The first list is our state([Low,High]) and
%the second list is a list of 1s and 2s. 1s correspond to dividing a state by 2 and
%2s correspond to applying 3n + 1 to the state. Seen is implemented 
%using assoc_list and it is used to store all the seen valid states we've gone
%through. Path is the path that we follow in order to start from the Starting State
%and reach the goal. Once our current state is goal the path is instantiated.
%Imp = 1 if Queue = [] which means it is impossible to reach the goal. Else 
%Imp = 0.
bfs(_,[],_,_,1).                        %case in which Queue becomes [] (Imp = 1) so it's 'IMPOSSIBLE'
bfs(Goal,[[State,Parents]RestQueue],Seen,Path,Imp)-
    (not(is_goal(State,Goal)) -        %case in which the State is not the goal
    (next_state(State,NextState1,[NextState2Low,NextState2High]), %producing the next state
    (get_assoc(NextState1,Seen,_) - Flag1 = 0      
     ;Flag1 = 1),
    ((NextState2High = 1000000;get_assoc([NextState2Low,NextState2High],Seen,_)) - Flag2 = 0
     ;Flag2 = 1),
    ((Flag1 = 1, Flag2 = 1) - (append(RestQueue,[[NextState1,[1Parents]],[[NextState2Low,NextState2High],[2Parents]]],QueueOut),                               
      put_assoc(NextState1,Seen,0,Seen2),
      put_assoc([NextState2Low,NextState2High],Seen2,0,NewSeen),
      bfs(Goal,QueueOut,NewSeen,Path,Imp));
      (Flag1 = 0, Flag2 = 0) - bfs(Goal,RestQueue,Seen,Path,Imp);
      (Flag1 = 0, Flag2 = 1) - (append(RestQueue,[[[NextState2Low,NextState2High],[2Parents]]],QueueOut),
                                 put_assoc([NextState2Low,NextState2High],Seen,0,NewSeen),
                                 bfs(Goal,QueueOut,NewSeen,Path,Imp));
      (Flag1 = 1, Flag2 = 0) - (append(RestQueue,[[NextState1,[1Parents]]],QueueOut),
                                 put_assoc(NextState1,Seen,0,NewSeen),
                                 bfs(Goal,QueueOut,NewSeen,Path,Imp))))
    ;reverse(Parents,Path), Imp = 0).   %case in which the State is goal.
%Note we use Flag1 to tell if NextState1 is 'legal' or 'illegal'.If Flag1 = 0
%(NextState1 already exists in Seen) then we dont add NextState1 to the queue.
%The same continues for Flag2 and NextState2=[NextState2Low,NextState2High].


is_goal([Low,High],[GoalLow,GoalHigh])-
    GoalLow = Low,
    GoalHigh = High.

next_state([PrevLow,PrevHigh],[NextLow1,NextHigh1],[NextLow2,NextHigh2])-
    NextLow1 is PrevLow div 2,
    NextHigh1 is PrevHigh div 2,
    NextLow2 is 3PrevLow + 1,
    NextHigh2 is 3PrevHigh + 1.

is_empty([],1).
is_empty([__],0).


%tnh(Path,CurrentProgram,OutputProgram,Empty,Impossible)
%It is used to generate the sequence of t and h. It's 'IMPOSSIBLE'
%if Impossible = 1, 'EMPTY' if Empty = 1.
tnh(_,_,'IMPOSSIBLE',0,1).
tnh(_,_,'EMPTY',1,0).
tnh([],X,X,0,0).
tnh([HT],CurrProg,OutProg,0,0)-
    (H = 1 - atom_concat(CurrProg,h,NewProg);
     H = 2 - atom_concat(CurrProg,t,NewProg)),
    tnh(T,NewProg,OutProg,0,0).        