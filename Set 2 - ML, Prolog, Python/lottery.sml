(*
    Lottery in Standard ML
    Vosinas Konstantinos
    Andriopoulos Konstantinos
*)

(*Function to read input data, returns K,N,Q and two lists of strings, Tickets. Winning tickets
Order does not matter in tickets, but matters in winning, so that results are presented in order
Winning tickets list is reversed
*)
fun readlist (file : string) = 
    let
        val inStream = TextIO.openIn file
        fun readInt input = 
            Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
        val K = readInt inStream
        val N = readInt inStream
        val Q = readInt inStream
        val dummy = TextIO.inputLine inStream
        fun readNums 0 acc = acc 
            | readNums i acc = readNums (i-1) ((Option.valOf(TextIO.inputLine inStream))::acc)
    in
      (K,N,Q, readNums N [], rev (readNums Q []))
end


(*Reverses all strings in given list and removes \n from the front
Example : ["1234\n","5678\n"] ==> ["4321","5678"]*)
fun reverseElements [] = []
    |reverseElements (hd::tl) = (String.extract(((implode o rev o explode) hd),1,NONE)):: (reverseElements tl)

(*Takes a string list as input and returns a list of list of chars, based on each string
*)
fun listify [] = []
    |listify (hd::tl) = (explode hd)::(listify tl)

(*The structure used in this solution is a trie
in this implementation, we use 'a as a bool variable to signify an end of a word*)
datatype 'a trie = empty
    | Node of 'a * 'a trie * 'a trie * 'a trie * 'a trie * 'a trie * 'a trie * 'a trie * 'a trie * 'a trie * 'a trie;

(*Inserts a ticket to the trie, returns new trie. Before being inserted each ticket has been reversed, so the first node is 
its last digit, the second node its second to last etc. Each node has an integer value which counts the number of leaves below
that node. When adding a new number to a pre-existing node, this value is increased by one. Leaves have a value of 1.
*)
fun insert([],empty) = Node(1,empty,empty,empty,empty,empty,empty,empty,empty,empty,empty)
    |insert (hd::tl, empty) = insert(hd::tl,Node(0,empty,empty,empty,empty,empty,empty,empty,empty,empty,empty))
    |insert(hd::tl, Node(num,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9)) = 
        case hd of 
            #"0" => Node(num+1,insert(tl,n0),n1,n2,n3,n4,n5,n6,n7,n8,n9)
        |   #"1" => Node(num+1,n0,insert(tl,n1),n2,n3,n4,n5,n6,n7,n8,n9)
        |   #"2" => Node(num+1,n0,n1,insert(tl,n2),n3,n4,n5,n6,n7,n8,n9)
        |   #"3" => Node(num+1,n0,n1,n2,insert(tl,n3),n4,n5,n6,n7,n8,n9)
        |   #"4" => Node(num+1,n0,n1,n2,n3,insert(tl,n4),n5,n6,n7,n8,n9)
        |   #"5" => Node(num+1,n0,n1,n2,n3,n4,insert(tl,n5),n6,n7,n8,n9)
        |   #"6" => Node(num+1,n0,n1,n2,n3,n4,n5,insert(tl,n6),n7,n8,n9)
        |   #"7" => Node(num+1,n0,n1,n2,n3,n4,n5,n6,insert(tl,n7),n8,n9)
        |   #"8" => Node(num+1,n0,n1,n2,n3,n4,n5,n6,n7,insert(tl,n8),n9)
        |   #"9" => Node(num+1,n0,n1,n2,n3,n4,n5,n6,n7,n8,insert(tl,n9))
        |   _    => empty

(*Counts leaves of a trie recursively*)
fun countLeaves empty = 0
    | countLeaves (Node(num,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9)) = num

(*Driver function to insert tickets*)
fun insertTickets [] t1 = t1
    | insertTickets (hd::tl) t1 = insertTickets tl (insert (hd,t1))

(*Finds number of winning tickets. A ticket wins when it shares at least the last digit with the winning ticket.
In an trie implementation, this means counting all leaves from the node which corresponds to the last digit of
the winning ticket, in this case num*)
fun findWinning (num,empty) = 0
    |findWinning (num,Node(_,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9)) = 
    case num of
        #"0" => countLeaves n0
    |   #"1" => countLeaves n1
    |   #"2" => countLeaves n2
    |   #"3" => countLeaves n3
    |   #"4" => countLeaves n4    
    |   #"5" => countLeaves n5
    |   #"6" => countLeaves n6
    |   #"7" => countLeaves n7
    |   #"8" => countLeaves n8
    |   #"9" => countLeaves n9
    |   _    => 0

(*Calculates total earnings. sum(total) = sum(2^M-1) for all winning tickets = sum(2^M) - totalWinning, so this function 
only needs to calculate the value of sum(2^M) and subtract the # of winning tickets we found.

Assuming a given node n and the next digit from the winning ticket to be checked d, the total winnings from this node is

winnings = (2*sum(countLeaves(node)) for all nodes except for n(d) + 2*calculateTotal(n(d)))

Due to the size of winnings temporarily causing an overflow before applying mod 10^9+7, a temporary conversion to LargeInt is needed.

*)
fun calculateTotal (_,[]) = 2
    |calculateTotal (Node(_,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9),(hd::tl)) =
    let 
        val result = 
        if hd = #"0" then     (((countLeaves(n1) +countLeaves(n2) +countLeaves(n3) +countLeaves(n4) +countLeaves(n5) + countLeaves(n6) +countLeaves(n7) + countLeaves(n8) +countLeaves(n9))) + (if (n0=empty) then 0 else (calculateTotal(n0,tl) )))           
       else if hd = #"1" then (((countLeaves(n0) +countLeaves(n2) +countLeaves(n3) +countLeaves(n4) +countLeaves(n5) + countLeaves(n6) +countLeaves(n7) + countLeaves(n8) +countLeaves(n9))) + (if (n1=empty) then 0 else (calculateTotal(n1,tl) )))           
       else if hd = #"2" then (((countLeaves(n1) +countLeaves(n0) +countLeaves(n3) +countLeaves(n4) +countLeaves(n5) + countLeaves(n6) +countLeaves(n7) + countLeaves(n8) +countLeaves(n9))) + (if (n2=empty) then 0 else (calculateTotal(n2,tl) )))           
       else if hd = #"3" then (((countLeaves(n1) +countLeaves(n2) +countLeaves(n0) +countLeaves(n4) +countLeaves(n5) + countLeaves(n6) +countLeaves(n7) + countLeaves(n8) +countLeaves(n9))) + (if (n3=empty) then 0 else (calculateTotal(n3,tl) )))           
       else if hd = #"4" then (((countLeaves(n1) +countLeaves(n2) +countLeaves(n3) +countLeaves(n0) +countLeaves(n5) + countLeaves(n6) +countLeaves(n7) + countLeaves(n8) +countLeaves(n9))) + (if (n4=empty) then 0 else (calculateTotal(n4,tl) )))           
       else if hd = #"5" then (((countLeaves(n1) +countLeaves(n2) +countLeaves(n3) +countLeaves(n4) +countLeaves(n0) + countLeaves(n6) +countLeaves(n7) + countLeaves(n8) +countLeaves(n9)) ) + (if (n5=empty) then 0 else (calculateTotal(n5,tl) )))           
       else if hd = #"6" then (((countLeaves(n1) +countLeaves(n2) +countLeaves(n3) +countLeaves(n4) +countLeaves(n5) + countLeaves(n0) +countLeaves(n7) + countLeaves(n8) +countLeaves(n9)) ) + (if (n6=empty) then 0 else (calculateTotal(n6,tl) )))          
       else if hd = #"7" then (((countLeaves(n1) +countLeaves(n2) +countLeaves(n3) +countLeaves(n4) +countLeaves(n5) + countLeaves(n6) +countLeaves(n0) + countLeaves(n8) +countLeaves(n9)) ) + (if (n7=empty) then 0 else (calculateTotal(n7,tl) )))           
       else if hd = #"8" then (((countLeaves(n1) +countLeaves(n2) +countLeaves(n3) +countLeaves(n4) +countLeaves(n5) + countLeaves(n6) +countLeaves(n7) + countLeaves(n0) +countLeaves(n9)) ) + (if (n8=empty) then 0 else (calculateTotal(n8,tl) )))          
       else (((countLeaves(n1) +countLeaves(n2) +countLeaves(n3) +countLeaves(n4) +countLeaves(n5) + countLeaves(n6) +countLeaves(n7) + countLeaves(n8) +countLeaves(n0)) ) + (if (n9=empty) then 0 else (calculateTotal(n9,tl) )))           
       val temp = Int.toLarge(result)
       val returnVal = Int.fromLarge(2*temp mod 1000000007)
    in
        returnVal
    end

(*Driver for calculateTotal, calls the function from the node which corresponds to the winning ticket's last digit*)
fun calculateDriver (empty,_) = 0
    |calculateDriver (Node(_,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9),(hd::tl)) =
    case hd of
        #"0" => calculateTotal(n0,tl)
    |   #"1" => calculateTotal(n1,tl)
    |   #"2" => calculateTotal(n2,tl)
    |   #"3" => calculateTotal(n3,tl)
    |   #"4" => calculateTotal(n4,tl)
    |   #"5" => calculateTotal(n5,tl)
    |   #"6" => calculateTotal(n6,tl)
    |   #"7" => calculateTotal(n7,tl)
    |   #"8" => calculateTotal(n8,tl)
    |   #"9" => calculateTotal(n9,tl)
    |   _    => 0

(* Takes the list of Winning tickets and the constructed trie and finds the result for each ticket
*)
fun results [] t1 = 0
    |results (h::tl) t1=
        let
            val winning = findWinning (hd h,t1)
            val total = if winning=0 then 0 else (calculateDriver(t1,h) - winning)

        in
            print(Int.toString(winning)^" "^Int.toString(total)^"\n");
            results tl t1
        end

(*
Using the reverseElements and listify functions, we end up with two char list lists
["1234\n","4567\n"] => [[#"4",#"3",#"2",#"1"], [#"7",#"6",#"5",#"4"]]
*)
fun lottery file = 
    let
        val (K,N,Q,Tickets,Winning) = readlist file
        val Tickets = listify (reverseElements Tickets)
        val Winning = listify (reverseElements Winning)
        val tofind = hd Winning
        val t1 = empty
        val t1 = insertTickets Tickets t1
    in
        results Winning t1
    end        