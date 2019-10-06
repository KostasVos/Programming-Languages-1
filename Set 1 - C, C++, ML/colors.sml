(*Function reads file and returns n,k values*)
(*And a list of ints whiech represents the ribbon (initially in reverse)*)

fun parse file =
    let
     fun readInt input = 
     Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

     val inStream = TextIO.openIn file

     val n = readInt inStream
     val k = readInt inStream
     val _ = TextIO.inputLine inStream

     fun readInts 0 acc = acc
      | readInts i acc = readInts (i - 1) (readInt inStream :: acc)
    in
   	(n, k, readInts n [])
    end;


(*Shrink function is called when counter = k, when a window containing all colors is reached*)
(*Cuts elements from the back until the window does not contain all colots*)
fun shrink input hash begin last k =
  let
    val newItem = Array.sub(input, begin)-1
    val newValue = Array.sub(hash, newItem) - 1
    val newMap = Array.update(hash, newItem, newValue)
  in
    if newValue = 0 then begin
    else shrink input hash (begin+1) last k
  end;

(*findMin calculates the output value, the minimum window which contains all colors*)
(*Implementation is similar to the implementation in C, using arrays*)
(*Icreasing the window to the right until a window is reached which contains all colors*)
(*Then shrinks the window until a new minimum is reached, which is then compared to the current min*)
fun findMin n k input hash min counter begin last=
    if counter<k then 
    let
      val newLast = (last+1)
    in
      if newLast = n then min
      else
        let
          val newItem = (Array.sub(input, newLast)-1)
          val newValue = Array.sub(hash, newItem) + 1
          val newMap = Array.update(hash, newItem, newValue)
        in
        (
          if newValue = 1 then findMin n k input hash min (counter+1) begin newLast
          else findMin n k input hash min counter begin newLast
        )
        end
    end
    else 
      let
        val begin = shrink input hash begin last k
        val len = (last-begin)+1
      in
        if((len < min) orelse (min = 0)) then findMin  n k input hash len (counter-1) (begin+1) last
        else findMin n k input hash min (counter-1) (begin+1) last
      end;


(*'Main' function, initializes the arrays used and calls findMin*)
(*Begins with counter = 0, min = 0, counter = 0, last = - 1*)
fun colors filename= 
  let
    val (n,k, alist) = parse filename
    val hash = Array.array(k+1,0)
    val input = Array.fromList (rev alist)
    val answer = findMin n k input hash 0 0 0 (~1)
  in
    print(Int.toString(answer))
  end;

local
    val testsuite = [
      ("colors.in1", 4),
      ("colors.in2", 10),
      ("colors.in3", 0),
      ("colors.in8", 17),
      ("colors.in12", 147),
      ("colors.in15", 0),
      ("colors.in21", 24252),
      ("colors.in27", 421742),
      ("testIn1.txt", 39),
      ("testIn2.txt", 94),
      ("testIn3.txt", 353),
      ("testIn4.txt", 757),
      ("testIn5.txt", 2180),
      ("testIn6.txt", 33019),
      ("testIn7.txt", 0)
  ]
    fun runtests f [] = ()
      | runtests f ((name, output) :: testcases) = (
          print ("Testcase " ^ name ^ ": ");
          if f name = output then print ("OK\n")
          else                     print ("FAILED!!! "^ Int.toString(f name) ^ "\n");
          runtests f testcases
        )
in
  fun test_colors foo = runtests foo testsuite
end        