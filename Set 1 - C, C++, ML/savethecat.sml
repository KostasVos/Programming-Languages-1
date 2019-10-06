(*thn kaleis arxika me orismata:
  ctimes = int pinakas idiwn diastasewn me arin
  ftimes = int pinakas idiwn diastasewn me arin
  i = 0 (metrhths seiras)
  j = 0 (metrhths sthlhs)
*)
fun printArrayRow arr i j col =
  if (j<col) then
  (
    
    print(Array2.sub(arr,i,j)^" ");

    printArrayRow arr i (j+1) col
  )
  else(print("\n"));
  

fun printArray arr i row col=
  if(i<row) then
  (
    printArrayRow arr i 0 col;
    printArray arr (i+1) row col
  )
  else();


fun initialize arin ctimes ftimes i j =
   if i = Array2.nRows(arin) then (ctimes,ftimes)
   else 
   (let
      val elin = Array2.sub(arin,i,j)
      val (rowin,colin) = Array2.dimensions(arin)
    in
      (Array2.update(ctimes, i, j, (if elin = #"A" then 0
                            else if elin = #"W" then ~1
                            else if elin = #"." then ~1
                            else ~2));
       Array2.update(ftimes, i, j, (if elin = #"A" then ~1
                            else if elin = #"W" then 0
                            else if elin = #"." then ~1
                            else ~2));
       if j = (colin - 1) then initialize arin ctimes ftimes (i+1) 0
       else initialize arin ctimes ftimes i (j+1))
    end);


(*pairnei ena kommati mias megalhs listas kai to kanei append se mia mikroterh mexri na synanthsei #"\n". h mikroterh lista
tha periexei sto telos to katharo kommati ths megalhs*)

fun divide listbig listsmall = 
   if ((hd listbig) = #"\n") then (tl listbig, rev(listsmall))
   else if ((tl listbig) = nil) then (tl listbig, rev((hd listbig)::listsmall))
   else divide (tl listbig) ((hd listbig)::listsmall)

(*pairnei san input tin list a toy parse kai xrhsimopoiei thn 
divide gia na kataskeyasei thn listoflists*)

fun maker listinput listoflists =
  let
    val (list1d,listrow) = divide listinput []
  in
    if list1d = nil then rev(listrow::listoflists)
    else maker list1d (listrow::listoflists)
  end;




(*gets all the contents from a text file and saves then in a
string a, then "explode" creates a list called lista, the 
elements in lista are the characters of the string in the same
order. got it from: "https://stackoverflow.com/questions/43923839/convert-char-list-array-to-char-array-array-in-sml"*)

fun parse file =
        let
            fun next_String input = (TextIO.inputAll input) 
            val stream = TextIO.openIn file
            val a = next_String stream
            val lista = explode(a)
        in
            Array2.fromList(maker lista [])
        end



fun isSquare row col i j=
  ((i<row) andalso (i>=0) andalso (j<col) andalso (j>=0));

fun createQueueFromRow arr q i j col =
  if (j<col) then
  (
    if (Array2.sub(arr,i,j)=0) then Queue.enqueue(q,i*col+j)
    else();

    createQueueFromRow arr q i (j+1) col
  )
  else();
  

fun createQueue arr q i row col=
  if(i<row) then
  (
    createQueueFromRow arr q i 0 col;
    createQueue arr q (i+1) row col
  )
  else();

fun floodFill flood_times node_queue row col=
    if(Queue.isEmpty(node_queue)=false) then
    let
        val node = Queue.dequeue(node_queue)
        val i = node div col;
        val j = node mod col;
        val current = Array2.sub(flood_times,i,j)
        val down = (if (isSquare row col (i+1) j) then Array2.sub(flood_times,(i+1),j) else ~3)
        val left = (if (isSquare row col i (j-1)) then Array2.sub(flood_times,i,(j-1)) else ~3)
        val right = (if (isSquare row col i (j+1)) then Array2.sub(flood_times,i,(j+1)) else ~3)
        val up = (if (isSquare row col (i-1) j) then Array2.sub(flood_times,(i-1),j) else ~3)
    in
    (
        
        if (down = ~1) then(
          Queue.enqueue(node_queue, ((i+1)*col + j));
          Array2.update(flood_times,(i+1),j,(current+1))
        )else();
        if (left = ~1) then(
          Queue.enqueue(node_queue, (i*col + (j-1)));
          Array2.update(flood_times,i,(j-1),(current+1))
        )else();
        if (right = ~1) then(
          Queue.enqueue(node_queue, (i*col + (j+1)));
          Array2.update(flood_times,i,(j+1),(current+1))
        )else();
        if (up = ~1) then(
          Queue.enqueue(node_queue, ((i-1)*col + j));
          Array2.update(flood_times,(i-1),j,(current+1))
        )else();

        floodFill flood_times node_queue row col
    )
    end
    else ();

fun update_visited flood_times cat_times moves visited node_queue row col i j pos next_t=(
  if ((Array2.sub(visited,i,j) = (~1)) andalso (((Array2.sub(flood_times,i,j)) > next_t) orelse (Array2.sub(flood_times,i,j)=(~1)))) then
    (
          Queue.enqueue(node_queue,i*col + j);
          Array2.update(cat_times, i,j,next_t);
          Array.update(moves, i*col + j, pos)
    )
    else()
);

fun BFS flood_times cat_times moves visited node_queue row col max pos =
  if(Queue.isEmpty(node_queue)=false) then
    let
      val node = Queue.dequeue(node_queue)
      val i = node div col;
      val j = node mod col;
      val next_t = (Array2.sub(cat_times,i,j)+1)
      val safe = (Array2.sub(flood_times,i,j)-1)
      val newmax = (if (safe>max) then safe else max)
      val newpos = (if (safe>max) then node 
                    else if (safe = max) then (
                      if (i < (pos div col)) then node
                      else if((i = (pos div col)) andalso (j < (pos mod col))) then node
                      else pos
                    )
                    else pos)
    in
      Array2.update(visited,i,j,1);
      if (isSquare row col (i-1) j) then
        update_visited flood_times cat_times moves visited node_queue row col (i-1) j node next_t
      else();

      if (isSquare row col i (j+1)) then
        update_visited flood_times cat_times moves visited node_queue row col i (j+1) node next_t
      else();

      if (isSquare row col i (j-1)) then
        update_visited flood_times cat_times moves visited node_queue row col i (j-1) node next_t
      else();
      if (isSquare row col (i+1) j) then
        update_visited flood_times cat_times moves visited node_queue row col (i+1) j node next_t
      else();

      

      


      

      BFS flood_times cat_times moves visited node_queue row col newmax newpos
    end

  else 
    (max,pos);

fun createList moves pos initial alist=
(
  if pos = initial then alist
  else
    let
      val newElem = Array.sub(moves,pos)
      val newlist = (newElem)::alist
    in
      createList moves newElem initial newlist
    end
)

fun identifyMove prev current col =
  if current = (prev-1) then "L"
  else if current = (prev+1) then "R"
  else if current = (prev - col) then "U"
  else if current = (prev + col) then "D"
  else " ";

fun printMoves printlist col current prev=
  if printlist = nil then print("\n")
  else
    let
      val current = (hd printlist)
      val toPrint= identifyMove prev current col
    in
    (
      print(toPrint);
      printMoves (tl printlist) col 0 current
    )
    end;

fun printResults moves pos initial max col=(
  if max = (~2) then print("infinity")
  else print(Int.toString(max));

  print("\n");
  if pos = initial then print("stay\n")
  else 
    let
      val alist = createList moves pos initial [pos]
    in
      printMoves  (tl alist) col 0 initial
    end
)


fun savethecat filename=
  let
    val input = parse(filename)
    val (row,col) = Array2.dimensions(input)
    val cat_times = Array2.array(row,col,0)
    val flood_times = Array2.array(row,col,0)
    val (cat_times,flood_times) = initialize input cat_times flood_times 0 0   
    val moves = Array.array(row*col,~1)
    val visited = Array2.array(row,col,~1)
    val q = Queue.mkQueue() : int Queue.queue
    val catq = Queue.mkQueue() : int Queue.queue
    val newq = createQueue flood_times q 0 row col
    val asdf = createQueue cat_times catq 0 row col
    val s = Queue.head(catq)
    val asdasd = floodFill flood_times q row col
    val (max, pos) = BFS flood_times cat_times moves visited catq row col (~2) s
    val alist = createList moves pos s []
  in
    printResults moves pos s max col
  end;                