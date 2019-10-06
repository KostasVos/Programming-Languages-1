import java.util.*;
import java.io.*;

class Graph{

    private int ver,row,col,cat_position;
    private int flood_times[][];
    private int cat_times[][];

    Graph(int V,int ROW,int COL, char[][] input){
        ver=V;
        row=ROW;
        col=COL;

        flood_times = new int[row][col];
        cat_times= new int[row][col];

        for(int i=0;i<row;i++){
            for(int j=0;j<col;j++){

                if(input[i][j]=='X'){
                   flood_times[i][j]=-2;
                   cat_times[i][j]=-2;
                   continue;
                }
                
                cat_times[i][j]=-1;

                if(input[i][j]=='W') flood_times[i][j] = 0;
                else flood_times[i][j] = -1; 
                
                if(input[i][j]=='A')
                    cat_position = i*col+j;                
            }
        }
    }

    int getValue(int [][] arr,int i){
        return arr[i/col][i%col];
    }



    void printArray(int arr[][]){
        for(int i=0;i<row;i++){
            for(int j=0;j<col;j++){
                System.out.print(arr[i][j]+" ");
            }
            System.out.println();
        }
    }
    void FloodFill(){

        //printArray(flood_times);

        LinkedList<Integer> queue = new LinkedList<Integer>();

        for(int v=0; v<ver;v++){
            if(getValue(flood_times, v)==0)
                queue.add(v);
        }
        LinkedList<Integer> temp = new LinkedList<Integer>();



        while(!queue.isEmpty()){
            int v = queue.pop();
            int i = v/col;
            int j = v%col;
            temp.clear();
            

            if(i<row-1)
                    if(flood_times[i+1][j] != -2)
                        temp.add(col*(i+1)+j);
                if(j>0)
                    if(flood_times[i][j-1] != -2)
                          temp.add(col*i+j-1);
                if(j<col-1)
                    if(flood_times[i][j+1] != -2)
                          temp.add(col*i+j+1);
                if(i>0)
                    if(flood_times[i-1][j] != -2)
                          temp.add(col*(i-1)+j); 


            Iterator<Integer> iter = temp.listIterator();
            while (iter.hasNext()){
                int val=iter.next();
                if(getValue(flood_times, val)==-1){
                    flood_times[val/col][val%col] = flood_times[v/col][v%col]+1; 
                    queue.add(val);
                }
            }
        }
        //System.out.println("Flood array after Flood fill : ");
        //printArray(flood_times);
    }

    //Implementation based on https://www.geeksforgeeks.org/breadth-first-search-or-bfs-for-a-graph/
    void BFS(){
        int max=-2, pos=cat_position, safe_time, left=col+1, up = row+1;
        int moves[] = new int[row*col];

        for(int i=0;i<ver;i++)
            moves[i]=-1;

        int s = cat_position;
        cat_times[s/col][s%col]=0;
        LinkedList<Integer> queue = new LinkedList<Integer>();

        moves[s] = 0;
        queue.add(s);
        LinkedList<Integer> temp = new LinkedList<Integer>();


        while(queue.size() != 0){
            s = queue.pop();
            int i = s/col;
            int j = s%col;
            temp.clear();

            if(i<row-1)
                    if(flood_times[i+1][j] != -2)
                        temp.add(col*(i+1)+j);
                if(j>0)
                    if(flood_times[i][j-1] != -2)
                          temp.add(col*i+j-1);
                if(j<col-1)
                    if(flood_times[i][j+1] != -2)
                          temp.add(col*i+j+1);
                if(i>0)
                    if(flood_times[i-1][j] != -2)
                          temp.add(col*(i-1)+j); 
            
            safe_time = flood_times[i][j]-1;

            if(safe_time > max){
                max = safe_time;
                left = j;
                up = i;
                pos = s;
            }


            if(safe_time == max){
                if(i < up){
                    left = j;
                    up = i;
                    pos = s;
                }else if(i==up && j < left){
                    left = j;
                    up = i;
                    pos = s;
                }
            }
            Iterator<Integer> iter = temp.listIterator();
            while (iter.hasNext()){
                int n = iter.next();
                if((moves[n]<0) && ((flood_times[n/col][n%col] > (cat_times[i][j] + 1)) || (flood_times[n/col][n%col]==-1))){
                    cat_times[n/col][n%col]=cat_times[i][j] + 1;
                    queue.add(n);
                    moves[n]=s;
                }
            }
        }
        flood_times=null;
        cat_times=null;
        
        printResults(max,pos,moves);
    }

    void printResults(int max, int pos, int []moves){
        String toPrint="";

        if(max==-2)
            toPrint += "infinity\n";
        else
            toPrint += max+"\n";
        
        if(pos == cat_position){
            toPrint +="stay";
            System.out.println(toPrint);
            return;
        }

        int prev=cat_position,current;
        LinkedList<Integer> temp = new LinkedList<Integer>();
        
        for(int j=pos; j!= cat_position; j=moves[j])
            temp.add(0,j);
        
        Iterator<Integer> i = temp.listIterator();
        while(i.hasNext()){
            current = i.next();
            if(current == prev-1) toPrint += "L";
            else if(current == prev+1) toPrint += "R";
            else if(current == prev-col) toPrint += "U";
            else if(current == prev+col) toPrint += "D";
            prev=current;
        }
        System.out.println(toPrint);
    }
}


public class savethecat2{
    public static void main(String[] args)throws Exception
    {
        File file = new File(args[0]);

        BufferedReader br = new BufferedReader(new FileReader(file));

        String st;
        int col=0,row=0;
        LinkedList<String> temp = new LinkedList<String>();
        while((st = br.readLine()) != null){
            row++;
            col = st.length();
            temp.add(st);
        }

        char input[][] = new char[row][col];

        Iterator<String> i = temp.listIterator();
        int counter=0;
        while(i.hasNext()){
            st = i.next();
            input[counter++]=st.toCharArray();
        }

        Graph g = new Graph(row*col, row, col, input);
        input=null;

        Runtime runtime = Runtime.getRuntime();
        // Run the garbage collector
        runtime.gc();
        g.FloodFill();
        g.BFS();
    }
}        
