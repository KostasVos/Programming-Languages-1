
          
/***************************************************************/
/*                   Programming Languages 1                   */
/*                     	    Ztalloc                 		   */
/*                                                             */                                             
/* Vosinas Konstantinos, AM: 03116435        				   */
/* Andriopoulos Konstantinos, AM: 03116023				       */
/*                                                             */
/***************************************************************/

import java.io.*;
import java.text.NumberFormat;
import java.util.*;


class ZtallocSolver{
    private int input[];
    private Map<LinkedList<Integer>,LinkedList<Integer>> previous_state;

    /* Seen list is implemented with a hash map containing a list of two elements as keys.
    The two elements are the lower bound and the higher bound of each state.
    */
    
    ZtallocSolver(int in_values[]){
        input = in_values;
        previous_state = new HashMap<LinkedList<Integer>,LinkedList<Integer> >();
    }

    void BFSolver(){

        //queue is the queue for BFS, origin contains the initial range, it is enqued to begin the BFS

        LinkedList<LinkedList<Integer>> queue = new LinkedList<>();
        LinkedList<Integer> origin = new LinkedList<>();

        origin.add(input[0]);
        origin.add(input[1]);
        queue.add(origin);
        LinkedList<Integer> current = new LinkedList<>();


        while(!queue.isEmpty()){      
            current = queue.pop();
            Iterator<Integer> it = current.listIterator();

            //Checking if the current state's range is within the output range, if yes, go to print output
            boolean flag = true;
            while(it.hasNext()){
                int value = it.next();
                if(value<input[2] || value>input[3])
                    flag=false;
            }
            if (flag == true) break;


            //Creating the next states, by applying the two rules to both the lower and the upper bound of the current state
            Iterator<Integer> iter = current.listIterator();
            flag=true;

            LinkedList<Integer> next_state1 = new LinkedList<>();
            LinkedList<Integer> next_state2 = new LinkedList<>();

            //While creating next_state 2, there is a check to ensure the resulting number is no more than 6-digits long
            while(iter.hasNext()){
                int value = iter.next();
                next_state1.add(value/2);
                int next_t = 3*value+1;
                if(next_t>999999) flag=false;
                if(flag==true)
                    next_state2.add(next_t);

            }

            //If each new state has not been visited, enqueue it and save the previous state
            if(!previous_state.containsKey(next_state1)){
                previous_state.put(next_state1,current);
                queue.add(next_state1);
            }
            if(flag == true && !previous_state.containsKey(next_state2)){
                previous_state.put(next_state2,current);
                queue.add(next_state2);
            }
        }
        printResults(current,origin,queue.size());
    }

    void printResults(LinkedList<Integer> current, LinkedList<Integer> origin,int queue_remaining){
        if(current==origin) System.out.println("EMPTY");
        else{
            if(queue_remaining==0){
                 System.out.println("IMPOSSIBLE");
                 return;
            }
            LinkedList<LinkedList<Integer>> temp = new LinkedList<>();

            //Similar to save the cat, trace back through previous_state and print the path in reverse
            for(LinkedList<Integer> j=current; j!= origin; j=previous_state.get(j))
                temp.add(0,j);
            String toprint = "";

            int prev = input[0];
            while(!temp.isEmpty()){
                LinkedList<Integer> t = temp.pop();
                int a0 = t.pop();

                if(a0 == prev/2)
                    toprint += "h";
                else
                    toprint += "t";
                prev = a0;
      
                    
            }
                System.out.println(toprint);
        }
    }

}

public class ztalloc{
    public static void main(String[] args)throws Exception
    {
        File file = new File(args[0]);

        BufferedReader buffer = new BufferedReader(new FileReader(file));
        String line="";
        buffer.readLine();

        while((line = buffer.readLine()) != null){
            String[] nums = line.split(" ");
            int input [] = new int[4];
            for(int i=0;i<4;++i)
                input[i]=Integer.parseInt(nums[i]);
            
            ZtallocSolver zt1 = new ZtallocSolver(input);
            zt1.BFSolver();
            System.gc();
        }
    }
}        