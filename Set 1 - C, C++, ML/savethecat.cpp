
          
/***************************************************************/
/*                   Programming Languages 1                   */
/*                     	 Save the Cat                 		   */
/*                                                             */                                             
/* Vosinas Konstantinos, AM: 03116435        				   */
/* Andriopoulos Konstantinos, AM: 03116023				       */
/*                                                             */
/***************************************************************/


#include <iostream>
#include <list>
#include <fstream>
using namespace std;

/************************Graph Class*****************************/

//Graph implemented with adjacency lists, includes all methods used
//Data : row,col are dimensions of grid
//flood_times : Array representing the time at which a square is flooded
//cat_times : Array representing the time at which a square can be reached by the cat
class Graph{
    
    int ver,row,col,cat_position;
    int **flood_times;
    int **cat_times;
    
    list<int> *adj;
    public:
        Graph(int row_in, int col_in);
        void addEdge(int ver, int w);
        void createGraph(string *input);
        void BFS();
        void floodFillMod();
        void print_results(int max, int pos, int *moves);

};

//Initializing variables used in other methods
Graph::Graph(int row_in, int col_in){
    this->ver = row_in*col_in;
    this->row = row_in;
    this->col = col_in;
    adj = new list<int>[ver];
}

//Creates Graph from input
//An edge is added only if a node is reachable, thus no edges towards X's
void Graph::createGraph(string *input){
    //First creating adjecency lists
    for(int i=0;i<row;i++){

        for(int j=0;j<col;j++){
            //In case of a wall, no edge is added
            if(input[i][j] == 'X')
                continue;
            //Finding initial position of cat in the process
            if(input[i][j] == 'A') this->cat_position = i*col + j;

            //Nodes are added to the list in dictionary order
            //Down
            if(i<row-1)
                if(input[i+1][j] != 'X')
                    this->addEdge(col*i+j,col*(i+1)+j);
            //Left
            if(j>0)
                if(input[i][j-1] != 'X')
                    this->addEdge(col*i+j,col*i+(j-1));
            //Right
            if(j<col-1)
                if(input[i][j+1] != 'X')
                    this->addEdge(col*i+j,col*i+(j+1));
            //Up
            if(i>0)
                if(input[i-1][j] != 'X')
                    this->addEdge(col*i+j,col*(i-1)+j);	
            
        }
    }
    
    //Creating and initializing flood_times & cat_times
    flood_times = new int*[row];
    cat_times = new int*[row];

    for(int i=0; i<row; i++){
        flood_times[i] = new int[col];
        cat_times[i] = new int[col];
        for(int j=0; j<col; j++){ 
            //Walls are set to -2 in both cases
            if(input[i][j] == 'X'){
                flood_times[i][j] = -2;
                cat_times[i][j] = -2;
            }else{
            //For the cat, all other (reachable) positions initialized to -1
                cat_times[i][j] = -1;
            //For the flood_times, sources initialized to 0, all other to -1
                if(input[i][j] == 'W')
                    flood_times[i][j] = 0;
                else	
                    flood_times[i][j] = -1;
            }
        }
    }
}
//Creates an edge v --> w
void Graph::addEdge(int v, int w){
    adj[v].push_back(w);
}

//Flood fill method, sets values on the flood_times array
void Graph::floodFillMod() {    
    
    
    // Create a queue for flood fill
    list<int> queue;
    list<int>::iterator it; 
    int v;

    // Mark the seed nodes as visited and enqueue
    for(v=0; v<ver; v++)
        if(flood_times[v/col][v%col] == 0)
            queue.push_back(v);
    
    while(!queue.empty()) 
    { 
        // Dequeue a vertex from queue
        v = queue.front(); 
        queue.pop_front(); 
        //Enque adjacent vertices which have not been visited (flood_time == -1)
        //Set the corresponding time as onw more than current

        for (it = adj[v].begin(); it != adj[v].end(); ++it) {
         	if(flood_times[*it/col][*it%col] == -1){
                flood_times[*it/col][*it%col] = flood_times[v/col][v%col] + 1;
                queue.push_back(*it); 
            }
        } 
    } 
}
//BFS algorithm, calculates required sequence of moves
void Graph::BFS() 
{ 
    
    int max = -2;			//maximum amount of time
    int pos = cat_position;	//final position of cat

 	int safe_time;

    int s = cat_position;	//Setting s as the initial cat position
    //Moves array: for each square on the grid, represents the square from which it was reached
    int *moves = new int[row*col];

    //Left and right position on the grid, used to determine square with lowest coordinates
    int left = col+1, up = row+1;

    bool *visited = new bool[row*col];
    // Mark all the vertices as not visited 
    for(int i = 0; i < ver; i++) 
        visited[i] = false;

    // Create a queue for BFS 
    list<int> queue; 
    
  
    // Mark the initial position as visited and enqueue it, with time = 0
    queue.push_back(s); 
    cat_times[s/col][s%col] = 0;  
    visited[s] = true;

    list<int>::iterator i;

    while(!queue.empty()) 
    { 
        //Pop an item from queue
        s = queue.front();
        queue.pop_front(); 
        //Calculate safe time for cat in corresponfing square
  		safe_time = flood_times[s/col][s%col]-1;
        
        //If it's greater than current max, update max,pos.left,right
        if(safe_time > max){
            max = safe_time;
            left = s%col;
            up = s/col;
            pos = s;
        }
        
        //In case of a square with same max time (or in the 'infinity case max = -2 for all)
        //Check if the new square's coordinates are lexicographically lower and update
        if(safe_time==max){
            if(s/col < up){
            	left = s%col;
            	up = s/col;
            	pos=s;
        	}else if(s/col == up && s%col < left){
            	left = s%col;
            	up = s/col;
            	pos=s;
            }
        }
        
        //For all adjacent nodes, if on the next time momment, the cat is safe on that node
        //Aka, flood_times>cat_times+1, update cat_times, enqueue node
        //and keep the node from where it was reached
        //Also, in the case of flood_times == -1, or 'infinity', enqueue the nodes anyway
        //In that case, the condition safe_time==max finds the correct node
        for (i = adj[s].begin(); i != adj[s].end(); ++i) 
        { 
            if ((!visited[*i]) && ((flood_times[*i/col][*i%col]>(cat_times[s/col][s%col]+1)) || (flood_times[*i/col][*i%col]==-1)))
            { 
                cat_times[*i/col][*i%col]=cat_times[s/col][s%col]+1;
                queue.push_back(*i); 
                moves[*i] = s;
                visited[*i]=true;
            } 
        } 
    }
    //Print the results
    this->print_results(max,pos,moves);
} 

void Graph::print_results(int max, int pos, int *moves){
    //If max hasn't changed, the cat can't be reached by the water
    if(max == -2)
        cout << "infinity" << endl;
    else
        cout << max << endl;
    
    //If the final position hasn't changed, cat should 'stay'
    if(pos == cat_position){
        cout << "stay"<<endl;
        return;
    }
    //Printing the results
    //Using a stack (temp) inside which we initially 
    //push the nodes based on the moves array, to access in reverse order

    int prev=cat_position, current;
    list<int> temp;
    list<int>::iterator i;
    for(int j=pos; j!=cat_position; j=moves[j])
        temp.push_front(j);
    
    //Dependeing on the previous and current node, we can deduce the type of movement
   	for (i= temp.begin();i!=temp.end();++i){
        current = *i;
        if(current == prev-1) cout << "L";
        else if(current == prev+1) cout << "R";
        else if(current == prev-col) cout << "U";
        else if(current == prev+col) cout << "D";
        prev = current;
    }
    cout << endl;
}

//Main Function : Creates an array from the input file and calls the corresponding methods
int main(int argc, char *argv[]){

    string line;
    ifstream myfile(argv[1]);
    list<string> queue_temp; 

    int row=0,col=0;
    if (myfile.is_open()){
        while (getline (myfile,line) ){
            if(line.size() == 0) break;
            queue_temp.push_back(line);
            col = line.size();
            row++;
        }
        myfile.close();
    }

    string *input;
    input = new string[row];
    
    for(int i=0; i<row; i++){
        input[i] = queue_temp.front();
        queue_temp.pop_front();
    }
    
    Graph g(row,col);
    g.createGraph(input);
    g.floodFillMod();
    g.BFS();

    return 0;
}        