class Graph(object):
    def __init__(self, row,col):
        self.row = row
        self.col = col
    
    def create_graph(self,input):
        self.flood_times = [[0 for i in range(self.col)] for j in range(self.row)]
        self.cat_times = [[0 for i in range(self.col)] for j in range(self.row)]

            
        for i in range(self.row):
            for j in range(self.col):
                if input[i][0][j] == 'A':
                    self.cat_position = i*self.col+j
                if input[i][0][j] == 'X':
                    self.flood_times[i][j]=-2
                    self.cat_times[i][j]=-2
                else:
                    self.cat_times[i][j]=-1
                    if input[i][0][j] == 'W':
                        self.flood_times[i][j]=0
                    else:
                        self.flood_times[i][j]=-1 
            
    
    def print_results(self,max,final_pos,moves):
        if max == -2:
            print("infinity")
        else:
            print(max)
        
        if final_pos==self.cat_position:
            print("stay")
            return
        
        prev=self.cat_position
        j=final_pos
        temp=[]
        while j!=self.cat_position:
            temp = [j]+temp
            j=moves[j]
        
        for i in temp:
            current=i
            if current==(prev-1):
                print("L",end='')
            elif current==(prev+1):
                print("R",end='')
            elif current==(prev-self.col):
                print("U",end='')
            elif current==(prev+self.col):
                print("D",end='')
            prev=current
        print()
                
                
    def floodFillMod(self):
        queue = []
        for i in range(self.row*self.col):
            if self.flood_times[i//self.col][i%self.col] == 0:
                queue.append(i)
        
        
        while queue:
            v = queue.pop(0)
            i = v//self.col
            j = v%self.col
            temp = []
            if i < (self.row-1):
                    if self.flood_times[i+1][j] != -2:
                        temp.append(self.col*(i+1)+j)

               
            if j>0:
                if self.flood_times[i][j-1] != -2:
                    temp.append(self.col*i+j-1)

                
            if j < (self.col-1):
               if self.flood_times[i][j+1] != -2:
                    temp.append(self.col*i+j+1)
                
            if i > 0:
                if self.flood_times[i-1][j] != 2:
                    temp.append(self.col*(i-1)+j)

            for it in temp:
                if self.flood_times[it//self.col][it%self.col] == -1:
                    self.flood_times[it//self.col][it%self.col] = self.flood_times[v//self.col][v%self.col]+1
                    queue.append(it)


    def BFS(self):
        max=-2
        final_pos = self.cat_position
        row = self.row
        col = self.col

        s = self.cat_position
        moves = [-1]*(row*col)

        left = col+1
        up = row+1

        queue = []

        queue.append(s)
        self.cat_times[s//col][s%col]
        moves[s]=0

        while queue:
            s=queue.pop(0)

            safe_time = self.flood_times[s//col][s%col]-1

            if safe_time>max:
                max=safe_time
                left=s%col
                up=s//col
                final_pos=s
            
            if safe_time==max:
                if s//col < up:
                    up = s//col
                    left = s%col
                    final_pos=s
                elif s//col==up and s%col<left:
                    up = s//col
                    left = s%col
                    final_pos=s
            
            i = s//self.col
            j = s%self.col
            temp = []
            if i < (self.row-1):
                    if self.flood_times[i+1][j] !=-2:
                        temp.append(self.col*(i+1)+j)

               
            if j>0:
                if self.flood_times[i][j-1] != -2:
                    temp.append(self.col*i+j-1)

                
            if j < (self.col-1):
               if self.flood_times[i][j+1] != -2:
                    temp.append(self.col*i+j+1)
                
            if i > 0:
                if self.flood_times[i-1][j] != -2:
                    temp.append(self.col*(i-1)+j)
            
            for it in temp:
                if (moves[it]<0) and ((self.flood_times[it//col][it%col] > (self.cat_times[s//col][s%col]+1)) or (self.flood_times[it//col][it%col]==-1)):
                    
                    self.cat_times[it//col][it%col] = self.cat_times[s//col][s%col]+1
                    queue.append(it)
                    moves[it]=s
        
        del self.flood_times
        del self.cat_times
        
        self.print_results(max,final_pos,moves)


    
    def printGraph(self):
        print("Graph in the form of adjacency list : \n")
        for i in range(self.row*self.col):
            print("Node ",i, " : ", end='')
            for j in self.adjacency_list[i]:
                print(" -> ",j, end='')
            print("")

if __name__=="__main__":
    import sys
    with open(sys.argv[1],"rt") as f:

        input = [i.split() for i in f.readlines()]
        row = len(input)
        col = len(input[0][0])

        g1=Graph(row,col)
        g1.create_graph(input)
        g1.floodFillMod()
        del input
        g1.BFS()        
