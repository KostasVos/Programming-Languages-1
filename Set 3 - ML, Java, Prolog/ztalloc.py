class ZtallocSolver(object):
    def __init__(self,input):
        self.input=input
        self.previous_state = {}
    
    def BFSolver(self):
        counter = 0
        queue = []
        origin = (self.input[0],self.input[1])
        queue.append(origin)

        while queue:
            counter = counter+1
            current = queue.pop(0)
            low = current[0]
            high = current[1]

            if(low>= self.input[2] and high<=self.input[3]):
                break
            next_state1 = (low//2,high//2)
            next_state2 = (3*low+1, 3*high+1)

            if(not (next_state1 in self.previous_state)):
                self.previous_state.update({next_state1 : current})
                queue.append(next_state1)

            if((not (next_state2 in self.previous_state)) and (3*high+1 < 1000000)):
                self.previous_state.update({next_state2 : current})
                queue.append(next_state2)
        

        if current == origin:
            print("EMPTY")
        elif not queue:
            print("IMPOSSIBLE")
        else:
            temp = []
            j = current
            while j != origin:
                temp = [j]+temp
                j = self.previous_state[j]
            prev = self.input[0]
            for i in temp:
                if i[0] == prev//2:
                    print("h",end='')
                else:
                    print("t",end='')
                prev=i[0]
            print()

if __name__=="__main__":
    import sys
    with open(sys.argv[1],"rt") as f:

        content = f.readlines()

    content = [x.strip() for x in content]
    input_number = int(content.pop(0))

    content = [[int(n) for n in x.split()] for x in content]

    for i in range(input_number):
        z1 = ZtallocSolver(content[i])
        z1.BFSolver()
        del z1        