#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv){
    int i;
    FILE *fp;
    int N,K;
    int *c, *hash;
    fp = fopen(argv[1], "r");
    
    fscanf(fp, "%d %d", &N, &K);

    c = (int *) malloc(N*sizeof(int));
    hash = (int *) malloc(K*sizeof(int));
    

    for(i=0;i<N;i++){
        fscanf(fp,"%d",&c[i]);
    
    }


    for(i=0;i<K;i++){
        hash[i]=0;
    }
    
    int begin=0, end=-1;
    int current_min=0;
    int counter=0;
    
    
    while(1){
        if(counter<K){
            if(++end == N)
                break;
            if(hash[c[end]-1] == 0) counter++;
            
            hash[c[end]-1]++;
            continue;
        }
        
        while(hash[c[begin]-1] > 1){
            hash[c[begin]-1]--;
            begin++;
        }
        
        if((current_min == 0) || end - begin < current_min ){
            current_min = end - begin+1;
        }
        
        hash[c[begin]-1]--;
        begin++;
        counter--;
    }
    printf("%d\n",current_min);

    fclose(fp);
}