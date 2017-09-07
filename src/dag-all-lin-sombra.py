#Antonio Luis Sombra de Medeiros

#Algoritmo de exibição de TODAS as linearizações de um grafo

#Estrutura usadas na entrada: proximo_no (lista de vizinhos que identifica o grafo)

#Complexidade: O(n!)

from collections import defaultdict as dd
from random import choice, shuffle

resp = set()
visited = set()

proximo_no = {5: [0,2], 4:[0,1], 0:[], 1:[], 2:[3], 3:[1]}
#proximo_no = {0: [], 1: []}
totNos = proximo_no.keys()
ltotNos = len(totNos)

qtd_arestas_In = dd(int)

#Cálculo e criação da estrutura qtd_arestas_In
for no in proximo_no:
    for jt in proximo_no[no]:
        qtd_arestas_In[jt] += 1

for no in proximo_no:
    if no not in qtd_arestas_In:
        qtd_arestas_In[no] = 0

def linearizacao():
    n = ltotNos-len(visited)
    if n == 0:
        return [[]]

    vtot = []
    for no in totNos:
        if qtd_arestas_In[no] == 0 and no not in visited:
            visited.add(no)
            for no_vizinho in proximo_no[no]:
                if no_vizinho not in visited:
                    qtd_arestas_In[no_vizinho] -= 1
                    
            rparcial = linearizacao()
            rparcial = list(filter(lambda x: len(x) == n-1, rparcial))
            vtot += [[no]+it for it in rparcial if len(it)==n-1]
            
            for no_vizinho in proximo_no[no]:
                if no_vizinho not in visited:
                    qtd_arestas_In[no_vizinho] += 1
         
            visited.remove(no)
    
    return vtot

resp = linearizacao()
print(resp)
print('TOTAL: ', len(resp))



'''
Todas as linearizações do grafo dado são:
4 5 0 2 3 1 
4 5 2 0 3 1 
4 5 2 3 0 1 
4 5 2 3 1 0 
5 2 3 4 0 1 
5 2 3 4 1 0 
5 2 4 0 3 1 
5 2 4 3 0 1 
5 2 4 3 1 0 
5 4 0 2 3 1 
5 4 2 0 3 1 
5 4 2 3 0 1 
5 4 2 3 1 0 
'''