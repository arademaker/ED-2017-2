#Antonio Luis Sombra de Medeiros
#Linearização de um DAG

#Entrada:  proximo_no (lista de vizinhos)
#SAÍDA : Uma linearização aleatória do grafo

from collections import defaultdict as dd
from random import choice, shuffle

#criar nos_com_arestasIn -->ddset
#criar qtd_arestas_In --> ddint
#criar proximo_no --> ddvet

proximo_no = {5: [0,2], 4:[0,1], 0:[], 1:[], 2:[3], 3:[1]}

qtd_arestas_In = dd(int)
nos_com_arestasIn = dd(set)

for no in proximo_no:
    for jt in proximo_no[no]:
        qtd_arestas_In[jt] += 1

for no in proximo_no:
    if no not in qtd_arestas_In:
        qtd_arestas_In[no] = 0
        
for no, aresta in qtd_arestas_In.items():
    nos_com_arestasIn[aresta].add(no)
   
linearizacao = []
while nos_com_arestasIn[0]:
  it = choice(list(nos_com_arestasIn[0])) #aleatoriza pra nao dar sempre a mesma linearização
  nos_com_arestasIn[0].remove(it)
  linearizacao.append(it)
  for no in proximo_no[it]:
    p = qtd_arestas_In[no]
    qtd_arestas_In[no] -=1
    nos_com_arestasIn[p].remove(no)
    nos_com_arestasIn[p-1].add(no)
    
print('Linearizacao deste DAG: ', linearizacao)

