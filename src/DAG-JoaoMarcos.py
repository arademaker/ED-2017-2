
# coding: utf-8

# ### Todas linearizações de um DAG, saindo de um nó até um nó destino

# In[1]:

grafo = {'A' : ['B','C'],    
        'B' : ['C'],
        'C' : ['D','E'],
        'D' : ['E'],
        'E':['F','G','H']}


# In[2]:

def dfs_all(graph, caminho, caminhos = []):
    no = caminho[-1] 
    if no in graph:
        for vizinho in graph[no]:
            descoberta = caminho + [vizinho]
            paths = dfs_all(graph, descoberta, caminhos)
    else:
        caminhos += [caminho]
    return caminhos


# In[3]:

dfs_all(grafo, ['A'],caminhos=[])

