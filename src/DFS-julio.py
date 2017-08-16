'''
Depth-first search
'''

#Input: Grafo
#Output: Se o grafo foi visitado, pre e pos visit e clock (contador)

def DFS(G):
    pre = { }
    post = { }
    clock = 0
    visited = {chr(65+i) : False for i in range(len(G))}
    for i in G:
        if False == visited[i]:
            explore_(G,i,visited,clock,pre,post)
    return [visited,pre,post,clock]
            
def explore_(G,V,visited,clock,pre,post):
    visited[V] = True
    previsited(V,clock,pre)
    for i in G:
        if False == visited[i]:
            clock = clock + 1
            explore_(G,i,visited,clock,pre,post)
            
    postvisited(V,clock,post) 

def previsited(o,clock,pre):
    clock = clock + 1
    pre[o] = clock

def postvisited(h,clock,post):
    clock = clock + 1
    post[h] = clock	

# exemplo de grafo
graph = {'A': ['D', 'E'], 
         'B': ['A', 'F', 'C'],
         'C': ['B', 'F'],
         'D': ['A'],
         'E': ['C', 'F'],
         'F': ['B', 'E']}

DFS(graph)