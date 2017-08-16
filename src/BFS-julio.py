'''
Breadth-first search
'''

# Input: Grafo e posicao inicial
# Output: no' visitado e a distancia para chegar nele
def BFS(G,st):
    #b = float("inf")
    dist = { } 
    dist[st] = 0
    Q = [st]
    while Q:
        u = Q.pop(0)
        for x in G[u]:
            if x not in dist:
                Q.append(x)
                dist[x] = dist[u] + 1
    return dist


# exemplo de grafo
graph = {'A': ['D', 'E'], 
         'B': ['A', 'F', 'C'],
         'C': ['B', 'F'],
         'D': ['A'],
         'E': ['C', 'F'],
         'F': ['B', 'E']}


BFS(graph,"D")

BFS(graph,"E")

