# Antônio Luis Sombra de Medeiros

# DFS Implementation using stack

#grafo =  Neighbors List
#Output = dfs(grafo, nó_inicial)

no_inicial = 'A'
grafo = {'A': ['B', 'C'],
         'B': ['D', 'E'],
         'C': ['F'],
         'D': [],
         'E': ['F'],
         'F': []
         }

def dfs(graph, start):
    stack, visited = [start], []

    while stack:
        vertex = stack.pop()
        if vertex in visited:
            continue
        visited.append(vertex)
        for neighbor in graph[vertex]:
            stack.append(neighbor)

    return visited

print("Grafo: ", grafo)
print()
print("DFS começando por :", no_inicial)
print(dfs(grafo, no_inicial)) 

# Uma solucao: ['A', 'C', 'F', 'B', 'E', 'D']