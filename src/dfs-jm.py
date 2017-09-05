
# coding: utf-8

### Depth-first-search with stack

graph = {'A': set(['B', 'C']),
         'B': set(['A', 'D', 'E']),
         'C': set(['A', 'F']),
         'D': set(['B']),
         'E': set(['B', 'F']),
         'F': set(['C', 'E']),
         'G': set(['H']),
         'H': set(['G'])
        }
#Imput: Grafo nao direcionado
#Output: Nós acessiveis a partir de um ponto
def dfs(graph, origem):
    visitados, stack = [], [origem]
    while stack:
        explore = stack.pop()
        if explore not in visited:
            visitados.append(explore)
            [stack.append(x) for x in graph[explore] if x not in visitados]
    return visitados

dfs(graph, 'D')

