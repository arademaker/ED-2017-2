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

print(dfs(grafo, 'A')) # Uma solucao: ['A', 'C', 'F', 'B', 'E', 'D']