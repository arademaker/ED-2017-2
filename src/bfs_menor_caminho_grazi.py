__author__ = 'Graziele'

def bfs_menor_caminho(graph, start, goal):
    # Acompanhar os nós explorados
    explored = []
    # Acompanha todos oa caminhos a serem verificados
    queue = [[start]]

    #Retorna o caminho se o incio foi o objetivo
    if start == goal:
        return "encontramos"

    #Mantém o loop até que todos os caminhos possíveis tenham sido verificados
    while queue:
        path = queue.pop(0)
        # Obtem o último nó do caminho
        node = path[-1]
        if node not in explored:
            neighbours = graph[node]
            # Atravessa todos os nós vizinhos, construa um novo caminho e
            #Empurre-o para a fila
            for neighbour in neighbours:
                new_path = list(path)
                new_path.append(neighbour)
                queue.append(new_path)
                # retorna o caminho se o vizinh é o objetivo
                if neighbour == goal:
                    return new_path

            #Marca o nó como explorado
            explored.append(node)

    # Caso não exista caminho entre dois nós
    return " O caminho entre os nós não existe"

graph = {'A': ['B', 'C', 'E'],
         'B': ['A','D', 'E'],
         'C': ['A', 'F', 'G'],
         'D': ['B'],
         'E': ['A', 'B','D'],
         'F': ['C'],
         'G': ['C']}

bfs_menor_caminho(graph, 'G', 'D')  # retorna ['G', 'C', 'A', 'B', 'D']
