__author__ = 'graziele'
def DFS(query_node, parents):
    # Acompanha todos os nós visitados
    result = {}
    # Acompanha os nós a serem verificados
    stack = []
    stack.append( (query_node, 0) )
    while len(stack) > 0:
        print("stack=", stack)
        node, dist = stack.pop()
        result[node] = dist
        if node in parents:
            for parent in parents[node]:
                stack_members = [x[0] for x in stack]
                if parent not in stack_members:
                    stack.append( (parent, dist+1) )
    return result

def BFS(query_node, parents):

    result = {}
    queue = []
    queue.append( (query_node, 0) )
    #Como podemos notar, QU já tem um nó a ser verificado, ou seja, o vértice
    #de início que é usado como um ponto de entrada para explorar o gráfico.
    #O próximo passo é implementar um loop que mantém o ciclo até QU ficar
    #vazio. Em cada iteração do loop, um nó é verificado. Isso, se já não foi
    #visitado, seus vizinhos são adicionados QU.
    # Mantém o loop até que existam nós que ainda devem ser verificados
    while queue:
        print("queue=", queue)
        node, dist = queue.pop(0)
        result[node] = dist
        if node in parents:
            for parent in parents[node]:
                queue_members = [x[0] for x in queue]
                if parent not in result and parent not in queue_members:
                    queue.append( (parent, dist+1) )
    return result

if __name__ == "__main__":

    parents = dict()
    parents = {'N1': ['N2', 'N3', 'N4'], 'N3': ['N6', 'N7'], 'N4': ['N3'], 'N5': ['N4', 'N8'], 'N6': ['N13'],
               'N8': ['N9'], 'N9': ['N11'], 'N10': ['N7', 'N9'], 'N11': ['N14'], 'N12': ['N5']}

    print("DFS:")
    dist = DFS('N1', parents)
    print(dist)

    print("BFS:")
    dist = BFS('N1', parents)
    print(dist)
