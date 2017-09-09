#Imput:Uma lista e um índice (i) da lista
#Output: Elemento no índice i da lista



lista= (0, (1, (2, ('Sombra', (4, None)))))
def busca_elemento(lista, indice):
    if lista==None:
        print("A lista está vazia")
    else:
        for i in range(1,indice):
        lista=lista[1]
        print(lista[0])
busca_elemento(lista,4)

