'''
Programming Assinment 1 - Julio Cesar Vieira
'''

'''
Input: lista de elementos a serem ordenados
Output: contagem de vezes que foram feitas trocas de posicao
'''


def mergesort_(x):
    n = len(x)
    global cont
    cont += 1
    if n>1:
        t1 = mergesort_(x[0:int(n/2)])
        t2 = mergesort_(x[int(n/2):])
        return merge_(t1,t2,cont)
    else:
        return x
    
    
def merge_(x,y,cont):
    k = len(x)
    l = len(y)
    cont += 1
    if k == 0:
        return y
    if l == 0:
        return x
    if x[0]<=y[0]:
        return [x[0]] + merge_(x[1:],y,cont)
    else:
        return [y[0]] + merge_(x,y[1:],cont)


import numpy as np

f = np.genfromtxt("IntegerArray.txt")

print(f)
len(f)

cont = 0
mergesort_(f)
print(cont)



