def mergesort_(x):
    n = len(x)
    if n>1:
        t1 = mergesort_(x[0:int(n/2)])
        t2 = mergesort_(x[int(n/2):])
        return merge_(t1,t2)
    else:
        return x
    
    
def merge_(x,y):
    k = len(x)
    l = len(y)
    if k == 0:
        return y
    if l == 0:
        return x
    if x[0]<=y[0]:
        return [x[0]] + merge_(x[1:],y)
    else:
        return [y[0]] + merge_(x,y[1:])

r = [8,4,2,6]
mergesort_(r)

r = [2,2,2,1]
mergesort_(r)

r = [-8,4,-2,6]
mergesort_(r)


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



