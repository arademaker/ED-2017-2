'''
Merge sort - Julio Cesar Vieira
'''

'''
Input: uma lista de elementos a serem ordenados
Output: lista ordenada
'''

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