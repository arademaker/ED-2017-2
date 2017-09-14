# Given a sorted array of distinct integers A[1, . . . , n], you want
# to find out whether there is an index i for which A[i] = i. Give a
# divide-and-conquer algorithm that runs in time O(log n).


#Joao Marcos Amorim
#Imput: array ordenado
#Output: elemento == true se existe algum elemento igual o índice ou false c.c.

def bsearch ( a ) :
    n=len(a)
    if len(a) == 1:
        return a[0] == n
    m = len(a)//2
    if a[m] == m:
        return True
    if a[m] > m:
        return bsearch(a[0:m])
    else:
        return bsearch (a[m:])



# Example
a = [-3,-2,1,2,5]
