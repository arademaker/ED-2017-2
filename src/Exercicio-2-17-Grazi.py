# exercise 2.17 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
# Vazirani --- Algorithms (2006)
# Graziele Cerqueira


#Given a sorted array of distinct integers A[1, ...,  n], you want to find
#out whether there is an index i for which A[i] = i. Give a divide-and-conquer
# algorithm that runs in time O(log n).

def exercicio17(L):
    n = len(L)
    if n==1:
        if L[0] == 0:
            print 'true'
    else:
        if L[n/2] == n/2:
            print 'true'
        else:
            if L[n/2] > n/2:
                exercicio17(L[:n/2])
            else:
                exercicio17(L[n/2:]-n/2)
