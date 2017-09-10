# exercise 2.15 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
# Vazirani --- Algorithms (2006)
# Graziele Cerqueira

#In our median-finding algorithm (Section 2.4), a basic primitive is the split
#operation, which takes as input an array S and a value v and then divides S
#into three sets: the elements less than v, the elements equal to v, and the
#elements greater than v. Show how to implement this split operation in place,
#that is, without allocating new memory.

def splitInPlace(S,v):
    smaller = 0
    bigger = 0
    for i in range(0, len(S)):
        if S[i] < v:
            temp = S[i]
            S[i] = S[smaller]
            S[smaller] = temp
            smaller +=1
    bigger = smaller
    smaller -= 1
    for i in range(bigger, len(S)):
        if S[i] == v:
            temp = S[i]
            S[i] = S[bigger]
            S[bigger] = temp
            bigger +=1

    return S, smaller, bigger

    splitInPlace([55,1,7,4,2,3,6],6)
