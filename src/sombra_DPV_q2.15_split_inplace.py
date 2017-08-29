def f(vet,val):
  less = sum(it<val for it in vet) # number of elements less than val in the vector -> O(n)
  equal = sum(it==val for it in vet) # number of elements equal to val in the vector -> O(n)
  
  i,j,k = 0,less,less+equal #starting index of the 3 groups inside sorted vet
  while i<less:
    if vet[i] < val:
      i += 1
    elif vet[i] == val:
      vet[i], vet[j] = vet[j], vet[i]
      j += 1
    else:
      vet[i], vet[k] = vet[k], vet[i]
      k += 1
  
  while j<less+equal:
    #print j,k
    #print vet
    if vet[j] == val:
      j += 1
    else:
      vet[j], vet[k] = vet[k], vet[j]
      k += 1

v = [1,5,3,1,2,-7,1,5,6,8,1]
f(v,1)
print(v)
