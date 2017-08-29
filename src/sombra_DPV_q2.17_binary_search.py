2.17


def binary_search(vet):
  first = 0
  last = len(vet)-1
  found = False
  while first<=last and not found:
    middle = (first + last)//2
    if vet[middle] == middle:
      found = True
    else:
      if middle < vet[middle]:
        last = middle-1
      else:
        first = middle+1
        
  return found

vet = [-3,1,10]
b = binary_search(vet)
print(b)
