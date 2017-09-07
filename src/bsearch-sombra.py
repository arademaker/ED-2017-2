# Antônio Luís Sombra de Medeiros - Ex. 2.7 - Papadimitriou


# Given a sorted array of distinct integers A[1, . . . , n], you want
# to find out whether there is an index i for which A[i] = i. Give a
# divide-and-conquer algorithm that runs in time O(log n).

def binary_search(vet):
  first = 0
  last = len(vet) - 1
  found = False
  while first <= last and not found:
    middle = (first + last) // 2
    if vet[middle] == middle:
      found = True
    else:
      if middle < vet[middle]:
        last = middle - 1
      else:
        first = middle + 1
        
  return found

vet = [-3, 1, 10]
print("Test example: ")
print("vetor = ", vet)
b = binary_search(vet)
print("Resposta: ", b)
