# Antônio Luís Sombra de Medeiros - Ex. 2.20 - Papadimitriou

# takes an array of integers and sorts them. the process is done in
# three steps:
# 1. count the numbers of each integer;
# 2. count how many elements are less than each integer;
# 3. fill up sorted vector with this info.

def sort(vn):
  minimum, maximum = min(vn), max(vn)
  m = (maximum - minimum)
  vm = (m+1)*[0]
  
  for it in vn:
    vm[it-minimum] += 1
  
  p = 0
  for val, freq in enumerate(vm,minimum):
    for _ in range(freq):
      vn[p] = val
      p += 1

vn = [1,3,5,1,2,1]


print("Test example: ")

print("vet = ", vn)

sort(vn)

print("Resposta: ", vn)

