def sort(vn):
  minimum, maximum = min(vn),max(vn)
  m = (maximum-minimum)
  vm = (m+1)*[0]
  
  for it in vn:
    vm[it-minimum] += 1
  
  p = 0
  for val, freq in enumerate(vm,minimum):
    for _ in xrange(freq):
      vn[p] = val
      p += 1

vn = [1,3,5,1,2,1]
sort(vn)
print(vn)