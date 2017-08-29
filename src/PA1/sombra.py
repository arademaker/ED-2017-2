#Programming Assignment 1 - Sombra

with open('IntegerArray.txt') as f:
    integers = f.readlines()
    
num_inversion = 0

def merge(left, right):
    global num_inversion
    len_left = len(left)
    len_right = len(right)
    i = 0
    j = 0
    merged = []
    while (i < len_left and j < len_right):
        if left[i] <= right[j]:
            merged.append(left[i])
            i = i + 1
        else:
            merged.append(right[j])
            j = j + 1
            num_inversion = num_inversion + len_left - i
    merged = merged + left[i:]
    merged = merged + right[j:]
    return merged

def sort(data):
    length = len(data)
    if length == 1:
        return data
    else:
        n = length // 2
        return merge(sort(data[:n]), sort(data[n:]))


integers = [int(integer) for integer in integers]
merged_sort = sort(integers)

print(num_inversion)