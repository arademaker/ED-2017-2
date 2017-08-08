def merge_count_split (a, b):
        res = []
        inv_count = 0
        i=0
        j=0
        for k in range( len(a) + len(b) ):
                if i < len(a) and j < len(b):
                        if a[i] < b[j]:
                                res.append(a[i])
                                i += 1
                        elif a[i] > b[j]:
                                res.append(b[j])
                                inv_count += len(a)-i
                                j += 1
                elif i == len(a):
                        res.append(b[j])
                        j += 1
                elif j == len(b):
                        res.append(a[i])
                        i += 1
        return res, inv_count

def sort_and_count (data):
        n = len(data)
        if n <= 1:
                return data, 0
        a, x = sort_and_count(data[:(n/2)])
        b, y = sort_and_count(data[(n/2):])
        c, z = merge_count_split(a,b)
        return c, x + y + z

with open('IntegerArray.txt') as f:
        array = [int(line) for line in f]
        print sort_and_count(array)[0]
