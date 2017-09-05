
# coding: utf-8

# 
# This file contains all of the 100,000 integers between 1 and 100,000
# (inclusive) in some order, with no integer repeated.
# 
# Your task is to compute the number of inversions in the file given,
# where the ith row of the file indicates the ith entry of an array.
# 
# Because of the large size of this array, you should implement the fast
# divide-and-conquer algorithm covered in the video lectures.
# 
# The numeric answer for the given input file should be typed in the
# space below.
# 
# So if your answer is 1198233847, then just type 1198233847 in the
# space provided without any space / commas / any other punctuation
# marks. You can make up to 5 attempts, and we'll use the best one for
# grading.
# 
# (We do not require you to submit your code, so feel free to use any
# programming language you want --- just type the final numeric answer
# in the following space.)
# 
# [TIP: before submitting, first test the correctness of your program on
# some small test files or your own devising. Then post your best test
# cases to the discussion forums to help your fellow students!]
# 


inv=0
def merge(p1, p2):
        global inv 
        array=[]
        j, k = 0, 0       
        while j < len(p1) and k < len(p2):
            if p1[j] <= p2[k]:
                array.append(p1[j])
                j += 1
            else:
                array.append(p2[k]) 
                k += 1
                inv += (len(p1) - j)
        while j < len(p1):
            array.append(p1[j])
            j += 1
        while k < len(p2):
            array.append(p2[k])
            k += 1
        return array
        #return print('Número de inversões {}'.format(inv))
def mergesort(array):
    if len(array)==1:
        return array
    else:
        p1=array[:len(array)//2]
        p2=array[len(array)//2:]
        p1=mergesort(p1)
        p2=mergesort(p2)
        return merge(p1,p2) 

def buildListFromFile(filename):
    intList = []
    f = open(filename, 'r')
    for line in f:
        token = line.strip()
        intList.append(int(token))
    f.close()
    return intList




array=buildListFromFile('IntegerArray.txt')
array=list(array)




len(array)



print(mergesort(array)[:10])
print(inv)



