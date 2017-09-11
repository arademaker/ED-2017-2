__author__ = 'graziele'
def mergesort(x):
    if len(x) > 1:
        return merge(mergesort(x[0:len(x)/2]), mergesort(x[len(x)/2:len(x)]))
    else:
        return x
