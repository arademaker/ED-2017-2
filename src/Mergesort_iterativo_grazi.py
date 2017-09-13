__author__ = 'Graziele'

def merge(x, y):
    if len(x) == 0:
        return y
    if len(y) == 0:
        return x
    if x[0] <= y[0]:
        return [x[0]] + merge(x[1:], y)
    else:
        return [y[0]] + merge(x, y[1:])
import Queue
def mergesort_iterativo(x):
    q = Queue.Queue()
    for i in range(0, len(x)):
        q.put([x[i]])
        i = i + 1
    while q.qsize() > 1:
        q.put(merge(q.get(), q.get()))
    return q.get()
