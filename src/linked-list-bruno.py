"""
authors: bruno cuconato (@odanoburu)
placed in the public domain.
"""

class cons:
    def __init__(self, car, cdr=None):
        if car is None:
            self.car = []
            self.cdr = []
        else:
            self.car = car
            if cdr is None:
                self.cdr = []
            else:
                self.cdr = cdr

    def recursive_search(self, ix, linked_list=None):
        "recursive solution"
        if linked_list is None:
            linked_list = self
        if ix == 0:
            return linked_list.car
        else:
            return self.recursive_search(ix-1, linked_list=linked_list.cdr)
    def search(self, ix):
        "loop solution"
        linked_list = self
        while ix > 0:
            linked_list = linked_list.cdr
            ix -= 1
        else:
            return linked_list.car

# tests

a = cons(1, cons(4, cons('5', cons(True, cons((3, 4))))))
assert a.search(2) == '5'
assert a.search(1) = 4
assert a.search(4) == a.recursive_search(4)
assert a.search(3)
