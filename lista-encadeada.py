
class Cons:
    car = []
    cdr = []

    def __init__(self, a, b):
        self.car = a
        self.cdr = b

a = Cons(1, Cons(2, Cons(3, 0)))

print(a)
