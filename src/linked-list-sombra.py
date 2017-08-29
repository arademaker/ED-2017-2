# Implementação em python da busca do i-ésimo elemento da lista
# encadeada usando a abordagem do lisp (funçao nth)
# A funçao length é uma funçao que retorna o tamanho da lista

cons   = lambda el, lst: (el, lst)
car = lambda lst: lst[0] if lst else lst
cdr = lambda lst: lst[1] if lst else lst
nth = lambda n, lst: nth(n-1, cdr(lst)) if n > 0 else car(lst)
length  = lambda lst, count=0: length(cdr(lst), count+1) if lst else count

lista = cons(1,cons(2,cons(3,cons(4,[]))))
print('lista = ', lista)
i = 3
print('i-esimo elemento da lista, i = {}:  {}'.format(i,nth(i,lista)))
