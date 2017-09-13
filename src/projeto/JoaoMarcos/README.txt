A função está dividida em 3 partes:

A primeira parte (Rollinghash) é caracterizada pela criação de uma classe para construção de hash da palavras.


A segunda parte (rabin_karp_multiple) é o algoritmo de Rabin-Karp para uma lista de patterns de mesmo tamanho e uma sentença.
	A entrada da mesma deve ser uma lista de patterns de mesmo tamanho e uma sentença.
	A saída é um dicionário indicando token e posição que foi achada na frase.

Ex: Input: rabin_karp_multiple(['casa','dedo','fora'],'Ele quebrou o dedo lá fora')
    Output:defaultdict(list, {'dedo': [14], 'fora': [22]})
Indica que o pattern dedo foi encontrado no caracter 14 da frase e o pattern fora no caracter 22.	



A terceira parte (search_in_list) é uma adaptação do algoritmo de Rabin-Karp para que a entrada seja uma lista de patterns, seja eles de tamanho semelhantes ou distintos, e uma lista de sentenças.
 	A entrada deve ser uma lista de patterns de qualquer tamanho e uma lista de sentenças
	A saída é um dicionário que indica a id da sentença, token e posição que a token foi encontrada naquela sentença.

Ex: Input: search_in_list([‘Fernando Henrique Cardoso’],sentenca) #sentenca é uma lista de frases
    Output: OrderedDict([(1, defaultdict(list, {'Fernando Henrique Cardoso': [[178]]})),
             (116, defaultdict(list, {'Fernando Henrique Cardoso': [[131]]})),
             (123, defaultdict(list, {'Fernando Henrique Cardoso': [[142]]})),
             (166, defaultdict(list, {'Fernando Henrique Cardoso': [[73]]})),
             (178, defaultdict(list, {'Fernando Henrique Cardoso': [[62]]})),
             (568, defaultdict(list, {'Fernando Henrique Cardoso': [[151]]})),
             (908, defaultdict(list, {'Fernando Henrique Cardoso': [[262]]})),
             (1901, defaultdict(list, {'Fernando Henrique Cardoso': [[39]]})),
             (2106, defaultdict(list, {'Fernando Henrique Cardoso': [[102]]})),
             (2171, defaultdict(list, {'Fernando Henrique Cardoso': [[39]]})),
             (2686, defaultdict(list, {'Fernando Henrique Cardoso': [[94]]})),
             (2798, defaultdict(list, {'Fernando Henrique Cardoso': [[147]]})),
             (2802, defaultdict(list, {'Fernando Henrique Cardoso': [[45]]})),
             (2913, defaultdict(list, {'Fernando Henrique Cardoso': [[78]]})),
             (2950, defaultdict(list, {'Fernando Henrique Cardoso': [[71]]})),
             (3009, defaultdict(list, {'Fernando Henrique Cardoso': [[78]]})),
             (3328, defaultdict(list, {'Fernando Henrique Cardoso': [[69]]})),
             (4208, defaultdict(list, {'Fernando Henrique Cardoso': [[50]]}))])
 

Indica que na sentença 1 foi encontrado Fernando Henrique Cardoso na posição 178, na sentença 116, Fernando Henrique Cardoso foi encontrado na posição 131 e assim por diante.


