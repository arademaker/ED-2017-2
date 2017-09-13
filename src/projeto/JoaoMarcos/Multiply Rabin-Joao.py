# Rabin-Karp


class Rollinghash:
    '''
    Input:Word and len of the word
    Output: Hash of word class
    '''

    def __init__(self, text, sizeword):
        self.text = text
        self.hash = 0
        self.sizeword = sizeword

        for i in range(0, sizeword):
            # ord maps the character to a number
            # subtract out the ASCII value of "a" to start the indexing at zero
            self.hash += ord(self.text[i]) * (26 ** (sizeword - i - 1))

        # start index of current window
        self.window_start = 0
        # end of index window
        self.window_end = sizeword

    def move_window(self):
        if self.window_end <= len(self.text) - 1:
            # remove left letter from hash value
            self.hash -= ord(self.text[self.window_start]) * 26 ** (self.sizeword - 1)
            self.hash *= 26
            self.hash += ord(self.text[self.window_end])
            self.window_start += 1
            self.window_end += 1

    def window_text(self):
        # return the substring of the text
        return self.text[self.window_start:self.window_end]


def rabin_karp_multiple(patterns, text):
    # input: List of patterns of same lenght and a sentence
    # output: dict of token id in text
    from collections import defaultdict
    result = defaultdict(list)  # O(1)
    if type(patterns) != list:  # O(1)
        return print("Patterns não é uma lista de patterns")
    if patterns == [] or text == "":  # O(1)
        return {}
    if len(patterns[0]) > len(text):  # O(1)
        return {}

    rolling_hash = Rollinghash(text, len(patterns[0]))
    patterns_hash = [Rollinghash(p, len(patterns[0])) for p in patterns]

    for i in range(len(text) - len(patterns[0]) + 1):
        for n in range(0, len(patterns_hash)):
            if rolling_hash.hash == patterns_hash[n].hash:
                if rolling_hash.window_text() == patterns[n]:
                    result[patterns[n]].append(i)
        rolling_hash.move_window()
    return result  # O(n+km), k patterns of lenght m, n lenght of sentence


def search_in_list(patterns, sentences):
    # Input: patterns is a list of pattern and sentences is a list of sentences
    # output: list with id sentence and id token where was match between pattern and sentence
    # k is the lenght of list of patterns

    from collections import defaultdict
    patterns.sort(key=len)  # O(klog(k))
    aux = [len(i) for i in patterns]  # O(k)
    aux2 = {x: aux.count(x) for x in aux}  # O(k)
    start = 0
    end = 0
    resultado = defaultdict(lambda: defaultdict(list))
    for t in aux2.values():  # O(j(n+km)) j is number of sentences, n lenght of highest sentence
        end += t
        k = 0  # sentence
        for j in sentences:
            part = rabin_karp_multiple(patterns[start: end], j)
            if part != {}:
                for g in part.keys():
                    resultado[k][g].append(part[g])
            k += 1
        start = end
    return resultado


# lendo a lista de sentenças
sentenca = []
with open('/home/joao/Dropbox/FGV/ED-2017-2/src/sentences.txt') as f:
    for linha in f:
        linha = linha.strip()
        sentenca.append(linha)

organizacao = list()
with open('/home/joao/Dropbox/FGV/ED-2017-2/src/organizacao.txt') as f:
    for linha in f:
        linha = linha.strip()
        organizacao.append(linha)

pessoas = list()
with open('/home/joao/Dropbox/FGV/ED-2017-2/src/pessoa-individuo.txt') as f:
    for linha in f:
        linha = linha.strip()
        pessoas.append(linha)

pessoas_resultado =  search_in_list(pessoas, sentenca)

print(pessoas_resultado)

organizacoes_resultado = search_in_list(organizacao, sentenca)

print(organizacoes_resultado)

