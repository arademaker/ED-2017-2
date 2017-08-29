
# Rabin-Karp
class Rollinghash:
    '''
    Input:Word and len of the word
    Output: Hash of word
    '''
    def __init__(self, text, sizeword):
        self.text = text
        self.hash = 0
        self.sizeword = sizeword

        for i in range(0, sizeword):
            # ord maps the character to a number
            # subtract out the ASCII value of "a" to start the indexing at zero
            self.hash += ord(self.text[i])*(26**(sizeword - i - 1))

        # start index of current window
        self.window_start = 0
        # end of index window
        self.window_end = sizeword

    def move_window(self):
        if self.window_end <= len(self.text) - 1:
            # remove left letter from hash value
            self.hash -= ord(self.text[self.window_start]) * 26**(self.sizeword-1)
            self.hash *= 26
            self.hash += ord(self.text[self.window_end])
            self.window_start += 1
            self.window_end += 1

    def window_text(self):
        # return the substring of the text
        return self.text[self.window_start:self.window_end]


def rabin_karp_multiple(patterns, text):
    # input: List of patterns of same lenght
    # output: dict of token id in text
    from collections import defaultdict
    if type(patterns) != list: #O(1)
        return print("Patterns não é uma lista de patterns")
    result = defaultdict(list) #O(1)
    if patterns == [] or text == "": #O(1)
        return None
    for e in range(0,len(patterns)): # O(len(patterns))
        if len(patterns[e]) != len(patterns[0]):
            return print('Tamanho dos elementos da lista diferem')
    if len(patterns[0]) > len(text): #O(1)
        return None
    rolling_hash = Rollinghash(text, len(patterns[0]))
    patterns_hash = [ Rollinghash(p, len(patterns[0])) for p in patterns]
    for i in range(len(text) - len(patterns[0]) + 1):
        for n in range(0,len(patterns_hash)):
            if rolling_hash.hash == patterns_hash[n].hash:
                if rolling_hash.window_text() == patterns[n]:
                    result[patterns[n]].append(i)
        rolling_hash.move_window()
    return result
def search_on_list(patterns, sentences):
    # Input: patterns is a list of pattern and sentences list of sentences
    # output: list with id sentence and id token where was match between pattern and sentence
    # Otimização
    patterns.sort(key=len)
    aux = [len(i) for i in patterns]
    aux2 = {x: aux.count(x) for x in aux}
    start = 0
    end = 0
    from collections import defaultdict
    resultado = defaultdict(lambda: defaultdict(list))
    for t in aux2.values():
        end += t
        k = 0
        for j in sentences:
            aux = rabin_karp_multiple(patterns[start: end], j)
            if aux != {}:
                for g in aux.keys():
                    resultado[g][k].append(aux[g])
            k += 1
        start += end
    return dict(resultado)

resultado = (rabin_karp_multiple(["Renato", "Aranha", 'Marcos', 'Sombra', "Renato Aranha"],
                                 "Renato Aranha , Marcos fizeram o trabalho de SRI com ajuda do Sombra. Renato Aranha"))

(rabin_karp_multiple(["Joao", "Daniel", 'Lucas', 'Aranha'],
                     "Renato Aranha , Marcos fizeram o trabalho do Duda com ajuda do Sombra. Renato Aranha"))

out = search_on_list(["Renato", "Aranha", 'Marcos', 'Sombra', "Renato Aranha"], 
                     ["Renato Aranha , Marcos fizeram o trabalho de SRI com ajuda do Sombra.", 
                      "Renato Aranha e Sombra estavam conversando",'alguns marcos são relvantes',
                      "Marcos vai dormir tarde",'Sombra veio do IME'])

print(out)

