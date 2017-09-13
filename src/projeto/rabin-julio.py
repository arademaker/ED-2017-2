'''
Rabin Karp
'''

class RollingHash:
    def __init__(self, text, sizeWord):
        self.text = text
        self.hash = 0
        self.sizeWord = sizeWord

        for i in range(0, sizeWord):
            #ord maps the character to a number
            #subtract out the ASCII value of "a" to start the indexing at zero
            self.hash += (ord(self.text[i]) - ord("a")+1)*(26**(sizeWord - i -1))

        #start index of current window
        self.window_start = 0
        #end of index window
        self.window_end = sizeWord

    def move_window(self):
        if self.window_end <= len(self.text) - 1:
            #remove left letter from hash value
            self.hash -= (ord(self.text[self.window_start]) - ord("a")+1)*26**(self.sizeWord-1)
            self.hash *= 26
            self.hash += ord(self.text[self.window_end])- ord("a")+1
            self.window_start += 1
            self.window_end += 1

    def window_text(self):
        return self.text[self.window_start:self.window_end]

def rk_aux(word, text):
    if word == "" or text == "":
        return None
    if len(word) > len(text):
        return None
    rolling_hash = RollingHash(text, len(word))
    word_hash = RollingHash(word, len(word))
    #word_hash.move_window()
    cc = 0
    ttu = 0
    for i in range(len(text) - len(word) + 1):
        if rolling_hash.hash == word_hash.hash:
            if rolling_hash.window_text() == word:
                cc = cc + 1
                ttu = i
        rolling_hash.move_window()
    if not cc:
        return None
    return cc,ttu


'''
Input: words - variavel do tipo list - palavras/entidades a serem buscadas
       texts - variavel do tipo list - com textos onde as words serÃ£o procuradas
Output: 'texto:', o numero , qnts vezes no mesmo texto,'- palavra:', a posicao na lista ,'- token:', num de onde comeca

'''

def rabin_karp(words,texts):
    if words == [] or texts == []:
        return None
    res = []
    for ii in range(len(words)):
        for jj in range(len(texts)):
            iio = rk_aux(words[ii],texts[jj])
            if iio != None:     
                res.append([jj,[ii,iio]])
                #res.append('texto: '+str(jj)+' - palavra: '+str(words[ii])+" - "+str(ii)+' - token sentenca: '+str(iio[1]))
    return res


texto = open("sentences.txt", "r",encoding='utf8')
sentencas = texto.read().split('\n')
texto.close()


texto = open("pessoa-individuo.txt", "r",encoding='utf8')
pess_ind = texto.read().split('\n')
texto.close()


texto = open("organizacao.txt", "r",encoding='utf8')
organizacao = texto.read().split('\n')
texto.close()

respon = rabin_karp(pess_ind,sentencas)


import json


with open('data-pessoa-individuo.json', 'w',encoding="utf8") as outfile:  
    json.dump(respon, outfile)



saida = rabin_karp(organizacao,sentencas)


with open('data_organizacao.json', 'w',encoding="utf8") as outfile:  
    json.dump(saida, outfile)


print(respon[0:100])

print(saida[0:100])

# exemplo inicial de teste

TEXTOS = ["Joao Marcos foi a praia no sabado","Apesar da chuva Julio Cesar saiu e foi ver Bruno.Julio Cesar e Bruno sao loucos",
          "Julio e Joao sairam","Joao, Marcos e Bruno da Silva"]
PALAVRAS = ["Julio Cesar","Joao","Joao Marcos","Bruno da Silva"]

#rabin_karp(PALAVRAS,TEXTOS))

