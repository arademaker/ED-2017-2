# Antonio Luís Sombra de Medeiros - 2017

# Programa que carrega um arquivo 'sentences'e um arquivo trie', 
# obtido através do create_trie.py alimentado por um arquivo de entidades.

#Modo de uso: Modifique as linhas 19 e 25 com os nomes dos 
#seus respectivos arquivos rode o script: python sombra_query_trie.py


from collections import Counter, defaultdict
import ast
import string

#DEFINA QUAL TRIE VOCÊ UTILIZARÁ AQUI:

#trie_pessoa.txt  ou  trie_organizacao.txt 

#load trie
with open('trie_organizacao.txt','r') as inf:
    trie = ast.literal_eval(inf.read())

#load sentences.txt  + tratamento das sentences para fazer a busca
sentences_vec = []
punctuation = set(string.punctuation).union({'»','«'}) - {'-'}
with open('sentences.txt','r') as sentence_file:
    for sentence in sentence_file.read().splitlines():
        sentence.strip().strip('.')
        sentence.replace('»','')
        sentence.replace('«','')
        for ch in punctuation:
            sentence = sentence.replace(ch, ' '+ch+' ')
            sentence = sentence.replace('  ', ' ')
        sentences_vec.append(sentence)
    
#Função recursiva que recebe como parametro a trie, 
#a posição na sentença e a entidade de retorno.
#entidade eh saida e nao entrada

def wordInTrie(trie,pos: int ,entidade: list) -> bool: 
    
    if pos >= len(sentenceVector):
        return False
    
    word = sentenceVector[pos]
    
    if word not in trie:
        return False
    
    entidade.append(word)
    entidade_aux = []
    
    if(wordInTrie(trie[word],pos+1,entidade_aux)):
        entidade += entidade_aux
        return True
    return 'finish' in trie[word]

c = Counter()
places = defaultdict(list)


# For loop que transforma cada linha (sentença) do txt em um SentenceVector (vetor de palavra da sentença) e cria um contador "c" e um defaultdict "places"
# c --> Counter {entidade: n}  onde "n" é número de sentenças em sentences.txt que possui a entidade. 
# places --> dicionário das posiçoes das entidades no documento sentences.txt. {entidade: (id_da_sentença, posição_na_sentença)} 

for line_id, sentence in enumerate(sentences_vec): # sentences_vec= documento com todas as sentencas  (uma em cada linha) em formato de vetor
    sentenceVector = [word for word in sentence.split(' ')] ###
    end = -1
    for i, word in enumerate(sentenceVector):
        if i <= end:
            continue
        entidade = []
        boolean  = wordInTrie(trie,i,entidade)
        if boolean:
            end = i+len(entidade)-1
            entidade_palavra = ' '.join(entidade)
            c[entidade_palavra] += 1
            places[entidade_palavra].append((line_id,i))

print("Total de entidades encontradas: ", len(c))
print()
print("Sentença | Entidade | id da Entidade na Sentença: ")
print()
for x,y in places.items():
    for i in range(len(y)):
        print(str(y[i][0])+" | ",x+" | ",y[i][1])

print()
print()
print("Counter com as Entidades e a quantidade de sentenças em que ela aparece: ")
print()
print(c)
print()


