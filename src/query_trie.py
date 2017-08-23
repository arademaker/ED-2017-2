from collections import Counter, defaultdict
import ast

#load trie
with open('trie.txt','r') as inf:
    trie = ast.literal_eval(inf.read())

#load sentences.txt
with open('sentences.txt','r') as sentence_file:
    sentences_vec = sentence_file.read().splitlines()


def wordInTrie(trie,pos: int ,entidade: list) -> bool: #recebe como parametro a trie, a posição na sentença e a entidade de retorno
    if pos >= len(sentenceVector):
        return False
    
    word = sentenceVector[pos]
    if 'finish' in trie and word not in trie:
        return True
    
    if word not in trie:
        return False
    
    entidade.append(word)
    entidade_aux = []
    
    if(wordInTrie(trie[word],pos+1,entidade_aux)):
        entidade += entidade_aux
        return True
    
    return 'finish' in trie

c = Counter()
places = defaultdict(list)


# For loop que transforma cada linha (sentença) do txt em um SentenceVector (vetor de palavra da sentença) e cria um contador "c" e um defaultdict "places"
# c --> Counter {entidade: n}  onde "n" é número de sentenças em sentences.txt que possui a entidade. 
# places --> dicionário das posiçoes das entidades no documento sentences.txt. {entidade: (id_da_sentença, posição_na_sentença)} 

for line_id, sentence in enumerate(sentences_vec): # sentences_vec= documento com todas as sentencas  (uma em cada linha) em formato de vetor
    sentenceVector = [word.lower() for word in sentence.split(' ')]
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

print(c)
print(places)