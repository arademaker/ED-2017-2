
Tenha os arquivo "entities.txt" e o aquivo "sentences.txt" presente na mesma pasta do seu query_trie.py

1.O arquivo entidades.txt é um arquivos que possui sua lista de entidades, com uma entidade por linha. Exemplo de arquivo entities.txt:
Joao
Joao Mario
Joao Rabelo
Mario Lucas
Mario Jorge
FGV
Rio de Janeiro

2.O arquivo senteces.txt é um simples txt com uma sentença por linha. Exemplo de arquivo sentences:
Rio de Janeiro é a melhor cidade do mundo.
FGV anuncia cortes nos salários dos funcionários.
Nunca antes Joao correu tanto.
Bandidos causam terror no Rio de Janeiro.
Joao Mario e Mario Lucas são melhores amigos.
joao mario.
mario gosta de comer carne.
Rio de Janeiro é a melhor cidade do mundo.
Joao e Guilherme tomam café na FGV todos os dias, mas Joao sempre toma mais.

3.No terminal, use o comando: 
	
	python get_trie.py entities.txt trie.txt

O aquivo obtido trie.txt será da forma:

{
    "rio": {
        "de": {
            "janeiro": {
                "finish": {}
            }
        }
    },
    "mario": {
        "jorge": {
            "finish": {}
        },
        "lucas": {
            "finish": {}
        }
    },
    "joao": {
        "finish": {},
        "mario": {
            "finish": {}
        },
        "rabelo": {
            "finish": {}
        }
    },
    "fgv": {
        "finish": {}
    }
}

4. Rode o query_trie.py fazendo o comando: 

python query_trie.py

O programa irá retornar dois resultados:

	1 - Um Counter com o número sentenças em que cada entidade aparece na lista de sentenças.
	2 - Um dicionário com o as entidades como key mapeadas a um vetor  (id, posicao), onde id é o número da sentença no documento (no caso o número da linha do documento) e id a posição da palavra naquela sentença. Ambos os valores são relativos a uma contagem iniciada em 0.
