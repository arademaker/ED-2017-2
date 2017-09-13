
Tenha os arquivos “trie_pessoa.txt” e “trie_organizacao.txt”, “organizacao.txt”, “pessoa-individuo.txt” e "sentences.txt" presentes na mesma pasta do seu sombra_query_trie.py. Abra o arquivo sombra_query_trie.py e na parte #load trie coloque o nome da trie que vc quer usar, correspondente a sua lista de entidades (trie_pessoa.txt ou trie_organizacao.txt). Depois rode o programa sombra_query_trie.txt

ou:

1.Crie ou copie para a pasta do programa um arquivo entities.txt como um arquivos que possui sua lista de entidades, com uma entidade por linha. Exemplo de arquivo entities.txt:
Joao
Joao Mario
Joao Rabelo
Mario Lucas
Mario Jorge
FGV
Rio de Janeiro

OBS: os arquivos “organizacao.txt” e “pessoa-individuo.txt” são exemplos de arquivos entities.

2.Crie ou copie para a pasta do programa um arquivo senteces.txt como um simples txt com uma sentença por linha. Exemplo de arquivo sentences:
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
	
	python create_trie.py entities.txt trie.txt

obs: o primeiro argumento do create_trie é o arquivo de entidades que você irá usar pra criar a trie e o segundo é o nome do arquivo onde vai ser salvo a trie.

obs2: certifique-se que o script “create_trie.py” está no seu diretório corrente.
Também certifique-se que o module simplejson está instalado, senão faça o comando:
	
	pip install simplejson


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

4. Rode o sombra_query_trie.py fazendo o comando: 

	python sombra_query_trie.py

O programa irá retornar 3 resultados:

	1 - A quantidade de entidades encontrada no arquivo de sentenças
	2 - Saída na forma (id da Sentença | Entidade | id da Entidade na Sentença) por linha 
	3 - Um Counter com o número sentenças em que cada entidade aparece na lista de sentenças.
	
