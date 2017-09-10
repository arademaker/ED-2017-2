Observação inicial:
	Tenha os arquivos "entities.txt" e "sentences.txt" presentes na mesma pasta

Execução:
	Entre na pasta BortoliniFGV
	Entre na pasta dist
	Execute o comando java -jar BortoliniFGV.jar
	Entre com o path dos arquivos "entities.txt" e "sentences.txt" (Exemplo: C:\\Users\\Matheus\\Desktop)

Observações de execução:
	Aplicação cria uma Trie com os nomes presentes em "entities.txt"
	Aplicação substitui espaços e sinal de pontuação nas frases por ; e depois retira ;s duplicados

Saída:
	Aplicação imprime todas as frases na tela e as entidades que ocorrem em cada frase com sua respectiva posição
	Aplicação salva esse mesmo output num arquivo "search.txt" no mesmo path dos arquivos "entities.txt" e "sentences.txt"

Observação final:
	Dentro dessa pasta estão arquivos "entities.txt" e "sentences.txt" de exemplo e o "search.txt" gerado por eles