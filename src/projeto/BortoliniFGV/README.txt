Observa��o inicial:
	Tenha os arquivos "entities.txt" e "sentences.txt" presentes na mesma pasta

Execu��o:
	Entre na pasta projeto
	Entre na pasta BortoliniFGV
	Entre na pasta dist
	Execute o comando java -jar BortoliniFGV.jar
	Entre com o path dos arquivos "entities.txt" e "sentences.txt" (Exemplo: C:\\Users\\Matheus\\Desktop)

Observa��es de execu��o:
	Aplica��o cria uma Trie com os nomes presentes em "entities.txt"
	Aplica��o substitui espa�os e sinal de pontua��o nas frases por ; e depois retira ;s duplicados

Sa�da:
	Aplica��o imprime todas as frases na tela e as entidades que ocorrem em cada frase com sua respectiva posi��o
	Aplica��o salva esse mesmo output num arquivo "search.txt" no mesmo path dos arquivos "entities.txt" e "sentences.txt"
	