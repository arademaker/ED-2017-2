
# Solucao baseada no pseudo-codigo apresentado em:
# http://en.wikipedia.org/wiki/Stable_marriage_problem

stable.match <- function(hprefs, mprefs, debug=FALSE){
  proposed <- matrix(rep(FALSE,25), ncol = 5, dimnames = list(Homens = 1:5, Mulheres = 1:5))
  alloc <- matrix(rep(FALSE,25), ncol = 5, dimnames = list(Homens = 1:5, Mulheres = 1:5))

  # os nomes das linhas e colunas irao ser usados para indicar os
  # indices para selecao de elementos das matrizes proposed e alloc,
  # por isso precisam ser renomeados.
  dimnames(hprefs) <- lapply(dimnames(hprefs), function(x) substr(x,2,2))
  dimnames(mprefs) <- lapply(dimnames(mprefs), function(x) substr(x,2,2))

  # Explicacao sobre o argumento "..." na pagina 55 do livro
  # "Introducao a programacao em R"
  mycat <- function(...) {
    if(debug)
      cat(...)
  }

  husband <- function(woman){
    x <- names(which(alloc[,woman]))
    if(length(x) == 0)
      return(0)
    else
      return(x)
  }

  # O interessante aqui é o uso do rbind para criarmos uma matriz com
  # as preferencias e propostas feitas por um dado homen.
  has.options <- function(man){
    options <- rbind(hprefs[man,], proposed[man,])
    options[1,options[2,] == 0]
  }

  free.men <- function(){
    names(which(!apply(alloc,1, any)))
  }

  while( length(free.men()) > 0 && length(has.options(free.men()[1])) > 0 ){
    man <- free.men()[1]
    woman <- names(which.max(has.options(man)))
    mycat("     try: ", c(man, woman), "\n")
    if ( husband(woman) == 0 ) {
      alloc[man, woman] <- TRUE
      mycat("  maried: ", c(man,woman), "\n")
    } else {
      if( mprefs[woman,man] > mprefs[woman,husband(woman)] ) {
        mycat("Divorced: ", c(husband(woman),woman), "\n")
        mycat("  Maried: ", c(man,woman), "\n")
        alloc[husband(woman), woman] <- FALSE
        alloc[man, woman] <- TRUE
      }
    }
    proposed[man, woman] <- TRUE
    mycat("proposed: ", c(man, woman), "\n")
  }
  # Em rodadas armazenamos o número de elementos na matriz proposed
  # com TRUE, indicando a quantidade de vezes que o corpo do while foi
  # executado.
  retorno <- list(casamentos = alloc, propostas = proposed, rodadas = length(which(proposed)))
  mycat("--> rodadas: ", retorno$rodadas, "\n")
  return(retorno)
}


# Testando a funcao

for(z in 1:15){
  a <- matrix(sample(1:100,25, replace=FALSE), ncol = 5,
              dimnames = list(Homens = paste("H",1:5, sep=""), Mulheres = paste("M",1:5, sep="")))
  b <- matrix(sample(1:100,25, replace=FALSE), ncol = 5,
              dimnames = list(Mulheres = paste("M",1:5, sep=""), Homens = paste("H",1:5, sep="")))

  # debug=TRUE imprimi as mudancas de valores
  x <- stable.match(a,b, debug=FALSE)

  # verificando se nao existe homem solteiro ou poligamia
  print( all(dim(which(x$casamentos, arr.ind=TRUE)) == c(5,2)) )
}
