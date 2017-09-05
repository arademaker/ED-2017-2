#include <iostream>
#include <stdlib.h>
using namespace std;

class Elem{
public:
    int chave;
    Elem *prox;
    ~Elem(){
        cout << "destrutor "  << chave << endl;
    }
};

class ListaEncadeada{
    Elem *inicio;
    int tam;
public:
    ListaEncadeada(){
        inicio = NULL;
        tam = 0;
    }
    ~ListaEncadeada(){
        while (inicio){
            Elem *aux = inicio->prox;
            delete inicio;
            inicio = aux;
            tam--;
        }
    }
    void inserir(int valor){
        Elem *atual = new (nothrow) Elem;
        if (atual==NULL){
            cout << "ERRO";
            return;
        }

        atual->chave = valor;
        atual->prox = inicio;
        inicio = atual;
        tam++;
    }
    void remover(int valor){
        Elem *atual = inicio, *ant = NULL;

        while (atual && atual->chave !=valor){
            ant = atual;
            atual = atual->prox;
        }

        if (atual){
            if (atual==inicio)
                inicio = inicio->prox;
            else
                ant->prox = atual->prox;
            delete atual;
            tam--;
        }
    }
    void impressao(){
        Elem *atual = inicio;
        while (atual){
            cout << atual->chave << " ";
            atual = atual->prox;
        }
        cout << endl;
    }

    void tamanho(){
        cout << "Tamanho da lista: " << tam << endl;
    }

    void posicao(int k){
        if(k>tam){
            cout << "Posicao invalida" << endl;
        }
        else{
            Elem *aux = inicio;
            for(int i=1;i<k;i++){
                aux = aux->prox;
            }
            cout << "Elemento na posicao " << k << ": " << aux->chave << endl;
        }
    }
};
int main(){
    ListaEncadeada l;

    l.inserir(10);
    l.inserir(7);
    l.inserir(5);
    l.tamanho();

    l.impressao();

    l.posicao(1);
    l.posicao(2);
    l.posicao(3);
    l.posicao(4);

    l.remover(7);
    l.tamanho();
    l.remover(9);
    l.tamanho();

    l.impressao();
}
