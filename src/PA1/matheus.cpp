#include <iostream>
#include <fstream>
#include <stdio.h>
#define n 100000

using namespace std;

int mergee(int v[], int tam1, int u[], int tam2, int w[]){
    int cont = 0, a = 0, b = 0, c = 0;

    while((a < tam1)&&(b < tam2)){
        if(v[a] < u[b]){
            w[c] = v[a];
            c++;
            a++;}
        else{
            w[c] = u[b];
            b++;
            c++;
            cont = cont + tam1 - a;}}

    while(a < tam1){
        w[c] = v[a];
        c++;
        a++;}

    while(b < tam2){
        w[c] = u[b];
        c++;
        b++;}

    return cont;}

int contInversao(int v[], int tam){
    int w[tam];
    int mid, cont1, cont2, cont3, i;

    if(tam > 1){
        mid = tam/2;
        cont1 = contInversao(v, mid);
        cont2 = contInversao(v+mid, tam-mid);
        cont3 = mergee(v, mid, v+mid, tam-mid, w);
        for(i=0;i<tam;i++){
            v[i] = w[i];}
        return (cont1 + cont2 + cont3);}
    else{
        return 0;}}

int main(){
    int v[n];
    ifstream file("IntegerArray.txt");
    int i, tam;
    double value;
    unsigned int cont=0;

    if(!file){
        cout << "Erro ao abrir o arquivo." << endl;
        return 1;}

    i = 0;

    while(!file.eof()){
        file >> value;
        v[i] = value;
        i++;}

    tam = sizeof(v)/sizeof(v[0]);
    cont = contInversao(v, tam);
    cout << "Numero de inversoes: " << cont << endl;

    return 0;}
