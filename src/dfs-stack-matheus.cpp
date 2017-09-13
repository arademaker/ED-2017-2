#include <iostream>
#include <list>
#include <algorithm>
#include <stack>

using namespace std;

class Grafo{
private:
	int V;
	list<int> *adj;
public:
	Grafo(int V);
	void adA(int v1, int v2);
	void dfs(int v);};

Grafo::Grafo(int V){
	this->V = V;
	adj = new list<int>[V];}

void Grafo::adA(int v1, int v2){
	adj[v1].push_back(v2);}

void Grafo::dfs(int v){
	stack<int> pilha;
	bool vis[V];
    for(int i = 0; i < V; i++){
        vis[i] = false;}
	while(true){
		if(!vis[v]){
			cout << "Visitando vertice " << v << "\n";
			vis[v] = true;
			pilha.push(v);}
		bool f = false;
		list<int>::iterator it;
		for(it = adj[v].begin(); it != adj[v].end(); it++){
			if(!vis[*it]){
				f= true;
				break;}}
		if(f){
			v = *it;}
		else{
			pilha.pop();
			if(pilha.empty()){
				break;}
			v = pilha.top();}}}

int main(){
    /*
    Figure 3.2, página 90
    A=0
    B=1
    C=2
    D=3
    E=4
    F=5
    G=6
    H=7
    I=8
    J=9
    K=10
    L=11
    */
	int V = 12;
	Grafo grafo(V);
	grafo.adA(0, 1);
	grafo.adA(0, 2);
	grafo.adA(0, 3);
	grafo.adA(0, 5);
	grafo.adA(1, 4);
	grafo.adA(2, 5);
	grafo.adA(3, 6);
	grafo.adA(3, 7);
	grafo.adA(4, 8);
	grafo.adA(4, 9);
	grafo.adA(6, 7);
	grafo.adA(8, 9);
	grafo.adA(10, 11);
	grafo.adA(1, 0);
	grafo.adA(2, 0);
	grafo.adA(3, 0);
	grafo.adA(5, 0);
	grafo.adA(4, 1);
	grafo.adA(5, 2);
	grafo.adA(6, 3);
	grafo.adA(7, 3);
	grafo.adA(8, 4);
	grafo.adA(9, 4);
	grafo.adA(7, 6);
	grafo.adA(9, 8);
	grafo.adA(11, 10);
	grafo.dfs(10);
	return 0;}
