#include <iostream>
#include <list>
#include <algorithm>
#include <stack>
#include <vector>

using namespace std;

class Grafo{
private:
    int V;
    list<int> *adj;
    vector<int> in;
    void al(vector<int>& res, bool vis[]);
 public:
    Grafo(int V);
    void adA(int v1, int v2);
    void aL();};

Grafo::Grafo(int V){
    this->V = V;
    adj = new list<int>[V];
    for(int i = 0; i < V; i++){
        in.push_back(0);}}

void Grafo::adA(int v1, int v2){
    adj[v1].push_back(v2);
    in[v2]++;}

void Grafo::al(vector<int>& res, bool vis[]){
    bool flag = false;
    for(int i = 0; i < V; i++){
        if(in[i] == 0 && !vis[i]){
            list<int>:: iterator j;
            for(j = adj[i].begin(); j != adj[i].end(); j++){
                in[*j]--;}
            res.push_back(i);
            vis[i] = true;
            al(res, vis);
            vis[i] = false;
            res.erase(res.end() - 1);
            for(j = adj[i].begin(); j != adj[i].end(); j++){
                in[*j]++;}
            flag = true;}}
    if (!flag){
        for(int i = 0; i < res.size(); i++){
            cout << res[i] << " ";}
        cout << endl;}}

void Grafo::aL(){
    bool *vis = new bool[V];
    for(int i = 0; i < V; i++){
        vis[i] = false;}
    vector<int> res;
    al(res, vis);}

int main(){
    /*
    Figure 3.8, página 96
    A=0
    B=1
    C=2
    D=3
    E=4
    F=5
    */
    Grafo grafo(6);
    grafo.adA(0, 2);
    grafo.adA(1, 0);
    grafo.adA(1, 3);
    grafo.adA(3, 2);
    grafo.adA(2, 4);
    grafo.adA(2, 5);
    cout << "Todas as linearizacoes possiveis:\n";
    grafo.aL();
    return 0;}
