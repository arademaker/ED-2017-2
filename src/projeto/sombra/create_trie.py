# Antonio Luís Sombra de Medeiros - 2017

# Programa que gera a trie a partir de um arquivo de entidades
 
#Modo de uso: O programa recebe dois argumentos: arquivo_entidade, arquivo_de_gravação_trie
# Ex de uso: Rode no terminal o comenado: python create_trie.py entities.txt trie.txt
# entities.txt é seu arquivo de entities, na mesma pasta do script
# trie.txt é o arquivo em que será gravada a trie.



import os
import sys
import simplejson as json
import codecs
import io


class BuildTrie():
    def __init__(self, file_path: str):
        self.file_path = file_path
        self.name_finish = "finish"

    def get_parsed_name_list(self) -> list:
        parsed_name_list = list()
        if os.path.exists(self.file_path):
            with codecs.open(self.file_path, encoding='utf-8') as file:
                line_list = file.readlines()
            for line in line_list:
                parsed_name = list(map(lambda x: x, line.strip().replace("  "," ").split(' ')))
                parsed_name[-1] = parsed_name[-1].replace("\n","")
                parsed_name_list.append(parsed_name)
        else:
            print('Path does not exists!')
        return parsed_name_list

    def build_trie(self, trie_dict: dict, name_list: list, layer: int) -> dict:
        if layer == len(name_list):
            trie_dict[self.name_finish] = {}
        if layer >= len(name_list):
            return trie_dict
        if name_list[layer] not in trie_dict:
            trie_dict[name_list[layer]] = dict()
        trie_dict[name_list[layer]] = self.build_trie(trie_dict[name_list[layer]], name_list, layer + 1)
        return trie_dict

    def get_trie(self):
        trie_dict = dict()
        parsed_name_list = self.get_parsed_name_list()
        for name_list in parsed_name_list:
            if name_list != ['']:
                trie_dict = self.build_trie(trie_dict, name_list, 0)
        return trie_dict

    def write_file(self,filename):
        with open(filename, 'w') as file:
            trie = self.get_trie()    
            data = json.dumps(trie, ensure_ascii=False, indent = 4, encoding="utf-8")
            file.write(str(data))

if __name__ == '__main__':
    filename = sys.argv[1]
    output = sys.argv[2]
    BuildTrie(filename).write_file(output)
