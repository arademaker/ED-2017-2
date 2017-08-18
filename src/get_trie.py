import os
import json

class BuildTrie():
    def __init__(self, file_path: str):
        self.file_path = file_path

    def get_parsed_name_list(self) -> list:
        parsed_name_list = list()
        if os.path.exists(self.file_path):
            with open(self.file_path, 'r') as file:
                line_list = file.readlines()
            for line in line_list:
                parsed_name = list(map(lambda x: x.lower(), line.split(' ')))
                parsed_name[-1] = parsed_name[-1][:-1]
                parsed_name_list.append(parsed_name)
        else:
            print('Path does not exists!')
        return parsed_name_list

    def build_trie(self, trie_dict: dict, name_list: list, layer: int) -> dict:
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

    def write_file(self):
        write_path = os.path.join(os.path.dirname(self.file_path), 'trie_result.txt')
        with open(write_path, 'w') as file:
            file.write(json.dumps(self.get_trie()))

if __name__ == '__main__':
    file_path = input('Entre com o path do arquivo com os nomes: ')
    BuildTrie(file_path).write_file()
