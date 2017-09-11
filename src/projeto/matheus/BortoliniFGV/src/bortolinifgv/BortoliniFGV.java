package bortolinifgv;

import java.util.*;
import java.io.*;

class TrieNode{
    String content;
    boolean isEnd;   
    LinkedList<TrieNode> childList; 

    public TrieNode(String word){/* Constructor */
        childList = new LinkedList();
        isEnd = false;
        content = word;
    }  

    public TrieNode subNode(String word){
        if(childList!=null)
            for(TrieNode eachChild : childList)
                if(eachChild.content.equals(word))
                    return eachChild;
        return null;
    }
}

class Trie{
    private final TrieNode root;
     
    public Trie(){/* Constructor */
        root = new TrieNode(" "); 
    }
     
    public void insert(String phrase){/* Function to insert phrase */
        if(searchPhrase(phrase) == true) 
            return;        
        TrieNode current = root;     
        for(String word : phrase.split(";")){
            TrieNode child = current.subNode(word);
            if(child != null)
                current = child;
            else{
                 current.childList.add(new TrieNode(word));
                 current = current.subNode(word);
            }
        }
        current.isEnd = true;
    }
    
    public boolean searchPhrase(String phrase){/* Function to search for phrase */
        TrieNode current = root;  
        for(String word : phrase.split(";")){
            if(current.subNode(word) == null)
                return false;
            else
                current = current.subNode(word);
        }      
        return (current.isEnd == true);
    }
    
    public TrieNode searchWord(String word){/* Function to search for word */
        TrieNode current = root;
        return current.subNode(word);
    }
}

class Sentence{ /* List sentences */
    int id;
    String phrase;
    LinkedList<Name> match;
    
    Sentence(int id, String phrase){
        this.id = id;
        this.phrase = phrase;
        match = new LinkedList();
    }
}

class Name{ /* List with all matches */
    String name;
    int nameIndex;

    Name(String name, int nameIndex){
        this.name = name;
        this.nameIndex = nameIndex;
    }
}

public class BortoliniFGV{
    public static void main(String[] args) throws FileNotFoundException, IOException {
        int id = 0, wordIndex, nameLength;
        Trie t = new Trie();
        LinkedList<Sentence> sentences = new LinkedList();
        Sentence s;
        Name n;
        String line, auxNameTrue, auxNameFalse, path;
        String[] auxString;
        TrieNode auxTrieNode;
        boolean auxBoolean;
        Scanner ler = new Scanner(System.in);
        
        System.out.println("Entre com o path dos arquivos:");
        path = ler.nextLine();
        
        BufferedReader br = new BufferedReader(new FileReader(path + "\\entities.txt"));
        File file = new File(path + "\\search.txt");
        BufferedWriter writer = new BufferedWriter(new FileWriter(file));
        
        /* Build Trie */
        while((line = br.readLine()) != null){
            line = line.replace(" ", ";");
            t.insert(line);
        }
        br.close();
        
        /* Open sentences file */
        br = new BufferedReader(new FileReader(path + "\\sentences.txt"));
        
        /* Read all sentences */
        while((line = br.readLine()) != null){
            line = line.replace(" ", ";");
            line = line.replace(",", ";");
            line = line.replace("-", ";");
            line = line.replace("'", ";");
            line = line.replace(".", ";");
            line = line.replace("!", ";");
            line = line.replace("?", ";");
            line = line.replace("(", ";");
            line = line.replace(")", ";");
            line = line.replace(";;", ";");
            line = line.replace(";;", ";");
            line = line.replace(";;", ";");
            
            if((line.substring(line.length()-1,line.length())).equals(";"))
                line = line.substring(0,line.length()- 1);
            
            /* Create a new sentence */
            s = new Sentence(id, line);
            id++;
            
            /* Find names in sentence */
            auxString = line.split(";");
            for(wordIndex=0;wordIndex<auxString.length;wordIndex++){
                auxTrieNode = t.searchWord(auxString[wordIndex]);
                if(auxTrieNode != null){
                    auxBoolean = false;
                    auxNameTrue = "";
                    auxNameFalse = "";
                    nameLength = 0;
                    
                    do{
                        if(auxTrieNode.isEnd){
                            if(auxNameFalse.equals("")){
                                if(auxNameTrue.equals(""))
                                    auxNameTrue = auxTrieNode.content;
                                else
                                    auxNameTrue = auxNameTrue + " " + auxTrieNode.content;
                            }else{
                                auxNameTrue = auxNameTrue + " " + auxNameFalse + " " + auxTrieNode.content;
                            }
                            auxBoolean = true;
                        }else{
                            if(auxNameFalse.equals(""))
                                auxNameFalse = auxTrieNode.content;
                            else
                                auxNameFalse = auxNameFalse + " " + auxTrieNode.content;
                        }
                        nameLength++;
                        if(wordIndex+nameLength<(auxString.length))
                            auxTrieNode = auxTrieNode.subNode(auxString[wordIndex+nameLength]);
                    }while((auxTrieNode!=null)&&(wordIndex+nameLength<auxString.length));
                    
                    if(auxBoolean){
                        n = new Name(auxNameTrue, wordIndex);
                        wordIndex = wordIndex + nameLength - 1;
                        (s.match).add(n);
                    }
                }
            }
            
            /* Add sentence to senteces*/
            sentences.add(s);
        }
        
        /* Show senteces and matches */
        for(Sentence a : sentences){
            System.out.println(String.valueOf(a.id) + " " + a.phrase);
            writer.write(String.valueOf(a.id) + " " + a.phrase);
            for(Name b : a.match){
                System.out.println("\t" + String.valueOf(b.nameIndex) + " " + b.name);
                writer.newLine();
                writer.write("\t" + String.valueOf(b.nameIndex) + " " + b.name);
            }
            writer.newLine();
        }
    writer.flush();
    writer.close();
    }
}
