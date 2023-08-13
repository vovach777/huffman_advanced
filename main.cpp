#include <algorithm>
#include <iostream>
#include <limits>
#include <ostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <vector>
#include <bitset>
#include <cassert>
#include <cstdint>
#include <iomanip>
#include <cmath>
#include <stdexcept>
#include "bitstream.hpp"

using namespace std;



// A Tree node
template<typename T = char>
struct Node {
    T ch;
    int freq;
    Node *left, *right;
    // Function to allocate a new tree node
    Node(T ch, int freq, Node* left, Node* right)
        : ch(ch), freq(freq), left(left), right(right) {}
    bool operator<(const Node& other) const { return freq < other.freq; }
};

// Comparison object to be used to order the heap


template<typename T = char>
struct Node_comp {
    
    bool operator()(Node<T>* l, Node<T>* r) {
        // highest priority item has lowest frequency
        return l->freq > r->freq;
    }
};



inline ostream& operator<<(ostream& o, vector<int> const& a) {
    for (auto v : a) o << v << " ";
    return o;
}

// Function to convert a coefficient into a "category/index" pair in JPEG format
int symbol_to_catindex(int coeff)
{
    // If the coefficient is negative, we change its sign to positive
    uint32_t positive = (coeff < 0) ? -coeff : coeff;
    if (positive == 0)
        return 0;
    
    // Calculate the category of the coefficient as the number of bits needed to represent its absolute value
    int cat = 32 - __builtin_clz(positive);

    // Calculate the minimum and maximum value in the given category
    int minValue = (1 << (cat - 1));
    int maxValue = (1 << cat) - 1;

    // Calculate the index of the coefficient within the given category
    auto index = (coeff < 0) ? maxValue + coeff : coeff - minValue + (maxValue - minValue + 1);
    
    // Return the "category/index" pair in JPEG format
    return cat | index << 4;
}

// Function to recover the coefficient from a "category/index" pair in JPEG format
int catindex_to_symbol(int pair) {
    if (pair == 0)
        return 0;
    
    // Extract the category and index from the "category/index" pair
    int cat = pair & 0xf;
    int index = pair >> 4;
    
    // Calculate the minimum and maximum value in the given category
    int minValue = (1 << (cat - 1));
    int maxValue = (1 << cat) - 1;    
    
    // Calculate the index of the positive value within the given category
    auto positive_index = 1 << (cat - 1);
    
    // Recover the original coefficient
    return  (index < positive_index) ? -maxValue + index : index - positive_index + minValue;
}

// Function to recover the coefficient from a "category/index" pair in JPEG format
int catindex_to_symbol(int cat, int index ) {
    if (cat == 0)
        return 0;
   
    // Calculate the minimum and maximum value in the given category
    int minValue = (1 << (cat - 1));
    int maxValue = (1 << cat) - 1;    
    
    // Calculate the index of the positive value within the given category
    auto positive_index = 1 << (cat - 1);
    
    // Recover the original coefficient
    return  (index < positive_index) ? -maxValue + index : index - positive_index + minValue;
}



// A Tree
template<typename T=char,typename Node=Node<T>>
class Tree {
    Node* root;
    vector<Node> pool;
    using BitCountType = uint8_t;
    using HuffmanCodeType = uint32_t;
    unordered_map<T, pair<BitCountType,HuffmanCodeType>> huffmanCode;
    vector<vector<T>> DHT;

    // traverse the Huffman Tree and store Huffman Codes
    // in a map.
    void fillDHT(Node* node, int len) {
        if (node == nullptr) return;

        // found a leaf node
        if (!node->left && !node->right) {
            assert(len<=16 && len > 0);
            if (DHT.size() < len)
                DHT.resize(len);
            DHT[len-1].push_back(node->ch);
        }
    
        fillDHT(node->left, len+1);
        fillDHT(node->right, len+1);
    }

   public:
    void encodeHuffmanTree(BitWriter & dest) {
        if (DHT.size() == 0) 
            throw std::domain_error("No DHT found!"); 

        vector<bool> str;
        for (int i = 0; i < DHT.size(); i++) {         
            dest.writeBits(8, DHT[i].size());
            for (auto symbol : DHT[i]) {
                auto catindex = symbol_to_catindex(symbol);
                dest.writeBits(4,catindex & 0xf);
                dest.writeBits(catindex & 0xf, catindex >> 4);
            }
        }
        //rest of DHT (16 bytes)
        for (int i=DHT.size(); i<16; i++) {
            dest.writeBits(8,0);
        }
    }

void decodeHuffmanTree(BitReader & src)
{
        //decode huffman table
        DHT.clear;
        DHT.resize(16);

        size_t poolSize = 1;
        for (int i = 0; i < 16; i++ )
        {        
            DHT[i].resize(src.readBits(8));
            for ( auto & value : DHT[i] ) 
            {
                auto cat = src.readBits(4);
                auto index = cat==0 ? 0 : src.readBits(cat);
                value = catindex_to_symbol(cat, index);
            }
        }  
        create_lockup_table();//need for encoder
        buildHuffmanTree();//need for decoder
        
}
void create_lockup_table() {
  std::cout << "generation huffman codes:" << endl;
  huffmanCode.clear();
    HuffmanCodeType code=0;
    for (int bits = 0; bits < DHT.size(); ++bits) {
      
        for (int i=0; i < DHT[bits].size(); i++)
        {
            // HuffmanCodeType rev_code = 0;
            // for (int n = bits-1; n >= 0; n--){
            //     rev_code |= (code >> n) & 1;
            //     rev_code <<= 1;
            // }

            huffmanCode[ DHT[bits][i] ] = make_pair(bits+1,code);
            code++;
        }
        code <<= 1;
    }
}
    // Builds Huffman Tree 
template <typename Iterator>
auto buildHuffmanTree(Iterator begin, Iterator end) -> decltype(std::enable_if_t<std::is_same_v<typename std::iterator_traits<Iterator>::value_type, T>, Iterator>(), void())
{
        // count frequency of appearance of each character
        // and store it in a map
        unordered_map<T, int> freq;
        for (auto  it=begin; it != end; ++it) {
            freq[*it]++;
        }

        pool.clear();
        pool.reserve(freq.size()*2);
        
        // Create a priority queue to store live nodes of
        // Huffman tree;

        priority_queue<Node*, vector<Node*>, Node_comp<T>> pq;

        // Create a leaf node for each character and add it
        // to the priority queue.
        for (auto pair : freq) {
            pq.push(
                &pool.emplace_back(pair.first, pair.second, nullptr, nullptr));
        }

        // do till there is more than one node in the queue
        while (pq.size() != 1) {
            // Remove the two nodes of highest priority
            // (lowest frequency) from the queue
            Node* left = pq.top();
            pq.pop();
            Node* right = pq.top();
            pq.pop();

            // Create a new internal node with these two nodes
            // as children and with frequency equal to the sum
            // of the two nodes' frequencies. Add the new node
            // to the priority queue.
            int sum = left->freq + right->freq;
            pq.push(&pool.emplace_back('\0', sum, left, right));
        }

        // root stores pointer to root of Huffman Tree
        root = pq.top();

        // traverse the Huffman Tree and store (not Huffman Codes) bitlength of each symbol
        // in a map. Also prints them
        DHT.clear();
        DHT.reserve(16);
        fillDHT(root, 0);

        //huffman table not needed any more - lockup table to be regenerated from DHT
        root = nullptr;
        pool.clear();
        create_lockup_table();
        

// const int MAX_BITS = 3;
    // std::vector<int> bl_count = {0, 1, 1, 3};
    //std::vector<int> next_code(MAX_BITS + 1);

        cout << "Huffman Codes are :\n" << '\n';
        for (const auto& pair : huffmanCode) {
            std::bitset<std::numeric_limits<HuffmanCodeType>::digits> bs(pair.second.second);
            auto s = bs.to_string();
            cout << setw(4) << pair.first << " " << s.substr(s.size()-pair.second.first, pair.second.first) << " (" << (int)pair.second.first << "," << pair.second.second << ")" << endl;
        }

        buildHuffmanTree();
    }
    //from lockuptable
    void buildHuffmanTree() {
        if (huffmanCode.size() == 0) {
            root = nullptr;
            return;
        }
          pool.clear();
          pool.reserve(huffmanCode.size()*2);
          root = &pool.emplace_back('\0',1,nullptr,nullptr);
      
        // for (const auto & symbols : DHT)
        // for (const auto symbol : symbols) {
        //     const auto& code = huffmanCode[symbol]; 
        //     cout << symbol << " " << code<< '\n';
            for (const auto& pair : huffmanCode) {
            auto symbol = pair.first;
            //cerr << endl << "(" << symbol << ") : ";
            auto& code  = pair.second;
            auto n = root;
            for (int i = code.first-1; i>=0; --i) {
                auto v = (code.second >> i) & 1;
                //cerr << v;
                if (v==0) {
                    if (!n->left) {
                        n->left = &pool.emplace_back('\0',1,nullptr,nullptr);
                    }
                    assert(n->freq == 1);
                    n = n->left;
                    
                } else {
                    if (!n->right) {
                        n->right = &pool.emplace_back('\0',1,nullptr,nullptr);
                    }
                    assert(n->freq == 1);
                    n = n->right;
                }
            }
            assert(n->left == nullptr && n->right == nullptr && n->freq==1);
            n->ch = symbol;
            n->freq = 0;
            
        }
        //cerr << endl;
    }

    template <typename Iterator>
    void encode(BitWriter & dest, Iterator begin, Iterator end) { 
        for (auto it=begin; it != end; ++it) {
            encode(dest,*it);
        }
    }
    inline void encode(BitWriter & dest, T value) {
        auto [bit_count, code] = huffmanCode.at(value);
        // while (bit_count--) {
        //     dest.writeBit(code & 1);
        //     code >>= 1;
        // }
        dest.writeBits(bit_count,code);       
    }

    // traverse the Huffman Tree and decode the encoded string
    T decode(BitReader & src) {
        if (root == nullptr){
            throw std::domain_error("No the huffman-tree found!");
        }
        
        auto n = root;

        for (;;) {
            // if (src.bit_left() == 0)
            //     return root->ch;

            if (src.readBit() == false) {
                n = n->left;
            }
            else {
                n = n->right;
            }
            // found a leaf node
            if (n->freq==0) {
                //cerr << "(" << n->ch << ")";
                return n->ch;
            }
        }
    }
};

// Huffman coding algorithm
int main() {
    //#define TEXT
    #ifdef  TEXT
    Tree tree;
    string data = "Huffman coding is a data compression algorithm.";
    string decoded_data;
    #else
    Tree<int> tree;
    vector<int> data;
    vector<int> decoded_data;
    for (int i=1; i<256; ++i)
        data.push_back(sin(i/256.0*6*2)*8);
    #endif
    
    tree.buildHuffmanTree(data.begin(), data.end());

    cout << "\nOriginal string was :\n" << data << '\n';

    BitWriter bw;
    
    tree.encode(bw, data.begin(), data.end());
    bw.flush();

    // cout << endl
    //      << "Huffman Table is :" << endl
    //      << tree.encodeHuffmanTree() << endl;
    // print encoded string
    cout << "\nEncoded string is :\n" << bw <<  "  (" << bw.size_in_bits() << " bits)" << endl;


    // decode the encoded string
    cout << "\nDecoded string is: \n";

    BitReader br(bw.data(),bw.size(), bw.size_in_bits());
    while (br.bit_left())
       decoded_data.push_back( tree.decode(br) );
  
    if (decoded_data.size() != data.size())
        cerr << "decode fail!!!" << endl;
    else
    for (int i=0; i< data.size(); ++i)
       {
         if (data[i] != decoded_data[i]) {
            cerr << "decode fail!!!" << endl;
            break;
         }
       }
    cout << decoded_data;
    


    return 0;
}