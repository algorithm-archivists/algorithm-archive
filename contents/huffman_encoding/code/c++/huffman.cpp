#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

struct node {
  node(int a_weight, char a_ch) : ch(a_ch), weight(a_weight) {}
  node(std::unique_ptr<node>&& a_left, std::unique_ptr<node>&& a_right)
      : left(std::move(a_left)), right(std::move(a_right)),
        weight(left->weight + right->weight) {
    left->parent = this;
    left->branch = branch::left;
    right->parent = this;
    right->branch = branch::right;
  }

  // leaf fields
  char ch = '\0';

  // branch fields
  std::unique_ptr<node> left;
  std::unique_ptr<node> right;

  // common fields
  node* parent = nullptr;
  enum class branch { left, right };
  branch branch = branch::left;
  int weight = 0;
};

struct codebook {
  // maps characters to leafs in the tree
  std::unordered_map<char, node*> char_map;

  // root of the tree
  std::unique_ptr<node> root;
};

std::vector<bool> encode(const std::string& string_to_encode, codebook& cb) {
  // first pass
  // calculate weights
  std::unordered_map<char, int> weights;
  std::vector<std::unique_ptr<node>> orphans;
  for (char ch : string_to_encode) {
    ++weights[ch];
  }

  // build tree
  for (auto&& w : weights) {
    orphans.push_back(std::make_unique<node>(w.second, w.first));
    cb.char_map[orphans.back()->ch] = orphans.back().get();
  }
  // make parents for orphans until only one root orphan is remained
  while (orphans.size() > 1) {
    std::sort(
        std::begin(orphans),
        std::end(orphans),
        [](const std::unique_ptr<node>& a, const std::unique_ptr<node>& b) {
          return a->weight > b->weight;
        });
    auto right = std::move(orphans.back());
    orphans.pop_back();
    auto left = std::move(orphans.back());
    orphans.pop_back();
    orphans.push_back(
        std::make_unique<node>(std::move(left), std::move(right)));
  }

  // move last orphan to codebook
  assert(orphans.size() == 1);
  cb.root = std::move(orphans.front());

  // second pass
  // encoding
  std::vector<bool> res;
  for (auto ch : string_to_encode) {
    auto node = cb.char_map[ch];
    std::vector<bool> compressed_char;
    while (node->parent != nullptr) {
      compressed_char.push_back(node->branch != node::branch::left);
      node = node->parent;
    }
    res.insert(std::end(res), compressed_char.rbegin(), compressed_char.rend());
  }
  return res;
}

std::string decode(const codebook& cb, const std::vector<bool>& compressed_bit_stream) {
  std::string res;
  node* node = cb.root.get();
  for (auto bit : compressed_bit_stream) {
    if (!bit)
      node = node->left.get();
    else
      node = node->right.get();
    if (node->right == nullptr) {
      res.push_back(node->ch);
      node = cb.root.get();
    }
  }
  return res;
}

int main() {
  std::string to_be_encoded = "bibbity bobbity";

  codebook cb;

  auto encoded = encode(to_be_encoded, cb);

  std::cout << "Encoded, the string looks like: ";
  for (bool b : encoded) {
    std::cout << b;
  }

  std::cout << "\nand decoded, the string looks like: "
            << decode(cb, encoded);
  std::cout << "\n\nAs opposed to the original, which is "
            << to_be_encoded.size() * 8 << " bits, the encoded has size "
            << encoded.size() << ".\n";
}
