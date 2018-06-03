#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <cctype>
#include <cstddef>
#include <limits>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <iostream>

using std::begin;
using std::end;

namespace huffman {

[[noreturn]] inline void unreachable() {
  std::cerr << "this should never happen\n";
  std::abort();
}

// --- interface ---
class codebook {
  struct node {
    int frequency;

    node(int freq) : frequency(freq) {}
    virtual ~node() = 0;
  };

  // never null
  using node_ptr = std::unique_ptr<node const>;
  using bitstring = std::vector<bool>;

  // this is a flatmap between char and a bitstring
  // there should only ever be one character with a given
  // value at any time.
  using encoder_t = std::vector<std::pair<char, bitstring>>;

  struct leaf final : node {
    char key;

    leaf(int freq, char key) : node(freq), key(key) {}
  };

  struct branch final : node {
    node_ptr lhs;
    node_ptr rhs;

    branch(node_ptr lhs, node_ptr rhs)
        : node(lhs->frequency + rhs->frequency), lhs(std::move(lhs)),
          rhs(std::move(rhs)) {}
  };

  // this allows us to share [codebook]s among encoded strings
  struct data {
    node_ptr decoder;
    encoder_t encoder;
  };
  std::shared_ptr<data const> underlying_;

public:
  template <typename Iter>
  codebook(Iter const first, Iter const last);

  template <typename Iter>
  std::vector<bool> encode(Iter first, Iter last) const;

  template <typename Iter>
  std::string decode(Iter first, Iter last) const;
};

struct encoded_string {
  codebook codes;
  std::vector<bool> string;

  explicit encoded_string(std::string const& s)
      : codes(begin(s), end(s)), string(codes.encode(begin(s), end(s))) {}

  encoded_string(codebook codes, std::string const& s)
      : codes(codes), string(codes.encode(begin(s), end(s))) {}

  std::string decoded() const {
    return codes.decode(begin(string), end(string));
  }
};

// --- implementation ---
inline codebook::node::~node() {}

inline std::vector<bool> with_new_bit(std::vector<bool> bits, bool b) {
  bits.push_back(b);
  return bits;
}

template <typename Iter>
codebook::codebook(Iter const first, Iter const last) {
  struct helper {
    static node_ptr make_decoder(Iter const first, Iter const last) {
      // in this part of the function, we build up a frequency list
      // sorted by frequency, descending
      auto freq = std::vector<leaf>();

      std::for_each(first, last, [&freq](char c) {
        auto const it = std::find_if(
            begin(freq), end(freq), [c](auto const& p) { return p.key == c; });
        if (it != end(freq)) {
          // it's already in the list
          it->frequency += 1;
        } else {
          // it's not already in the list
          freq.emplace_back(1, c);
        }
      });

      if (freq.empty()) {
        throw std::invalid_argument("attempted to codebook an empty range");
      }

      std::sort(begin(freq), end(freq), [](auto const& lhs, auto const& rhs) {
        return lhs.frequency > rhs.frequency;
      });

      auto ret = std::vector<std::unique_ptr<node>>();
      std::transform(
          begin(freq), end(freq), std::back_inserter(ret), [](auto const l) {
            return std::make_unique<leaf>(l);
          });

      while (ret.size() > 1) {
        auto rhs = std::move(ret.back());
        ret.pop_back();
        auto lhs = std::move(ret.back());
        ret.pop_back();

        auto new_node =
            std::make_unique<branch>(std::move(lhs), std::move(rhs));
        auto const new_freq = new_node->frequency;

        // look for the first element with a smaller frequency
        auto const it =
            std::find_if(begin(ret), end(ret), [new_freq](auto const& n) {
              return n->frequency < new_freq;
            });
        // and insert the new_node before that element
        ret.insert(it, std::move(new_node));
        // in this way, we keep the list sorted by frequency
      }

      return std::move(ret.back());
    }

    static void
    encoder_rec(node const* cur, std::vector<bool> bits, encoder_t& out) {
      if (auto l = dynamic_cast<leaf const*>(cur)) {
        out.push_back(std::make_pair(l->key, std::move(bits)));
      } else if (auto b = dynamic_cast<branch const*>(cur)) {
        encoder_rec(b->lhs.get(), with_new_bit(bits, 0), out);
        encoder_rec(b->rhs.get(), with_new_bit(std::move(bits), 1), out);
      } else {
        unreachable();
      }
    }

    static encoder_t make_encoder(node const& decoder) {
      auto ret = encoder_t();

      encoder_rec(&decoder, std::vector<bool>(), ret);

      return ret;
    }
  };

  auto decoder = helper::make_decoder(first, last);
  auto encoder = helper::make_encoder(*decoder);
  underlying_ = std::make_shared<data const>(
      data{std::move(decoder), std::move(encoder)});
}

template <typename Iter>
std::vector<bool> codebook::encode(Iter const first, Iter const last) const {
  std::vector<bool> ret;

  auto& encoder = underlying_->encoder;
  std::for_each(first, last, [&ret, &encoder](char c) {
    auto const it =
        std::find_if(begin(encoder), end(encoder), [c](auto const& p) {
          return p.first == c;
        });
    if (it != end(encoder)) {
      auto const& code = it->second;
      std::copy(begin(code), end(code), std::back_inserter(ret));
    } else {
      throw std::invalid_argument(
          "The range has a character which was not in the huffman set");
    }
  });

  return ret;
}

template <typename Iter>
std::string codebook::decode(Iter const first, Iter const last) const {
  std::string ret;

  node const* const top = underlying_->decoder.get();

  // returns a pair:
  // the second member is the decoded character
  // the first member is the place we've gotten to in the range
  // i.e., if [0] is an 'a', and we have
  // [it, last) = { 0, 1, 1, 0 }
  // we return (it', 'a') such that
  // [it', last) = { 1, 1, 0 }
  auto decode_single =
      [top](Iter it, Iter const last) -> std::pair<Iter, char> {
    node const* current_node = top;

    for (; it != last; ++it) {
      if (auto l = dynamic_cast<leaf const*>(current_node)) {
        return std::make_pair(it, l->key);
      } else if (auto b = dynamic_cast<branch const*>(current_node)) {
        if (*it) {
          current_node = b->rhs.get();
        } else {
          current_node = b->lhs.get();
        }
      } else {
        unreachable();
      }
    }

    if (auto l = dynamic_cast<leaf const*>(current_node)) {
      return std::make_pair(last, l->key);
    } else {
      throw std::invalid_argument(
          "The range was not encoded with this huffman set");
    }
  };

  for (auto it = first; it != last;) {
    auto p = decode_single(it, last);
    it = p.first;
    ret.push_back(p.second);
  }

  return ret;
}

} // namespace huffman

int main() {
  std::string to_be_encoded = R"(bibbity bobbity)";

  auto encoded = huffman::encoded_string(to_be_encoded);

  std::cout << "Encoded, the string looks like: ";
  for (bool b : encoded.string) {
    std::cout << b;
  }
  std::cout << "\nand decoded, the string looks like: " << encoded.decoded();
  std::cout << "\n\nAs opposed to the original, which is "
            << to_be_encoded.size() * 8 << " bits, the encoded has size "
            << encoded.string.size() << ".\n";
}
