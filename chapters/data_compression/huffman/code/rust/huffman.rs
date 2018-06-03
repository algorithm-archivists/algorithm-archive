extern crate itertools;

use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::{BinaryHeap, HashMap};

use itertools::Itertools;

#[derive(Debug)]
enum HuffmanTree {
    Branch {
        count: i32,
        left: Box<HuffmanTree>,
        right: Box<HuffmanTree>,
    },
    Leaf {
        count: i32,
        value: char,
    },
}

impl PartialEq for HuffmanTree {
    fn eq(&self, other: &Self) -> bool {
        self.count() == other.count()
    }
}

impl Eq for HuffmanTree {}

impl PartialOrd for HuffmanTree {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.count().partial_cmp(&self.count())
    }
}

impl Ord for HuffmanTree {
    fn cmp(&self, other: &Self) -> Ordering {
        other.count().cmp(&self.count())
    }
}

#[derive(Debug)]
struct Codebook {
    codebook: HashMap<char, String>,
    tree: HuffmanTree,
}

impl HuffmanTree {
    pub fn from(input: &str) -> Self {
        let counts = input.chars().fold(HashMap::new(), |mut map, c| {
            *map.entry(c).or_insert(0) += 1;
            map
        });
        let mut queue = counts
            .iter()
            .map(|(&value, &count)| HuffmanTree::Leaf { value, count })
            .collect::<BinaryHeap<HuffmanTree>>();

        while queue.len() > 1 {
            let left = queue.pop().unwrap();
            let right = queue.pop().unwrap();
            queue.push(HuffmanTree::Branch {
                count: left.count() + right.count(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        queue.pop().expect("The Huffman tree has to have a root")
    }

    pub fn count(&self) -> i32 {
        match *self {
            HuffmanTree::Branch { count, .. } => count,
            HuffmanTree::Leaf { count, .. } => count,
        }
    }

    pub fn make_codebook(self) -> Codebook {
        let mut codebook = HashMap::new();
        self.dfs(String::from(""), &mut codebook);
        Codebook {
            codebook,
            tree: self,
        }
    }

    pub fn decode(&self, input: &str) -> String {
        let mut result = String::from("");
        let mut start = 0;
        while !input[start..].is_empty() {
            start += self.decode_dfs(&input[start..], &mut result);
        }
        result
    }

    fn decode_dfs(&self, input: &str, result: &mut String) -> usize {
        let current = input.chars().next();
        match *self {
            HuffmanTree::Branch { ref left, .. } if current == Some('0') => {
                1 + left.decode_dfs(&input[1..], result)
            }
            HuffmanTree::Branch { ref right, .. } if current == Some('1') => {
                1 + right.decode_dfs(&input[1..], result)
            }
            HuffmanTree::Leaf { value, .. } => {
                result.push(value);
                0
            }
            _ => panic!("Unexpected end of input"),
        }
    }

    fn dfs(&self, code: String, codebook: &mut HashMap<char, String>) {
        match *self {
            HuffmanTree::Branch {
                ref left,
                ref right,
                ..
            } => {
                left.dfs(code.clone() + "0", codebook);
                right.dfs(code.clone() + "1", codebook);
            }
            HuffmanTree::Leaf { value, .. } => {
                codebook.insert(value, code);
            }
        }
    }
}

impl Codebook {
    fn encode(&self, input: &str) -> String {
        input.chars().map(|c| &self.codebook[&c]).join("")
    }

    fn decode(&self, input: &str) -> String {
        self.tree.decode(input)
    }
}

fn main() {
    let input = "bibbity bobbity";

    let tree = HuffmanTree::from(input);
    let codebook = tree.make_codebook();
    let encoded = codebook.encode(input);
    let decoded = codebook.decode(&encoded);

    // Uncomment this line if you want to see the codebook/tree
    // println!("{:#?}", codebook);
    println!("{}", encoded);
    println!("{}", decoded);
}
