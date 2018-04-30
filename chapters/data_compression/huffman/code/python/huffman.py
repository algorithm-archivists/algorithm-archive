# Huffman Encoding
# Python 2.7+
# Submitted by Matthew Giallourakis 

from collections import Counter

# constructs the tree
def build_tree(message):
    
    # get sorted list of character,frequency pairs
    frequencies = Counter(message)
    trees = frequencies.most_common()

    # while there is more than one tree
    while len(trees) > 1:
        
        # pop off the two trees of least weight from the trees list
        tree_left,weight_left = trees.pop()
        tree_right,weight_right = trees.pop()
        
        # combine the nodes and add back to the nodes list
        new_tree = [tree_left,tree_right]
        new_weight = weight_left+weight_right
        trees.append((new_tree,new_weight))

        # sort the trees list by weight
        trees = sorted(trees, key=lambda n: n[1], reverse=True)

    tree = trees[0][0]
    return tree

# constructs the mapping with recursion
def build_mapping(tree,code=''):

    results = []

    # split the tree
    left_tree,right_tree = tree

    # if the left node has children, find the mapping of those children
    # else pair the character with the current code + 0 
    if type(left_tree) is list:
        results += build_mapping(left_tree,code+'0')
    else:
        results.append((left_tree,code+'0'))

    # if the right node has children, find the mapping of those children
    # else pair the character with the current code + 1 
    if type(right_tree) is list:
        results += build_mapping(right_tree,code+'1')
    else:
        results.append((right_tree,code+'1'))

    return results

# encodes the message
def encode(mapping,message):
    
    encoding = ""

    # build a char -> code dictionary
    forward_dict = dict(mapping)

    # replace each character with its code
    for char in message:
        encoding += forward_dict[char]
    
    return encoding

# decodes a message
def decode(mapping,encoding):
    
    message = ""
    key = ""

    # build a code -> char dictionary
    inverse_dict = dict([(v,k) for k,v in mapping])

    # for each bit in the encoding
    # if the bit is in the dictionary, replace the bit with the paired character
    # else look at the bit and the following bits together until a match occurs
    # move to the next bit not yet looked at
    for index,bit in enumerate(encoding):
        key += bit
        if key in inverse_dict:
            message += inverse_dict[key]
            key = ""
    
    return message

def main():

    # test example
    message = "bibbity_bobbity"
    tree = build_tree(message)
    mapping = build_mapping(tree)
    encoding = encode(mapping,message)
    decoding = decode(mapping,encoding)

    print('message: '+message)
    print('tree: '+str(tree))
    print('mapping: '+str(mapping))
    print('encoding: '+encoding)
    print('decoding: '+decoding)

    # prints the following:
    #
    #  message: bibbity_bobbity
    #  tree: ['b', [[['_', 'o'], 'y'], ['t', 'i']]]
    #  mapping: [('b', '0'), ('_', '1000'), ('o', '1001'),
    #            ('y', '101'), ('t', '110'), ('i', '111')]
    #  encoding: 01110011111010110000100100111110101
    #  decoding: bibbity_bobbity
    
if __name__ == '__main__':
    main()
