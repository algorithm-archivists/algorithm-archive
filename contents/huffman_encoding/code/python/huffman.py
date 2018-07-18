# Huffman Encoding
# Python 2.7+
# Submitted by Matthew Giallourakis

from collections import Counter

# constructs the tree
def build_huffman_tree(message):

    # get sorted list of character and frequency pairs
    frequencies = Counter(message)
    trees = frequencies.most_common()

    # while there is more than one tree
    while len(trees) > 1:

        # pop off the two trees of least weight from the trees list
        tree_left,weight_left = trees.pop()
        tree_right,weight_right = trees.pop()

        # combine the nodes and add back to the nodes list
        new_tree = [tree_left, tree_right]
        new_weight = weight_left + weight_right

        # find the first tree that has a weight smaller than new_weight and returns its index in the list
        # If no such tree can be found, use len(trees) instead to append
        index = next((i for i, tree in enumerate(trees) if tree[1] < new_weight), len(trees))

        # insert the new tree there
        trees.insert(index, (new_tree, new_weight))

    huffman_tree = trees[0][0]
    return huffman_tree

# constructs the mapping with recursion
def build_codebook(tree, code=''):

    codebook = []

    # split the tree
    left_tree, right_tree = tree

    # if the left node has children, find the mapping of those children
    # else pair the character with the current code + 0
    if type(left_tree) is list:
        codebook += build_codebook(left_tree, code+'0')
    else:
        codebook.append((left_tree, code+'0'))

    # if the right node has children, find the mapping of those children
    # else pair the character with the current code + 1
    if type(right_tree) is list:
        codebook += build_codebook(right_tree, code+'1')
    else:
        codebook.append((right_tree, code+'1'))
    return codebook

# encodes the message
def huffman_encode(codebook, message):

    encoded_message = ''

    # build a char -> code dictionary
    forward_dict = dict(codebook)

    # replace each character with its code
    for char in message:
        encoded_message += forward_dict[char]

    return encoded_message

# decodes a message
def huffman_decode(codebook, encoded_message):

    decoded_message = ''
    key = ''

    # build a code -> char dictionary
    inverse_dict = dict([(v, k) for k, v in codebook])

    # for each bit in the encoding
    # if the bit is in the dictionary, replace the bit with the paired character
    # else look at the bit and the following bits together until a match occurs
    # move to the next bit not yet looked at
    for index, bit in enumerate(encoded_message):
        key += bit
        if key in inverse_dict:
            decoded_message += inverse_dict[key]
            key = ''

    return decoded_message

def main():

    # test example
    message = 'bibbity_bobbity'
    tree = build_huffman_tree(message)
    codebook = build_codebook(tree)
    encoded_message = huffman_encode(codebook, message)
    decoded_message = huffman_decode(codebook, encoded_message)

    print('message: ' + message)
    print('huffman tree: ' + str(tree))
    print('codebook: ' + str(codebook))
    print('encoded message: ' + encoded_message)
    print('decoded message: ' + decoded_message)

    # prints the following:
    #
    #  message: bibbity_bobbity
    #  huffman_tree: ['b', [[['_', 'o'], 'y'], ['t', 'i']]]
    #  codebook: [('b', '0'), ('_', '1000'), ('o', '1001'),
    #             ('y', '101'), ('t', '110'), ('i', '111')]
    #  encoded_message: 01110011111010110000100100111110101
    #  decoded_message: bibbity_bobbity

if __name__ == '__main__':
    main()
