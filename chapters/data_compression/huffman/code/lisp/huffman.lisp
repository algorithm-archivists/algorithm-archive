(defstruct leaf symbol weight)
(defstruct code-tree left right symbols weight)

(defun weight (x)
  "Returns the total weight of a tree or the weight of a leaf."
  (if (leaf-p x)
      (leaf-weight x)
      (code-tree-weight x)))

(defun symbols (x)
  "When called on a tree returns the list of symbols.
When called on a leaf returns a list containing the one symbol."
  (if (leaf-p x)
      (list (leaf-symbol x))
      (code-tree-symbols x)))

(defun tree-merge (x y)
  "Merge two trees or leaves into one tree."
  (make-code-tree :left x
		  :right y
		  :symbols (append (symbols x) (symbols y))
		  :weight (+ (weight x) (weight y))))

(defun encode-string (message tree)
  "Encodes a string into a list of bits using tree."
  (encode (symbolize message) tree))

(defun encode (message tree)
  "Encodes a list of symbols into a list of bits using tree."
  (if (null message)
      nil
      (append (encode-symbol (first message) tree)
	      (encode (rest message) tree))))

(defun encode-symbol (symbol tree)
  "Encodes a symbol into a list of bits using tree.
If the tree is a single leaf, all bits are 0."
  (if (leaf-p tree)
      (if (eq symbol (leaf-symbol tree)) '(0))
      (let ((left (code-tree-left tree))
	    (right (code-tree-right tree)))
	(cond ((member symbol (symbols left))
	       (if (leaf-p left) '(0) (cons 0 (encode-symbol symbol left))))
	      ((member symbol (symbols right))
	       (if (leaf-p right) '(1) (cons 1 (encode-symbol symbol right))))
	      (t (error "Symbol ~S is not represented in code-tree." symbol tree))))))

(defun decode-to-string (bits tree)
  "Decodes a list of bits into a string using tree."
  (desymbolize (decode bits tree)))

(defun decode (bits tree)
  "Decodes a list of bits into a list of symbols using tree."
  (defun decode% (bits current-branch)
    (if (null bits)
	nil
	(let ((next-branch
	       (choose-branch (first bits) current-branch)))
	  (if (leaf-p next-branch)
	      (cons (leaf-symbol next-branch)
		    (decode% (rest bits) tree))
	      (decode% (rest bits) next-branch)))))
  (decode% bits tree))

(defun choose-branch (bit branch)
  "Returns left branch on 0 bit and right branch on 1 bit.
If branch is already a leaf, it simply returns branch."
  (if (leaf-p branch) branch
      (cond ((= bit 0) (code-tree-left branch))
	    ((= bit 1) (code-tree-right branch)))))

(defun generate-huffman-tree-from-string (message)
  "Generates a huffman tree using the frequencies of characters
in message"
  (generate-huffman-tree (frequency-pairs (symbolize message))))

(defun generate-huffman-tree (pairs)
  "Constructs a Huffman Tree based on a list of pairs
of symbols and weights."
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (leaf-set)
  "Recursively merges an ordered list of leaves into
one tree in order from smallest to largest weight."
  (if (> (length leaf-set) 1)
      (successive-merge (adjoin-set (tree-merge (first leaf-set) (second leaf-set))
				    (rest (rest leaf-set))))
      (first leaf-set)))

(defun adjoin-set (x set)
  "Adds a leaf or tree to a list of leaves or trees
in a position sorted by weight."
  (cond ((null set) (list x))
	((< (weight x) (weight (first set))) (cons x set))
	(t (cons (first set)
		 (adjoin-set x (rest set))))))

(defun make-leaf-set (pairs)
  "Returns a list of leaves constructed from a list
of pairs of symbols and weights."
  (if (null pairs)
      '()
      (let ((pair (first pairs)))
	(adjoin-set (make-leaf :symbol (first pair) :weight (second pair))
		    (make-leaf-set (rest pairs))))))

(defun symbolize (message)
  "Converts a string to a list of character symbols."
  (map 'list (lambda (x) (intern (string x))) message))

(defun desymbolize (message)
  "Converts a list of character symbols to a string."
  (apply #'concatenate (cons 'string (map 'list #'string message))))

(defun frequency-pairs (message)
  "Returns a list of pairs of symbols and frequencies."
  (let ((table (make-hash-table)))
    (loop for symbol in message do
	 (incf (gethash symbol table 0)))
    (loop for key being the hash-keys of table
       using (hash-value value) collect (list key value))))
