#lang racket

(require data/heap)

(struct branch (freq left right))
(struct leaf (sym freq))

(define (weight node)
  (if (branch? node)
      (branch-freq node)
      (leaf-freq node)))

(define (make-huffman-tree leaves)
  (define (combine x y)
    (branch (+ (weight x) (weight y)) x y))
  
  (define hp (make-heap
             (lambda (x y)
               (<= (weight x) (weight y)))))

  (heap-add-all! hp leaves)

  (define (merge)
    (if (= (heap-count hp) 1)
        ;; if there's only one node in the heap then return that element
        (heap-min hp)

        ;; else merge the two smallest nodes
        (let ((a (heap-min hp)))
          ;; get the smallest node and remove it
          (heap-remove-min! hp)

          (let ((b (heap-min hp)))
            ;; get the second smallest node and remove it
            (heap-remove-min! hp)

            ;; add the combined node to the heap
            (heap-add! hp (combine a b))
            (merge)))))
  (merge))

(define (make-codebook tree)
  (define ht (make-hash))

  (define (dfs node path)
    (if (leaf? node)
        ;; if node is a leaf then add the symbol to the codebook
        (hash-set! ht (leaf-sym node) path)
        (begin (dfs (branch-left node) (append path (list 0)))
               (dfs (branch-right node) (append path (list 1))))))

  (dfs tree null)
  ht)

(define (encode codebook msg)
  (if (null? msg)
      null
      (append (hash-ref codebook (car msg))
              (encode codebook (cdr msg)))))

(define (decode tree msg)
  (define (dfs node bits)
    (if (leaf? node)
        (cons (leaf-sym node) (decode tree bits))
        (if (= (car bits) 0)
            (dfs (branch-left node) (cdr bits))
            (dfs (branch-right node) (cdr bits)))))

  (if (null? msg)
      null
      (dfs tree msg)))

(define (generate-leaves msg)
  (define ht (make-hash))
  (for ([ch msg])
    (hash-update! ht ch add1 (lambda () 0)))
  (for/list ([(k v) (in-hash ht)])
    (leaf k v)))
  
;; test
(define message (string->list "bibbity bobbity"))
(define leaves (generate-leaves message))
(define huffman-tree (make-huffman-tree leaves))
(define codebook (make-codebook huffman-tree))

(define encoded (encode codebook message))
(define decoded (list->string (decode huffman-tree encoded)))

(println encoded)
(println decoded)
