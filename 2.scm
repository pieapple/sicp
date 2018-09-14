; 2.17
(define last-pair
  (lambda (items)
    (cond ((null? items) '())
          ((null? (cdr items)) items)
          (else (last-pair (cdr items))))))

(last-pair (list 23 72 149 34))


; 2.18
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1)
                    list2))))

(define (reverse items)
  (if (null? items)
      '()
      (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 4 9 16 25))


; 2.19
(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0)
	 1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define us-coins 
  (list 50 25 10 5 1))

(define us-coins2 
  (reverse (list 50 25 10 5 1)))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

(cc 100 us-coins2)


; 2.20
(define (eq-parity x y)
  (= (remainder x 2) (remainder y 2)))

(define (same-parity . items)
  (define (filter-parity parity items)
    (cond ((null? items) '())
        ((= parity (remainder (car items) 2))
         (cons (car items) (filter-parity parity (cdr items))))
        (else (filter-parity parity (cdr items)))))
  

  (if (null? items)
      '()
      (cons (car items)
            (filter-parity (remainder (car items) 2) (cdr items)))))

(same-parity 2 3 4 5 6 7)
(same-parity 1 2 3 4 5 6 7)

					; 2.21
(define (square-list items)
  (map square items))

(define (square-list2 items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (cdr items))))

(square-list (list 1 2 3 4))

(square-list2 (list 1 2 3 4))

; 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list3 (list 1 2 3 4))

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items '()))

(square-list4 (list 1 2 3 4))

;;; 2.23
(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;;; 2.24
(list 1 (list 2 (list 3 4)))


;;; 2.25
(define item1 '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr item1)))))

(define item2 '((7)))
(car (car item2))

(define item3 '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr item3))))))))))))

;;; 2.26
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (count-leaves items)
  (cond ((null? items) 0)
	((not (pair? items)) 1)
	(else (+ (count-leaves (car items))
		 (count-leaves (cdr items))))))

(define x (cons (list 1 2) (list 3 4)))

(length x)
(length (list x x))
(count-leaves x)
(count-leaves (list x x))

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;;; 2.27
(define (deep-reverse items)
  (map (lambda (item)
	 (if (pair? item)
	     (deep-reverse item)
	     item))
       (reverse items)))

(define x (list (list 1 2) (list 3 4)))

x

(reverse x)

(deep-reverse x)

;;; 2.28
(define (fringe items)
  (cond ((null? items) '())
	((not (pair? items)) (list items))
	(else (append
	       (fringe (car items))
	       (fringe (cdr items))))))

(fringe x)
(fringe (list x x))

;;; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (is-branch-weight? item)
  (not (pair? item)))

(define (weight-of-branch branch)
  (let ((item (branch-structure branch)))
    (if (is-branch-weight? item)
	item
	(total-weight item))))

(define (total-weight mobile)
  (+ (weight-of-branch (left-branch mobile))
     (weight-of-branch (right-branch mobile))))

(define (balanced-branch? branch)
  (let ((item (branch-structure branch)))
    (or
     (is-branch-weight? item)
     (self-balanced? item))))

(define (self-balanced? mobile)
  (let ((lb (left-branch mobile))
	(rb (right-branch mobile)))
    (and
     (= (* (weight-of-branch lb)
	   (branch-length lb))
	(* (weight-of-branch rb)
	   (branch-length rb)))
     (balanced-branch? lb)
     (balanced-branch? rb))))

(define br (make-branch 10 10))

(branch-structure br)

(define balance-mobile (make-mobile (make-branch 10 10)
				    (make-branch 10 10)))

(define unbalance-mobile (make-mobile (make-branch 0 0)
				      (make-branch 10 10)))

(define mobile-with-sub-mobile (make-mobile (make-branch 10 balance-mobile)
					    (make-branch 10 balance-mobile)))

(self-balanced? balance-mobile)

;;; 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else
	 (cons (square-tree (car tree))
	       (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree2 sub-tree)
	     (square sub-tree)))
       tree))

(define tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree tree1)

(square-tree2 tree1)

;;; 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square-tree3 tree)
  (tree-map square tree))

;;; 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (item)
			     (cons (car s) item)) rest)))))

(subsets '(1 2 3))

;;; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op
		      initial
		      (cdr sequence)))))

(define (map2 p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      '() sequence))

(map2 square '(1 2 3))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(append2 '(1 2 3) '(4 5 6))

(define (length2 sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0 sequence))

(length2 '(1 2 3))

;;; 2.34
(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ (* higher-terms x) this-coeff))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;;; 2.35
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (count-leaves2 tree)
  (accumulate +
	      0
	      (map (lambda (sub-tree)
		     (if (pair? sub-tree)
			 (count-leaves sub-tree)
			 1))
		   tree)))


;;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)

;;; 2.37
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 1 2 3))

(define map-n op)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v m)

(define (matrix-*-vector m v)
  (map (lambda (row)
	 (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map () m)))
