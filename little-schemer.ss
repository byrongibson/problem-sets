#lang scheme

;insert λ in DrScheme/DrRacket with Ctrl+\.

;comment key words:
;(troubleshoot) : not working, review and fix
;(difficult)    : didn't get it, had to look at answer, review
;(p.1234)       : page in The Little Schemer this code appears on

;atom primitive from The Little Schemer not implemented in DrScheme/DrRacket
(define atom?
   (lambda (a)
     (not (list? a))
   )
)

;vars for testing
(define z
  '0)

;lists of atoms used for testing
(define lat
  '(an oldish sort of oldish long list of oldish atoms))
(define latlat
  '(((an)) (oldish (sort of)) oldish (long (list of)) oldish atoms))
(define nulllatlat
  '(((()))((an)) (oldish (sort of)) ((())) oldish (long (list of(()))) oldish atoms))
(define numlat
  '(5 pears 6 prunes 9 dates))
(define commanumlat
  '(3 blind mice, 3 blind mice))

;tups used for testing
(define tup
  '(1 2 3 4 5))
(define negTup
  '(1 -2 3 -4 5))
(define t1
  '(3 7))
(define t2
  '(4 6))
(define t3
  '(3 7 8 1))
(define t4
  '(4 6 8 1))

;sets used for testing
(define pair
  '(1 2))
(define pairs
  '(one (two)))
(define spairs
  '((one) (two)))
(define pairset
  '((a b c d) (e f g h)))
(define lset
  '((a b c)
    (c a d e)
    (e f g h a b))
)
(define lsets
  '((6 pears and)
    (3 peaches and 6 peppers)
    (8 pears and 6 plums)
    (and 6 prunes with some apples))
)

;rels used for testing
(define rel1
  '((8 3) (4 2) (7 6) (6 2) (3 4)))
(define rel2
  '((9 1) (8 2) (7 3) (6 4) (5 0)))
(define rel3
  '((0 9) (1 8) (2 7) (3 6) (4 5)))

;is l a list of atoms?
(define lat?
  (λ (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f)
    )
  )
)

;is a in lat?
(define member?
  (λ (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat))))
    )
  )
)

;return lat with the first occurence of a removed
(define rember
  (λ (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat))))
    )
  )
)

;return lat with first occurence of new inserted to the right of old
(define insertR
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat))))
    )
  )
)

;return lat with first occurence of new inserted to the left of old
(define insertL
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat))))
    )
  )
)

;return lat with first occurence of new substituted for old
(define subst
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (subst new old (cdr lat))))
      (else (cons (car lat) (subst new old (cdr lat))))
    )
  )
)

;return lat with all occurences of a removed
(define multiRember
  (λ (a lat)
    (cond
      ((null? lat)(quote()))
      ((eq? (car lat) a)(multiRember a (cdr lat)))
      (else (cons (car lat)(multiRember a (cdr lat))))
    )
  )
)

;return lat with new inserted to the right of every occurence of old
(define multiInsertR
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)(cons old (cons new (multiInsertR new old (cdr lat)))))
      (else (cons (car lat)(multiInsertR new old (cdr lat))))
    )
  )
)

;return lat with new inserted to the left of every occurence of old
(define multiInsertL
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)(cons new (cons old (multiInsertL new old (cdr lat)))))
      (else (cons (car lat)(multiInsertL new old (cdr lat))))
    )
  )
)

;return lat with new substituted for every occurence of old
(define multiSubst
  (λ (new old lat)
    (cond
      ((null? lat)(quote()))
      ((eq? (car lat) old)(cons new (multiSubst new old (cdr lat))))
      (else (cons (car lat)(multiSubst new old (cdr lat))))
    )
  )
)

;add m to n
(define plus
  (λ (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m))))
    )
  )
)

;subtract m from n
(define minus
  (λ (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m))))
    )
  )
)

;sum all the members of a single tuple
(define sumTup
  (λ (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (sumTup (cdr tup))))
    )
  )
)

;sum two tuples
(define addTups
  (λ (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (plus (car t1)(car t2)) (addTups (cdr t1)(cdr t2))))
    )
  )
)

;multiply n by m
(define multiply
  (λ (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (multiply n (sub1 m))))
    )
  )
)

;is n greater than m?
(define gt?
  (λ (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt? (sub1 n) (sub1 m)))
    )
  )
)

;is n less than m?
(define lt?
  (λ (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt? (sub1 n) (sub1 m)))
    )
  )
)

;are n and m numerically equal?
(define nequal?
  (λ (n m)
    (cond
      ((gt? n m) #f)
      ((lt? n m) #f)
      (else #t)
    )
  )
)

;divide n by m
(define divide
  (λ (n m)
    (cond
      ((lt? n m) 0)
      (else (add1 (divide (minus n m) m)))
    )
  )
)

;raise n to the power of m
(define power
  (λ (n m)
    (cond
      ((zero? m) 1)
      (else (multiply n (power n (sub1 (abs m)))))
    )
  )
)

;what is the length of lat?
(define lenlat
  (λ (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (lenlat (cdr lat))))
    )
  )
)

;return the atom at index n in lat
(define pick
  (λ (n lat)
    (cond
      ((null? lat) (quote()))
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n)(cdr lat)))
    )
  )
)

;return the lat with the atom at index n removed
(define rempick
  (λ (n lat)
    (cond
      ((null? lat) (quote()))
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)(rempick (sub1 n) (cdr lat))))
    )
  )
)

;return lat with all numbers removed
(define nonums
  (λ (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (nonums (cdr lat)))
      (else (cons (car lat) (nonums (cdr lat))))
    )
  )
)

;return lat with all non-numbers removed
(define allnums
  (λ (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (cons (car lat) (allnums (cdr lat))))
      (else (allnums (cdr lat)))
    )
  )
)

;is atom a1 equal to atom a2?
(define eqan?
  (λ (a1 a2)
    (cond
      ((and (number? a1)(number? a2)) (nequal? a1 a2))
      ((or  (number? a1)(number? a2)) #f)
      (else (eq? a1 a2))
    )
  )
)

;how many times does a occur in lat?
(define occur?
  (λ (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur? a (cdr lat))))
      (else (occur? a (cdr lat)))
    )
  )
)

;is n = 1?
(define one?
  (λ (n)
    (= n 1)))

;return lat with the atom at index n removed
(define rempick2
  (λ (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n)(cdr lat))))
    )
  )
)

;return lat with all occurences of a removed, even from sublists within lat
(define rember*
  (λ (a lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat)) 
       (cond
         ((eqan? a (car lat)) (rember* a (cdr lat)))
         (else (cons (car lat) (rember* a (cdr lat))))))
    (else (cons (rember* a (car lat)) (rember* a (cdr lat))))
    )
  )
)

;incorrect attempt at reversing a list, probably don't know enough yet to do this
(define revlat
  (λ (lat)
    (cond
      ((null? lat) (quote()))
      (else (cons (revlat (cdr lat)) (car lat)))
    )
  )
)

;return latlat with new inserted to the right of all occurences of old
(define insertR*
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons old (cons new (insertR* new old (cdr lat)))))
         (else (cons (car lat) (insertR* new old (cdr lat))))))
       (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat))))
    )
  )
)

;return latlat with new inserted to the left of all occurences of old
(define insertL*
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons new (cons old (insertL* new old (cdr lat)))))
         (else (cons (car lat) (insertL* new old (cdr lat))))))
       (else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat))))
    )
  )
)

;return latlat with all occurences of old replaced with new
(define subst*
  (λ (new old lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons new (subst* new old (cdr lat))))
         (else (cons (car lat) (subst* new old (cdr lat))))))
       (else (cons (subst* new old (car lat)) (subst* new old (cdr lat))))
    )
  )
)

;how many times does a occur in latlat?
(define occur*
  (λ (a lat)
    (cond
      ((null? lat) 0)
      ((atom? (car lat))
       (cond
         ((eqan? a (car lat)) (add1 (occur* a (cdr lat))))
         (else (occur* a (cdr lat)))))
      (else (plus (occur* a (car lat)) (occur* a (cdr lat))))
    )
  )
)

;is a in latlat?
(define member*
  (λ (a lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat))
       (or (eq? (car lat) a)
           (member* a (cdr lat))))
       (else (or (member* a (car lat)) (member* a (cdr lat))))
    )
  )
)

;return the leftmost atom of latlat or null list for nulllatlat
(define leftmost
  (λ (lat)
    (cond
      ((null? (car lat)) (quote()))
      ((atom? (car lat)) (car lat))
      (else (leftmost (car lat)))
    )
  )
)

;is list1 = list2, where lists can include atoms (alpha/numeric), sublists, and null lists.
#|(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (null? (car l1)) (null? (car l2)))
            (eqlist (cdr l1) (cdr l2)))
      ((or (null? (car l1)) (null? (car l2))) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
           (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))|#

;simplified using equal
(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
    )
  )
)

;is S-expression1 = S-expression2?
(define equal?
  (λ (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2) #f))
      (else (eqlist? s1 s2))
    )
  )
)

;redefine rember to remove the first S-expression s from a list of S-expressions l
(define rembers
  (λ (s l)
    (cond
      ((null? l) (quote()))
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rembers s (cdr l))))
    )
  )
)

;is aexp an arithmetic expression (defined as a representation of an expression containing only numbers, +, *, ^) - needs troubleshooting
(define numbered?
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote(+)))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote(-)))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote(*)))
       (and (numbered? (car aexp)) (numbered? (cdr (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote(/)))
       (and (numbered? (car aexp)) (numbered? (cdr (cdr (cdr aexp))))))
    )
  )
)

;return first subexpression of the Scheme-standard representation an arithmetic expression (+ n m)
(define 1st-sub-exp
  (λ (aexp)
    (car (cdr (aexp)))
  )
)

;return second subexpression of the Scheme-standard representation an arithmetic expression (+ n m)
(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr (aexp))))
  )
)

;return the operator of the Scheme-standard representation an arithmetic expression (+ n m)
(define operator
  (λ (aexp)
    (car aexp)
  )
)

;calculate the value of the representation of an arithmetic expression (troubleshoot)
(define value
  (λ (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator aexp) (quote +))
       (+ (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) (quote *))
       (* (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
    )
  )
)

;return true if no atom appears more than once in the list
(define set?
  (λ (s)
    (cond
      ((null? (cdr s)))
      ((member? (car s) (cdr s))#f)
       (else (set? (cdr s)))
     )
  )
)

;return lat with all duplicates removed
(define makeset
  (λ (lat)
    (cond
      ((null? lat) (quote()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat))))
    )
  )
)

;rewrite makeset with multiRember; cons (car lat) to lat with all (car lat) removed from lat
(define makesetm
  (λ (lat)
    (cond
      ((null? lat) (quote()))
      (else (cons (car lat) (makeset (multiRember (car lat) (cdr lat)))))
    )
  )
)

;are all the atoms of lat1 in lat2?
(define subset?
  (λ (lat1 lat2)
    (cond
      ((null? lat1) #t)
      ((atom? lat1) (member? lat1 lat2)) 
      (else (subset? (car (cdr lat1)) lat2))
    )
  )
)

;define subset without atom? (troubleshoot) (p.115)
(define subsets?
  (λ (l1 l2)
    (cond
      ((null? l1) #t)
      (else 
       (and (member? (car l1) l2)
            (subsets? (cdr l1) l2))
      )
    )
  )
)
    
;is set1 = set2?
(define eqset?
  (λ (s1 s2)
    (cond
      ((and (null? s1)(null? s2)) #t)
      (else (and (eq? (car s1) (car s2)) (eqset? (cdr s1)(cdr s2))))
    )
  )
)

;rewrite eqset? using subset?
(define eqsets?
  (λ (s1 s2)
      (and (subset? s1 s2) (subset? s2 s1))
  )
)

;is at least one atom of set1 in set2?
(define intersect?
  (λ (s1 s2)
    (cond
      ((null? s1) #f)
      (else (or (member? (car s1) s2) (intersect? (cdr s1) s2)))
    )
  )
)

;return exactly once every member of set1 also in set2
(define intersect
  (λ (s1 s2)
    (cond
      ((null? s1) (quote()))
      ((member (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2))
    )
  )
)

;return the union of set1 and set2, eg return every occurance of every member in both sets exactly once (review) (p.116)
(define union
  (λ (s1 s2)
    (cond
      ((null? s1) s2)
      ((member (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) s2)))
    )
  )
)

;return all the atoms in set1 not in set2
(define difference
  (λ (s1 s2)
    (cond
      ((null? s1) (quote()))
      ((member (car s1) s2) (difference (cdr s1) s2))
      (else (cons (car s1) (difference (cdr s1) s2)))
    )
  )
)

;return the intersection of a list of an arbitrary number of sets (difficult)
(define intersectall
  (λ (set)
    (cond
      ((null? (cdr set)) (car set))
      (else (intersect (car set) (intersectall (cdr set))))
    )
  )
)

;return true if p is a list containing exactly 2 S-expressions
(define pair?
  (λ (p)
    (cond
      ((null? p) #f)
      ((atom? p) #f)
      ((null? (cdr p)) #f)
      ((null? (cdr (cdr p))) #t)
      (else #f)
    )
  )
)

;return the first S-exp of a pair
(define firstp
  (λ (p)
    (car p)
  )
)

;return the second S-exp of a pair
(define secondp
  (λ (p)
    (car (cdr p))
  )
)

;combine two S-exp into a pair
(define buildp
  (λ (sx1 sx2)
    (cons sx1 (cons sx2 (quote())))
  )
)

;return the third S-exp of a list
(define thirdl
  (λ (l)
    (car (cdr (cdr l)))))

;return a list of the first s-exp of each list in the input list
(define firsts
  (λ (l)
    (cons (car l) (firsts (cdr l)))
  )
)

;is the first s-exp of a rel a set?
(define fun?
  (λ (rel)
    (set? (firsts rel))
  )
)

;return the pair with elements reversed
(define revpair
  (λ (p)
    ((null? p) (quote()))
    (buildp (secondp) (firstp p))
  )
)

;return the list of pairs with the elements of each pair reversed
(define revrel
  (λ (s)
    (cond
      ((null? (cdr s)) (quote()))
      (cons (revpair (car s)) (revrel (cdr s)))
    )
  )
)

