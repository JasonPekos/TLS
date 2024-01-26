#lang racket

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) l)
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))


(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else cons (car lat) (subst2 new o1 o2 (cdr lat))))))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))


(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
         (cond
           ((eq? (car lat) old) (cons new (multiinsertR new old (cdr lat))))
           (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
         ((null? lat) '())
         ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
         (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
        ((eq? old (car lat))(cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat)))))))))


(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))


(define o+
  (lambda (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (o+ (add1 n1) (sub1 n2))))))

(define o-
  (lambda (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (o- (sub1 n1) (sub1 n2))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(define ox
  (lambda (n1 n2)
    (cond
      ((zero? n1) 0)
      (else
       (+ n2 (ox (sub1 n1) n2))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n1 n2)
    (cond
      ((zero? n1) #f)
      ((zero? n2) #t)
      (else
       (o> (sub1 n1) (sub1 n2))))))

(define o<
  (lambda (n1 n2)
    (cond
      ((zero? n2) #f)
      ((zero? n1) #t)
      (else
       (o< (sub1 n1) (sub1 n2))))))

(define o=
  (lambda (n1 n2)
    (cond
      ((> n1 n2) #f)
      ((> n2 n1) #f)
      (else #t))))

(define o-expt
  (lambda (n1 n2)
    (cond
      ((zero? n2) 1)
      (else (* n1 (o-expt n1 (sub1 n2)))))))

(define o-quotient
  (lambda (n1 n2)
    (cond
      ((< n1 n2) 0)
      (else
       (add1 (o-quotient (- n1 n2) n2))))))

(define o-len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (o-len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
       (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))
      
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))
 
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? a (car lat)) (add1 (occur a (cdr lat))))
         (else
          (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (eq? n 1)))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

    
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+(occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
       (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons old (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) #t)
         (else (member* a (cdr l)))
         ))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
         
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2) ) #f)
      (else (eqlist? s1 s2)))))
      
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define rember2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((equal? (car l) s) (cdr l))
         (else (cons (car l)
                     (rember s (cdr l))))))
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l)
                          (rember s
                                  (cdr l)))))))))

(define rember3
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember3 s (cdr l)))))))

; Begin Ch6 — Shadows

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+) (numbered? (car (cdr (cdr aexp)))))
      ((eq? (car (cdr aexp)) '*) (numbered? (car (cdr (cdr aexp)))))
      ((eq? (car (cdr aexp)) '*) (numbered? (car (cdr (cdr aexp))))))))

(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) '+) (+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'x) (* (value (car aexp)) (value (car (cdr (cdr aexp))))))
      (else (expt (value (car aexp)) (value (car (cdr (cdr aexp)))))))))

; This function is intentionally broken to teach about valid subexpressions.
; (cdr nexp) is not necessarily and nexp, so can't recur on this.
(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+) (+ (value2 (cdr nexp)) (value2 (cdr (cdr nexp)))))
      ((eq? (car nexp) '*) (* (value2 (cdr nexp)) (value2 (cdr (cdr nexp)))))
      (else (expt (value2 (cdr nexp)) (value2 (cdr (cdr nexp))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr(aexp))))))

(define operator
  (lambda (nexp)
    (car nexp)))

(define value3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (((eq? (operator nexp)) '+) (+ (value3 (1st-sub-exp nexp)) (value3 (2nd-sub-exp nexp))))
      (((eq? (operator nexp)) '*) (* (value3 (1st-sub-exp nexp)) (value3 (2nd-sub-exp nexp))))
      (else (expt (value3 (1st-sub-exp nexp)) (value3 (2nd-sub-exp nexp)))))))



(define 1st-sub-exp2
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp2
  (lambda (aexp)
    (car (cdr (cdr(aexp))))))

(define operator2
  (lambda (nexp)
    (car (cdr nexp))))

(define value3-2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (((eq? (operator2 nexp)) '+) (+ (value3-2 (1st-sub-exp2 nexp)) (value3-2 (2nd-sub-exp2 nexp))))
      (((eq? (operator2 nexp)) '*) (* (value3-2 (1st-sub-exp2 nexp)) (value3-2 (2nd-sub-exp2 nexp))))
      (else (expt (value3-2 (1st-sub-exp2 nexp)) (value3-2 (2nd-sub-exp2 nexp)))))))


(define peano-zero?
  (lambda (val)
    (null? val)))

(define peano-add1
  (lambda (val)
    (cons '() val)))

(define peano-sub1
  (lambda (val)
    (cdr val)))

(define peano+
  (lambda (val1 val2)
    (cond
      ((peano-zero? val1) val2)
      (else  (peano-add1 (peano+ val2 (peano-sub1 val1)))))))

(define lat2
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat2? (cdr l)))
      (else #f))))





