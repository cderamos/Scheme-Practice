#lang racket
(require math/base)

(define (addSecond mylist)
  (let ((cnt 0))
  (let recur ((newlist mylist))
      (cond
         ((null? newlist) cnt)
         ((not(= 0 (modulo  (index-of mylist (car newlist)) 2))) (set! cnt (+ cnt (car newlist))) (recur(cdr newlist)))
         (else (recur (cdr newlist)))))))
         
;(addSecond '(1 2 3 4))

(define lightSide '("Luke Skywalker" "Han Solo" "Anakin Skywalker" "BB-8" "Chewbacca" "Leia Organa" "Obi-Wan Kenobi" "Rey" "Finn"))
(define darkSide '("Darth Vader"  "Darth Maul" "BB-9E" "Boba Fett" "Palpatine" "Bossk" "General Grievous" "Kylo Ren"))

(define (partner herolist)
  (cond
    ((null? herolist) "There's no one here!")
    ((let ((a (random 0 (length herolist)))  (b (random 0 (length herolist))))
      (cond
        ((= a b) (partner herolist))
        (else  (display "Team up!\n") (cons (list-ref herolist a) (list-ref herolist b))))))))
        
;(partner lightSide)
;(partner darkSide)

(define (matchmaker mylist)
  (define ht (make-hash))
   (cond
   ((null? mylist) '())
   ((let recur ((studentlist mylist))
      (cond
        ((= (length studentlist) 0) (hash->list ht))
        ((= (length studentlist) 1) (hash-set! ht (list-ref studentlist 0) "VACANT") (hash->list ht))
        ((let ((a (random 0 (length studentlist)))  (b (random 0 (length studentlist))))
               (cond
                 ((= a b) (recur studentlist))
                 (else (hash-set! ht (list-ref studentlist a) (list-ref studentlist b))
                       (set! studentlist (remove* (list (list-ref studentlist a) (list-ref studentlist b)) studentlist))
                       (recur studentlist))))))))))
       

;(matchmaker '("Anna" "Hans" "Shira" "Kevin" "Gereon"))

(define (changed numlist)
  (cond
    ((null? numlist) '())
    (else (let recur ((newlist (remove* '(0 1 -1) numlist)))
          (cond
            ((null? newlist) '())
            (else (cond
                    ((negative? (car newlist)) (append (list(/ 1 (car newlist))) (recur (cdr newlist))))
                    (else (append (list(* (car newlist) 10)) (recur (cdr newlist)))))))))))

;(changed '(1 0 2)) --> (20)

;(occur 4 '(1 2 2 4 4 4)) --> 3
;variant of occur where it gets the longest occurence (occur 4 '(1 2 4 4 5 6 4 4 4)) --> 3
(define (occurVariant num somelist)
  (let ((cnt 0) (most 0))
  (cond
    ((null? somelist) 0)
    (else (let recur ((newlist somelist))
       (cond
         ((null? newlist) most)
         ((and (= 2 (length newlist)) (not( = (car newlist) num))) 0 (recur (cdr newlist)))
         ((= (car newlist) num) (set! cnt (+ cnt 1)) (set! most cnt) (recur (cdr newlist)))
         ((> cnt most) (set! most cnt))
         (else (set! cnt 0) (recur (cdr newlist)))))))))
    
(define (occur num somelist)
  (let ((cnt 0))
  (cond
    ((null? somelist) cnt)
    (else
     (let recur ((newlist somelist))
        (cond
          ((null? newlist) cnt)
          ((= (car newlist) num) (set! cnt (+ cnt 1)) (recur (cdr newlist)))
          (else (recur (cdr newlist)))))))))

          
(define (occur2 somelist)
  (let ((most 0) (occurs 0))
  (cond
    ((null? somelist) most)
    (else
     (let recur ((newlist somelist))
       (cond
         ((null? newlist) (for/list ([i occurs]) most))
         ((> (occurVariant (car newlist)  newlist) occurs) (set! most (car newlist)) (set! occurs(occurVariant (car newlist)  newlist)) (recur (cdr  newlist)))
         (else (recur (cdr newlist)))))))))


;lambda example/non-lambda examples
;add 5 to every number in a list          
;(map (lambda (x) (+ x 5)) (list 1 2 3 4)) ;<-- one use only
;
;(define (fives1 mylist)(+ mylist 5))
;(map fives1 (list 1 2 3 4))
;
;(define (fives2 mylist)
; (cond
;   ((null? mylist) '())
;   (else (append (list (+ 5 (car mylist))) (fives2 (cdr mylist))))))
;   (fives2 '(1 2 3 4))

;list version
(define planetList '(("Geonosis" 200)("Kamino" 7000)("Rhen Var" 100) ("Kashyyyk" 2500) ("Naboo" 9700) ("Coruscant" 9800)))
 
;get planets with over N pop.
(define (planetPop num planets)
  (cond
    ((null? planets) '())
    ((= num 0) planets)
    ((> (cadr (car planets)) num) (append (list(car planets)) (planetPop num (cdr planets))))
    (else (planetPop num (cdr planets)))))

(define (getMin numlist)
  (let ((min (car numlist)))
  (cond
    ((null? numlist) '())
    (else (let recur ((newlist numlist))
            (cond
              ((null? newlist) min)
              ((< (car newlist) min) (set! min (car newlist)) (recur (cdr newlist)))
              (else (recur (cdr newlist)))))))))

(define (getMax numlist)
  (cond
    ((null? numlist) '())
    (else  (let ((max (car numlist))) (let recur ((newlist numlist))
            (cond
              ((null? newlist) max)
              ((>= (car newlist) max) (set! max (car newlist)) (recur (cdr newlist)))
              (else (recur (cdr newlist)))))))))


;(hiSort '(5 6 1 4)) --> '(6 5 4 1)), high to low
(define (hiSort numlist)
  (let ((max (getMax numlist)))
    (set! numlist (remove max numlist))
  (cond
    ((null? numlist) (append (list max) '()))
    (else (append (list max) (hiSort numlist))))))



(define (totals person numlist)
  (define ht (make-hash))
  (let ((total 0))
  (cond
    ((null? numlist) '())
    ((string=? person  " ") numlist)
    (else (let recur ((newlist numlist))
            (cond
              ((null? newlist) (hash-set! ht person total) (car(hash->list ht)))
              ((string=? (caar newlist) person) (set! total (+ total (cadr (car newlist)))) (recur (cdr newlist)))
              (else (recur (cdr newlist)))))))))
            
(totals "Luke"  '(("Luke" 50) ("Bossk" 55) ("Iden" 25) ("Luke" 75)))
(totals "Luke"  '(("Luke" 50)))

;(everyTotal '(("Luke" 50) ("Bossk" 55) ("Iden" 25) ("Luke" 75))) --> ((Luke . 125) (Bossk . 55) (Iden . 25))
(define (everyTotal numlist)
  (define ht (make-hash))
  (cond
    ((null? numlist) '())
    (else (let recur ((newlist numlist))
            (cond
              ((null? newlist) (hash->list ht)) ;<-- will change this at the end
              ((hash-has-key? ht (caar newlist)) (recur (cdr newlist)))
              (else (hash-set! ht (caar newlist) (cdr (totals (caar newlist) numlist))) (recur (cdr newlist))))))))

;everyTotal but without the totals function
(define (everyTotal2 numlist)
  (define ht (make-hash))
  (cond
    ((null? numlist) '())
    (else (let recur ((newlist numlist))
            (cond
              ((null? newlist) (hash->list ht)) ;<-- will change this at the end
              ((hash-has-key? ht (caar newlist)) (let ((hashlist (hash->list ht))) ;<-- note use of LET vs DEFINE, either or
                                                   (define total (cdr (car hashlist)))
                                                   (set! total (+ total (cadr (car newlist)))) (hash-set! ht (caar newlist) total) (recur (cdr newlist))))
              (else (hash-set! ht (caar newlist) (cadr (car newlist))) (recur (cdr newlist))))))))
            
(everyTotal '(("Luke" 50) ("Bossk" 55) ("Iden" 25) ("Luke" 75)))  ;<-- longer, more efficient (doesn't evaluate EACH repeated name)
(everyTotal2 '(("Luke" 50) ("Bossk" 55) ("Iden" 25) ("Luke" 75))) ;<-- shorter, less efficient (evaluates EACH repeated name)


;(removal "Luke" '(("Luke" 125) ("Iden" 25) ("Bossk" 55))) --> '(("Iden" 25) ("Bossk" 55))
(define (removal name mylist)
  (cond
    ((null? mylist) '())
    ((string=? name " ") mylist)
    ((not (string=? (caar mylist) name)) (append (list(car mylist)) (removal name (cdr mylist))))
    (else (removal name (cdr mylist)))))

(removal "Luke" '(("Luke" 125) ("Iden" 25) ("Bossk" 55)))

;(sums '(1 2 3)) --> 6
(define (sums numlist)
  (let ((most 1) (hold 1))
  (cond
    ((null? numlist) 0)
    ((= 0 (car numlist)) 0)
    ((= 1 (length numlist)) (car numlist))
    (else (let recur ((newlist numlist))
            (cond
              ((null? newlist) most)
              (else (cond
                      ((>= (* (car newlist) most) most) (set! most (* (car newlist) most)) (set! hold (* (car newlist) most)) (recur (cdr newlist)))
                      ((>= (* (car newlist) hold) most) (set! most (* (car newlist) hold)) (set! hold (* (car newlist) hold))( recur (cdr newlist)))
                      ((<= (* (car newlist) most)) (set! hold (* (car newlist) most)) (recur (cdr newlist)))))))))))
    
