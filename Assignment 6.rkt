(define run1
  (lambda ()
    (- (+ 3 5) 7)
    )
  )

(define run2
  (lambda ()
    (- (* (+ (+ 8 5) 4) 2) 25)
    )
  )

(define run3
  (lambda ()
    (- 10 (+ (* 3 5) (+ 2 (* 0 5))))
    )
  )

(define run4
  (lambda ()
    (* 5 (+ 4 (/ (+ (+ 10 10) (* 5 8)) (+ 10 2))))
    )
  )

(define run5
  (lambda()
   (+ (/ (- (/ (/ (* (+ 3 5) (+ 6 4)) 2) 2) 5) 3) (+ (/ (+ (* 2 10) (* 5 4)) 2) (* 4 5)))
    )
  )


(define RecursiveMultiply
  (lambda (x y)
    (cond
      ((= x 0) 0) ;returns 0 if x is 0
      ((= y 0) 0) ;returns 0 if y is 0, base case
      ((< y 0)
       (RecursiveMultiply(- x) (- y))) ;if y is negative, turn it positive and turn x negative
      (else
       (+ x (RecursiveMultiply x (- y 1)))));add x to x y times
    )
  )


(define ReadAndMult
  (lambda()
    (Variable1 (read))
    )
  )
(define Variable1
  (lambda(x)
    (if (not (number? x))
        0;if the first input is not a number, return 0
        (Variable2 x (read)));if the first input is a number, check for a second input
    )
  )
(define Variable2
  (lambda(x y)
    (if (not (number? y))
        0;if the second input is not a number, return 0
        (RecursiveMultiply x y));if both inputs were numbers, we can multiply them
    )
  )

(define number-list?
  (lambda (lst)
   (if (not (pair? lst));if the parameter is not a list or empty, return false
       #f
       (if (= (HowMany lst) 1);base case: there's only one element in the list
           (if (number? (car lst))
               #t;if this one element is a number, return true
               #f;else return false
            )
           (if (not (number? (car lst)));there is more than one element in the list
               #f;if the first element in this list is not a number, return false
               (number-list? (cdr lst)))));else, check all other elements in the list
    )
  )

(define HowMany
  (lambda (lst)
    (if (null? lst);the list is empty
        0
        (+ 1 (HowMany (cdr lst))));else, add one and see how many elements are left
    )
  )

(define sum-number-list
  (lambda (lst)
    (if (number-list? lst)
        (add lst);if this is a number list, perform the add function
        #f);otherwise, return false
    )
  )

(define add
  (lambda (lst)
    (if (null? lst);base case
        0;when the list is empty, add 0
        (+ (car lst) (add (cdr lst))));add the first element to all other elements in the list
    )
  )


(define read-int-list
  (lambda ()
    (let ((a (read)))
      (if (equal? a 'q);base case
          '();if input is q, don't add to the list
          (cons a (read-int-list))));else, put the first input as the head and read more inputs and add them to this list
    )
  )


(define PairOff
  (lambda (x y)
    (cons x (cons y '()));adds x and y to the head of an empty list
    )
  )

(define Combiner
  (lambda (lst1 lst2)
    (if (null? lst1);base case
        '();if the list is empty, add nothing
        (cons (list (car lst1) (car lst2)) (Combiner (cdr lst1) (cdr lst2))));else, make a list of lists with the heads of both lists being the first element as a list
    )
  )


(define happy?
  (lambda (x)
   (cond
     ((= x 4) #f);base case, number is not happy
     ((= x 1) #t);base case, number is happy
     (else
      (happy? (SplitAdd (NumToList x)))))
    )
  )

(define NumToList
  (lambda (x)
    (if (< x 10);base case
        (list x);add x to the list, it's the last digit of the number
        (append (NumToList (quotient x 10)) (list (- x (* 10 (quotient x 10))))));makes each digit a single number in a list
    )
  )
(define SplitAdd
  (lambda (x)
    (if (null? x);base case
        0;add nothing
        (+ (expt (car x) 2) (SplitAdd (cdr x))));add the first number squared to all the other numbers squared
    )
  )


(display "Testing Convert and Run")(newline)
(display "(run1) - answer should be 1 - tested: ")
(run1)
(display "(run2) - answer should be 9 - tested: ")
(run2)
(display "(run3) - answer should be -7 - tested: ")
(run3)
(display "(run4) - answer should be 45 - tested: ")
(run4)
(display "(run5) - answer should be 45 - tested: ")
(run5)
(display "Testing RecursiveMultiply")(newline)
(display "(RecursiveMultiply 6 2) - expected 12 - got: ")
(RecursiveMultiply 6 2)
(display "(RecursiveMultiply -6 -2) - expected 12 - got: ")
(RecursiveMultiply -6 -2)
(display "(RecursiveMultiply 6 -2) - expected -12 - got: ")
(RecursiveMultiply 6 -2)
(display "(RecursiveMultiply -6 2) - expected -12 - got: ")
(RecursiveMultiply -6 2)
(display "Testing ReadAndMult")
(display "Input 6 5 -->")
(ReadAndMult)
(display "30 <-- correct answer")
(newline)
(display "Testing number-list?")(newline)
(display "(number-list? ‘(1 2 3 4) - epxected #t - got: ")
(number-list? '(1 2 3 4))
(display "(number-list? ‘(1 2 (3) 4) - expected #f - got: ")
(number-list? '(1 2 (3) 4))
(display "(number-list? ‘(1 2 a 4) - expected #f - got: ")
(number-list? '(1 2 a 4))
(newline)
(display "Testing sum-number-list")(newline)
(display "(sum-number-list '(1 2 3 4 5) - expected 15 - got: ")
(sum-number-list '(1 2 3 4 5))
(display "(sum-number-list '(1 (2))) - expected #f - got: ")
(sum-number-list '(1 (2)))
(display "(sum-number-list '(a b c)) - expected #f - got: ")
(sum-number-list '(a b c))
(newline)
(display "Testing read-int-list")(newline)
(display "Input 1 2 3 4 5 q --> ")
(read-int-list)
(display "(1 2 3 4 5) <-- expected")(newline)(newline)
(display "Testing (sum-number-list (read-int-list)")(newline)
(display "Input 1 2 3 4 5 q --> ")
(sum-number-list (read-int-list))
(display "15 <-- expected")
(newline)
(display "Testing PairOff & Combiner")(newline)
(display "(PairOff 1 'a) - expected (1 a) - got: ")
(PairOff 1 'a)
(display "(Combiner ‘(1 2 3) ‘(a b c)) - expected: ((1 a) (2 b) (3 c)) - got: ")
(Combiner '(1 2 3) '(a b c))
(newline)
(newline)
(display " o(^▽^)o HAPPY NUMBER TEST o(;△;)o ")
(newline)
(display "Is 863 happy? (happy? 863) - expected: #t - got: ")
(happy? 863)
(display "Is 55562 happy? (happy? 55562) - expected #f - got: ")
(happy? 55562)