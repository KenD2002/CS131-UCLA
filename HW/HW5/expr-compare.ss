#lang racket

;;; Part 1
;;; implement for expr-compare

;;; lambda?/1 checks if a symbol is 'lambda or 'λ
(define (lambda? x) 
    (if (member x '(lambda λ))
        #t
        #f)
)

;;; expr-compare/2 compares two arguments x and y, which are two expressions
(define (expr-compare x y)
    (cond
        ; there are four different conditions:

        ; 1. if x and y are the same expression, i.e. no difference, return x
        [(equal? x y) 
            x]

        ; 2. if x and y are different booleans, return %/(not %) depend on x's value
        [(and (boolean? x) (boolean? y))
            (if x '% '(not %))]

        ; 3. if one of x,y is not a list OR they are of different lengths, e.g. x='a , y='(cons a b),
        ;    we return the form '(if '% x y)
        [(or (or (not (list? x)) (not (list? y))) (not (equal? (length x) (length y))))
            (list 'if '% x y)]

        ; 4. if x and y are both lists, and they have the same lengths:
        [else 
            (expr-compare-list x y)]
    )
)


;;; auxiliary function expr-compare-list/2 for checking the 4th condition in expr-compare/2
(define (expr-compare-list x y)
    (cond
        ; there are five conditions:

        ; 1. if either of x or y starts with "quote" or "'", return the form '(if '% x y)
        [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
            (list 'if '% x y)]

        ; 2. if both of x or y starts with "lambda" or "λ"
        [(and (lambda? (car x)) (lambda? (car y)))
            (if (or (equal? (car x) 'λ) (equal? (car y) 'λ))
                ; 2.1 if at least one of them is starting with greek letter λ
                (expr-compare-lambda (cdr x) (cdr y) 'λ '() '())
                ; 2.2 if both of them are starting with "lambda"
                (expr-compare-lambda (cdr x) (cdr y) 'lambda '() '())
            )]

        ; 3. if only one of x or y starts with "lambda" or "λ", return the form '(if '% x y)
        [(or (lambda? (car x)) (lambda? (car y)))
            (list 'if '% x y)]

        ; 4. if only one of x or y starts with "if", return the form '(if '% x y)
        [(not (equal? (equal? (car x) 'if) (equal? (car y) 'if)))
            (list 'if '% x y)]

        ; 5. all other cases
        [else
            (expr-compare-default x y)]
    )
)


;;; auxiliary function expr-compare-default/2 for checking the 5th condition in expr-compare-list/2
;;; this function recursively compares the elements in x and y one-by-one
(define (expr-compare-default x y)
    (cond
        ; base case: x is empty list '(), return '()
        [(equal? x '())
            '()]

        ; if the current element of x and y are the same, recursively compare the next one
        [(equal? (car x) (car y))
            (cons (car x) (expr-compare-default (cdr x) (cdr y)))]

        ; if the current element of x and y are not the same, use expr-compare/2 to compare
        ; the current element, and use recursively compare the rest using expr-compare-default/2
        [else
            (cons (expr-compare (car x) (car y)) (expr-compare-default (cdr x) (cdr y)))]
    )
)


;;; auxiliary function expr-compare-lambda/5 for checking the 3rd condition in expr-compare-list/2
;;; it uses lambda-args to construct the arguments of the lambda function
;;; it uses lambda-fun to construct the function body of the lambda function
(define (expr-compare-lambda x y lambda xdicts ydicts)
    (list
        lambda
        (lambda-args (car x) (car y))
        (lambda-fun (car (cdr x)) (car (cdr y))
            (cons (build-dict (car x) (car y) #t) xdicts)
            (cons (build-dict (car x) (car y) #f) ydicts))
    )
)


;;; auxiliary function lambda-args/2 for construct the arguments of the lambda function in expr-compare-lambda/5
(define (lambda-args x-args y-args)
    (cond
        ; base case: x-args is empty list '(), return '()
        [(equal? x-args '()) '()]

        ; if the current element of x and y are the same, recursively compare the next one
        [(equal? (car x-args) (car y-args))
            (cons (car x-args) (lambda-args (cdr x-args) (cdr y-args)))]

        ; if the current element of x and y are not the same, first get a unification and then append
        [else
            (cons (unification (car x-args) (car y-args)) (lambda-args (cdr x-args) (cdr y-args)))]
    )
)


;;; auxiliary function unification/2 for construct the symbol unification for lambda-args/2
(define (unification arg1 arg2)
    (string->symbol (string-append (symbol->string arg1) "!" (symbol->string arg2)))
)


;;; auxiliary function lambda-fun/4 for construct the lambda function body of the in expr-compare-lambda/5
(define (lambda-fun x y xdicts ydicts)
    (let
        ([newx (if (equal? (check-dicts xdicts x) "failure") 
            x 
            (check-dicts xdicts x))]
         [newy (if (equal? (check-dicts ydicts y) "failure") 
            y
            (check-dicts ydicts y))])

        (cond
            ; there are five conditions:
        
            ; 1. if the original/mapped values are the same, we're done
            [(equal? newx newy)
                newx]

            ; 2. both x and y are lists, use expr-compare recursively
            [(and (list? x) (list? y))
                (expr-compare (apply-dicts xdicts x #t) (apply-dicts ydicts y #t))]

            ; 3. only x is list, use expr-compare recursively
            [(list? x)
                (expr-compare (apply-dicts xdicts x #t) newy)]

            ; 4. only y is list, use expr-compare recursively
            [(list? y)
                (expr-compare x (apply-dicts ydicts y #t))]

            ; 5. other conditions, use expr-compare recursively
            [else
                (expr-compare newx newy)]
        )
    )
)
  

;;; auxiliary function build-dict/3 building dict mapping x->(x!y) or y->(x!y)
(define (build-dict x y mapping-x?)
    (cond
        ; base case
        [(equal? x '()) 
            (hash)]

        ; different - add mapping to dict
        [(not (equal? (car x) (car y)))
            (if mapping-x? 
                (hash-set (build-dict (cdr x) (cdr y) mapping-x?) (car x) (unification (car x) (car y)))
                (hash-set (build-dict (cdr x) (cdr y) mapping-x?) (car y) (unification (car x) (car y))))]
        
        ; equal - no mapping needed
        [else (build-dict (cdr x) (cdr y)
            mapping-x?)]
    )
)


;;; auxiliary function identity-dict/1 builds a dict mapping to itself
(define (identity-dict x)
    (if (equal? x '())
        (hash)  
        (hash-set (identity-dict (cdr x)) (car x) (car x))
    )
)


;;; auxiliary function check-dicts/2 checks value in dicts and prioritized by most recent definition
(define (check-dicts dicts k)
    (cond
        ; reached end of dicts without finding match
        [(empty? dicts)
            "failure"]

        ; if current dict fails, check the next
        [(equal? (hash-ref (car dicts) k "failure") "failure")
            (check-dicts (cdr dicts) k)]

        ; found in current dict
        [else
            (hash-ref (car dicts) k)]
    )
)


;;; auxiliary function apply-dicts/3 apply dictionary mappings to a list
(define (apply-dicts dicts x first?)
  (cond
    ; base case
    [(equal? x '()) 
        '()]

    ; don't replace booleans
    [(boolean? (car x))
        (cons (car x) (apply-dicts dicts (cdr x) #f))]

    ; if first element is 'quote, don't apply
    [(and first? (equal? (car x) 'quote))
        x]

    ; first element is if
    [(and first? (equal? (car x) 'if))
        (cons 'if (apply-dicts dicts (cdr x) #f))]

    ; first element is lambda - we prioritize the dict for the new lambda and apply all mappings
    [(and first? (lambda? (car x)))
       (cons (car x)
             (cons (car (cdr x))
             (apply-dicts (cons (identity-dict (car (cdr x))) dicts) (cdr (cdr x)) #t)))]

    ; recursively apply dict inside lists
    [(list? (car x))
        (cons (apply-dicts dicts (car x) #t) (apply-dicts dicts (cdr x) #f))]
    
    ; check-dicts and replace
    [else
        (cons (if (equal? (check-dicts dicts (car x)) "failure") 
                (car x) 
                (check-dicts dicts (car x)))
            (apply-dicts dicts (cdr x) #f))]
    )
)



;;; Part 2
;;; implement for test-expr-compare

(define (test-expr-compare x y) 
    (and (equal? (eval x) (eval `(let ((% #t)) ,(expr-compare x y))))
         (equal? (eval y) (eval `(let ((% #f)) ,(expr-compare x y))))
    )
)



;;; Part 3: 
;;; implement for test-expr-x and test-expr-y

(define test-expr-x '(lambda (a b) (lambda (a d) (if d #t (quote a)))))
(define test-expr-y '(lambda (b a) (lambda (c d) (if d #f (quote b)))))