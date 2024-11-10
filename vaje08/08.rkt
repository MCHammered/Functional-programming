#lang racket

;; Podatkovni tipi
(struct int (const) #:transparent)
(struct false ()    #:transparent)
(struct true ()     #:transparent)

;; Operacije in preostale strukture
(struct add (e1 e2)              #:transparent)
(struct mul (e1 e2)              #:transparent)
(struct ?leq (e1 e2)             #:transparent)
(struct ~ (a)                    #:transparent)
(struct ?int (a)                 #:transparent)
(struct if-then-else (arg e1 e2) #:transparent)

;; Makra
(define-syntax ifte
  (syntax-rules (then else)
    [(ifte e1 then e2 else e3) (if-then-else e1 e2 e3)]))

(define-syntax ?geq
  (syntax-rules ()
    [(?geq e1 e2) (?leq e2 e1)]))



;; Interpreter
(define (fri e)
  (cond [(int? e) e]
        [(false? e) e]
        [(true? e) e]

        ; Seštevanje in konjunkcija
        [(add? e)
         (let ([v1 (fri (add-e1 e))]
               [v2 (fri (add-e2 e))])
           (cond
               [(and (int? v1) (int? v2)) (int (+ (int-const v1) (int-const v2)))]
               [(and (false? v1) (false? v2)) (false)]
               [(and (false? v1) (true? v2)) (true)]
               [(and (true? v1) (false? v2)) (true)]
               [(and (true? v1) (true? v2)) (true)]
               [#t (error "Napačni add argumenti!")]))]

        ; Množenje in disjunkcija
        [(mul? e)
         (let ([v1 (fri (mul-e1 e))]
               [v2 (fri (mul-e2 e))])
           (cond
               [(and (int? v1) (int? v2)) (int (* (int-const v1) (int-const v2)))]
               [(and (false? v1) (false? v2)) (false)]
               [(and (false? v1) (true? v2)) (false)]
               [(and (true? v1) (false? v2)) (false)]
               [(and (true? v1) (true? v2)) (true)]
               [#t (error "Napačni mul argumenti!")]))]

        ; Less or equal in implikacija
        [(?leq? e)
         (let ([v1 (fri (?leq-e1 e))]
               [v2 (fri (?leq-e2 e))])
           (cond
             [(and (int? v1) (int? v2)) (if (<= (int-const v1) (int-const v2)) (true) (false))]
             [(and (false? v1) (false? v2)) (true)]
             [(and (false? v1) (true? v2)) (true)]
             [(and (true? v1) (false? v2)) (false)]
             [(and (true? v1) (true? v2)) (true)]
             [#t (error "Napačni ?leq argumenti!")]))]

        ; Negacija
        [(~? e)
         (let ([v1 (fri (~-a e))])
           (cond
             [(int? v1) (int (- (int-const v1)))]
             [(false? v1) (true)]
             [(true? v1) (false)]
             [#f (error "Napačni ~ argumenti!")]))]

        ; Ali je tipa int
        [(?int? e)
         (let ([v1 (fri (?int-a e))])
           (if (and (int? v1) (integer? (int-const v1))) (true) (false)))]

        ; Lastni if stavek
        [(if-then-else? e)
         (let ([v1 (fri (if-then-else-arg e))])
           (cond
             [(true? v1) (fri (if-then-else-e1 e))]
             [(false? v1) (fri (if-then-else-e2 e))]
             [#t (error "Argument ni tipa (true) ali (false)!")]))]
        ))


               



