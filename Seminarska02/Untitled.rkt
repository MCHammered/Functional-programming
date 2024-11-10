#lang racket

;; Podatkovni tipi
(struct true ()         #:transparent)
(struct false ()        #:transparent)
(struct int (const)     #:transparent)
(struct .. (e1 e2)      #:transparent)
(struct empty ()        #:transparent)
(struct exception (exn) #:transparent)

;;

;; Strukture in operacije
(struct trigger (e) #:transparent)
(struct triggered (e) #:transparent)
(struct handle (e1 e2 e3) #:transparent)
(struct if-then-else (condition e1 e2) #:transparent)

(struct ?int (e) #:transparent)
(struct ?bool (e) #:transparent)
(struct ?.. (e) #:transparent)
(struct ?seq (e) #:transparent)
(struct ?empty (e) #:transparent)
(struct ?exception (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct head (e) #:transparent)
(struct tail (e) #:transparent)
(struct ~ (e) #:transparent)
(struct ?any (e) #:transparent)
(struct ?all (e) #:transparent)
(struct ?= (e1 e2) #:transparent)

; (?int e), (?bool e), (?.. e), (?seq e), (?empty e), (?exception e): Funkcije vračajo (true),
;če je rezultat izraza e ustreznega tipa. V primeru (?seq e1) je rezultat (true) le, če se podano zaporedje konča z (empty).

(define (fri expression environment)
  (letrec ([friq (lambda (e env)
                   (cond
                     [(true? e) (true)]
                     [(false? e) (false)]
                     [(int? e) (if (integer? (int-const e)) e (error "not const!"))]
                     [(..? e) (.. (friq (..-e1 e) env) (friq (..-e2 e) env))]
                     [(empty? e) (empty)]
                     [(exception? e) (if (string? (exception-exn e)) e (error "fuck you"))]
                     [(triggered? e) e]
                     ; Operacije
                     [(trigger? e) (if (exception? (friq (trigger-e e) env)) (triggered (trigger-e e)) (triggered (exception "trigger: wrong argument type")))]
                     [(handle? e)
                      (let ([v1 (friq (handle-e1 e) env)]
                            [v2 (friq (handle-e2 e) env)]
                            [v3 (friq (handle-e3 e) env)])
                        (if (triggered? v1) v1 (if (not (exception? v1)) (triggered (exception "handle: wrong argument type"))
                                                   (if (not (triggered? v2)) (triggered (exception "handle: wrong argument type"))
                                                       (if (eq? (exception-exn v1) (exception-exn (triggered-e v2))) v3 v2)))))]
                     ; Lovljenje izjem
                     [(if-then-else? e)
                      (let ([v1 (friq (if-then-else-condition e) env)])
                        (if (false? v1) (friq (if-then-else-e2 e) env) ((friq (if-then-else-e1 e) env))))]
                     ; Preverjanje tipov
                     [(?int? e)
                      (let ([v1 (friq (?int-e e) env)])
                        (if (and (int? v1) (integer? (int-const v1))) (true) (false)))]
                     [(?bool? e)
                      (let ([v1 (friq e env)])
                        (if (or (true? v1) (false? v1)) (true) (false)))]
                      [(?..? e)
                       (let ([v1 (friq (?..-e e) env)])
                         (if (..? v1) (true) (false)))]
                      [(?seq? e)
                       (let ([v1 (friq (?seq-e e) env)])
                         (if (..? v1) (friq (?seq (..-e2 v1)) env) (if (empty? v1) (true) (false))))]
                      [(?empty? e)
                       (let ([v1 (friq (?empty-e e) env)])
                         (if (empty? v1) (true) (false)))]
                      [(?exception? e)
                       (let ([v1 (friq (?exception-e e) env)])
                         (if (exception? v1) (true) (false)))]
                      ; Sestevanje
                      [(add? e)
                       (let ([v1 (friq (add-e1 e) env)]
                             [v2 (friq (add-e2 e) env)])
                         (cond
                           [(and (int? v1) (int? v2)) (int (+ (int-const v1) (int-const v2)))]
                           [(and (false? v1) (false? v2)) (false)]
                           [(and (false? v1) (true? v2)) (true)]
                           [(and (true? v1) (false? v2)) (true)]
                           [(and (true? v1) (true? v2)) (true)]
                           [(and (true? (friq (?seq v1) env)) (true? (friq (?seq v2) env))) (if (empty? v1) v2 (.. (..-e1 v1) (friq (add (..-e2 v1) v2) env)))]
                           [#t (friq (trigger (exception "add: wrong argument type")) env)]))]
                      ; Mnozenje
                      [(mul? e)
                       (let ([v1 (fri (mul-e1 e))]
                             [v2 (fri (mul-e2 e))])
                         (cond
                           [(and (int? v1) (int? v2)) (int (* (int-const v1) (int-const v2)))]
                           [(and (false? v1) (false? v2)) (false)]
                           [(and (false? v1) (true? v2)) (false)]
                           [(and (true? v1) (false? v2)) (false)]
                           [(and (true? v1) (true? v2)) (true)]
                           [#t (friq (trigger (exception "mul: wrong argument type")) env)]))]
                      ; Primerjanje
                      [(?leq? e)
                       (let ([v1 (friq (?leq-e1 e) env)]
                             [v2 (friq (?leq-e2 e) env)])
                         (cond
                           [(and (int? v1) (int? v2)) (if (<= (int-const v1) (int-const v2)) (true) (false))]
                           [(and (false? v1) (false? v2)) (true)]
                           [(and (false? v1) (true? v2)) (true)]
                           [(and (true? v1) (false? v2)) (false)]
                           [(and (true? v1) (true? v2)) (true)]
                           [(and (true? (friq (?seq v1) env)) (true? (friq (?seq v2) env)))
                            (cond
                              [(and (empty? v1) (not (empty? v2))) (true)]
                              [(and (not (empty? v1)) (empty? v2)) (false)]
                              [(and (empty? v1) (empty? v2)) (true)]
                              [#t (friq (?leq (..-e2 v1) (..-e2 v2)))])]
                           [#t (friq (trigger (exception "?leq: wrong argument type")) env)]))]
                      ; Ujemanje
                      [(?=? e)
                       (let ([v1 (friq (?=-e1 e) env)]
                             [v2 (friq (?=-e2 e) env)])
                         (cond
                           [(and (int? v1) (int? v2)) (if (eq? (int-const v1) (int-const v2)) (true) (false))]
                           [(and (false? v1) (false? v2)) (true)]
                           [(and (false? v1) (true? v2)) (false)]
                           [(and (true? v1) (false? v2)) (false)]
                           [(and (true? v1) (true? v2)) (true)]
                           [(and (..? v1)  (..? v2))
                            (cond
                              [(and (empty? v1) (empty? v2)) (true)]
                              [(or (and (not (empty? v1)) (empty? v2)) (and (not (empty? v2)) (empty? v1))) (false)]
                              [(true? (friq (?= (..-e1 v1) (..-e1 v2)) env)) (friq (?= (..-e2 v1) (..-e2 v2)) env)])]
                           [#t (false)]))]
                      ; Ekstrakcija head/tail
                      [(head? e)
                       (let ([v1 (friq (head-e e) env)])
                         (cond
                           [(empty? v1) (friq (trigger (exception "head: empty sequence")) env)]
                           [(..? v1) (..-e1 v1)]
                           [#t (friq (trigger (exception "head: wrong argument type")) env)]))]
                      [(tail? e)
                       (let ([v1 (friq (tail-e e) env)])
                         (cond
                           [(empty? v1) (friq (trigger (exception "tail: empty sequence")) env)]
                           [(..? v1) (..-e2 v1)]
                           [#t (friq (trigger (exception "tail: wrong argument type")) env)]))]
                      ; Negacija
                      [(~? e)
                       (let ([v1 (friq (~-e e))])
                         (cond
                           [(int? v1) (int (- (int-const v1)))]
                           [(false? v1) (true)]
                           [(true? v1) (false)]
                           [(friq (trigger (exception "~: wrong argument type")) env)]))]
                      ; Operatorja all/any
                      [(?all? e)
                       (let ([v1 (friq (?all-e e) env)])
                         (cond
                           [(..? v1) (if (false? (..-e1 v1)) (false) (friq (?all (..-e2 v1)) env))]
                           [(empty? v1) (true)]
                           [#t (friq (trigger (exception "?all: wrong argument type")) env)]))]
                      [(?any? e)
                       (let ([v1 (friq (?any-e e) env)])
                         (cond
                           [(..? v1) (if (true? (..-e1 v1)) (true) (friq (?any (..-e2 v1)) env))]
                           [(empty? v1) (false)]
                           [#t (friq (trigger (exception "?any: wrong argument type")) env)]))]

                     ))])
    
    (friq expression environment)))
                     
                     
                   
