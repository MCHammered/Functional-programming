#lang racket

;; Primer vrstičnega komentarja.

;; - veži število 5 v spremenljivko `pet`
; <rešitev>
(define pet 5)

;; pazi
(define _ 4)

;; Racket je dinamično tipiziran.
;; - preveri ali je `pet` število/celo število
; <rešitev>
(number? pet)
; <rešitev>
(integer? pet)

;;  == Uporabne bljižnice ==
;; - Autocomplete `ctrl + /`
;; - Poravnava `ctrl + i`
;; - "Run" `ctrl-r`
;; - Nastavitve `Edit -> Keybindings`
;; - Primer nastavitev (`ctrl + a : Run` in `ctrl + w : delete word`) v datoteki [key-bindings.rkt]

;;   v interpreterju:
;; 
;; - Navigacija po zgodovini: `ctrl + ↑`, `ctrl + ↓`
;; - Skoči na konec: `↓`
;; - Interpretiraj vnešen izraz: `ctrl + enter`

;; - veži število 1/5 v spremenljivko `petina` na dva načina.
; <rešitev>
(define petina (/ 1 5)) ; ali (/ 5)
; <rešitev>

;; Poglej dokumentacijo za funkcijo `/`
;; Izračunaj koliko let imaš, če si star 200000 ur.
; <rešitev>
(/ 200000 24 356)

;; - shrani vrednost meta (6-strane) kocke v spremenljivko `kocka`
;;   Uporabi `random`
; <rešitev>
(define kocka (random 1 6))

;; - veži anonimno funkcijo v spremenljivko `anonimna`,
;;   ki vrne vrednost spremenljivke `kocka`
; <rešitev>
(define anonimna (lambda () kocka)) ; ()... sprejme 0 arg., (_)... sprejme 1 arg.
; <rešitev>
(define (poimenovana) kocka)

;; nekaj dodantih primerov anonimnih funkcij
(define f0 (λ () 1))
(define f1 (λ (a) (- a)))
(define f2 (λ (a b) (+ a b)))
(define f3 (λ a (rest a)))
(define f4 (λ _ -1))
(define f5 (λ (a . _) (+ a a)))
(define f6 (λ (a _ b . c) (+ a b)))


;; - definiranj funkcijo vrži-kocko
; <rešitev>
(define (vrzi-kocko) (random 1 7))

;; - definiraj funkcijo `add`, ki zna sešteti dve števili
; <rešitev>
(define (add a b) (+ a b))

;; - Kakšna je razlika med `addc`, `addc2`, `addc3`?
(define addc
  (λ (x) (λ (y) (+ x y)))) ; klasicni currying
(define (addc2)
  (λ (x) (λ (y) (+ x y)))) ; vrne curry funkcijo
(define ((addc3 x) y) (+ x y)) ; olepsava addc
(define ((addc4 x y) z) (+ x y z))

;; - definiranj funkcijo `n-krat-vrži-kocko` (vrne vsoto n-tih metov)
; <rešitev>
(define (n-krat-vrzi-kocko n)
  (if (zero? n)                             ; ali pa (= n 0)
      0
      (+ (vrzi-kocko) (n-krat-vrzi-kocko (sub1 n))))) ; ali pa (- n 1)

;; - definiranj funkcijo `seznam-n-metov` (vrne seznam n-tih metov)
;; Prazen seznam: '(), null, (list)
;; Seznam z enim elementom `e`: (cons e null), (list e).
;; Seznam je zaporedje parov.
; <rešitev>
(define (seznam-n-metov n)
  (cond [(zero? n) '()]
        [else (cons (vrzi-kocko) (seznam-n-metov (sub1 n)))]))

;; alternativa
; <rešitev>
(define (seznam-n-metov2 n)
  (build-list n (lambda _ (vrzi-kocko))))

;; - definiranj funkcijo `n-krat-vrži-kocko2` z uporabo
;;   funkcije `seznam-n-metov`
; <rešitev>
(define (n-krat-vrzi-kocko2 n)
  (apply + (seznam-n-metov2 n)))

; <rešitev>
(define (n-krat-vrzi-kocko2-2 n)
  (foldl + 0 (seznam-n-metov2 n)))


;; - definiranj funkcijo `rev` z uporabo akumulatorjev
; define
; <rešitev>


;(define (rev s)
; (define (rev s acc)
;   (cond [(null? s) acc]
;         [else (rev (rest s) (cons (first s) acc))])))

; letrec
; <rešitev>
(define (rev2 s)
  (letrec
      ([rev (lambda (s acc)
                  (cond [(null? s) acc]
                        [else (rev (rest s) (cons (first s) acc))]))])
    (rev s null)))

; let
; <rešitev>
(define (rev3 s)
  (let rev
    ([s s]
     [acc null])
    (cond [(null? s) acc]
                        [else (rev (rest s) (cons (first s) acc))])))


; foldl
; <rešitev>
(define (rev4 s)
  (foldl cons null s))

; match
; <rešitev>
(define (rev5 s)
  (define (rev s acc)
    (match s
      ['() acc]                               ; ne gre z null
      [(cons h t) (rev t (cons h acc))]))
  (rev s null))

;; - definiranj funkcijo `preštej`,
;;   ki prešteje število argumentov
; <rešitev>
; <rešitev>
; <rešitev>



;; - definiranj funkcijo `skalarni-produkt`:
;; (skalarni-produkt (list 1 -1 1) (list 2 -4 -4)) -> 2
; <rešitev>

;; - definiranj funkcijo `binary`:
;; (binary 0) -> 0
;; (binary 1 0 1 0) -> 10
;; (binary 1 0 1 0 1 0) -> 42
; <rešitev>

;; definiranj funkcijo `tetration a n`
;; https://en.wikipedia.org/wiki/Tetration
; <rešitev>

;; i^0, i^i, i^(i^i), ... (tetration i n)

;; risanje grafov
; (require plot)
; (parameterize ([plot-width    800]
;                [plot-height   800]
;                [plot-x-label  "re(x)"]
;                [plot-y-label  "im(x)"])
;   (define xs (build-list 40 (λ (i) (tetration 0+i i))))
;   (plot (points (map (λ (a) (vector (real-part a) (imag-part a))) xs))
;         #:x-min -0.1 #:x-max 1.1
;         #:y-min -0.1 #:y-max 1.1))