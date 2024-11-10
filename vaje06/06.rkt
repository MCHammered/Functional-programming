#lang racket

; power, ki sprejme število x in potenco n, ter vrne n-to potenco števila x (0 je veljavna potenca).

(define (power x n)
  (cond [(zero? n) 1]
        [(* x (power x (sub1 n)))]))

; gcd, ki sprejme dve števili, in vrne njun največji skupni delitelj.

(define (gcd a b)
    (cond [(> a b) (gcd b (- a b))]
          [(< a b) (gcd a (- b a))]
          [else a]))

; fib, ki sprejme število n in vrne n-to Fibonaccijevo število.

(define (fib n)
  (cond [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (sub1 n)) (fib (- n 2)))]))

; reverse, ki sprejme seznam in vrne obrnjen seznam (za dodajanje na konec seznama lahko uporabite vgrajeno funkcijo append)

(define (reverse s)
  (foldl cons null s))

; remove, ki sprejme element x in seznam, ter vrne nov seznam, ki je enak kot vhodni, le da so v njem odstranjene vse pojavitve elementa x.

(define (remove x sez)
  (cond [(null? sez) '()]
        [(= x (first sez)) (remove x (rest sez))]
        [(cons (first sez) (remove x (rest sez)))]))

; map, ki sprejme funkcijo in seznam ter vrne seznam rezultatov, ki jih vrne podana funkcija, če jo zapovrstjo kličemo na elementih vhodnega seznama.

(define (map fun sez)
  (cond [(null? sez) '()]
        [(cons (fun (first sez)) (map fun (rest sez)))]))

; filter, ki sprejme funkcijo in seznam ter vrne seznam, ki vsebuje vse elemente vhodnega seznama, za katere podana funkcija vrne resnično vrednost.

(define (filter fun sez)
  (cond [(null? sez) '()]
        [(if (fun (first sez)) (cons (first sez) (filter fun (rest sez))) (filter fun (rest sez)))]))

; zip, ki sprejme dva seznama, vrne pa seznam parov, ki je tako dolg, kot krajši izmed vhodnih seznamov.
;  Prvi element izhodnega seznama vsebuje par prvih števil vhodnih seznamov, drugi element par drugih števil, ...

(define (zip sez1 sez2)
  (cond [(null? sez1) '()]
        [(null? sez2) '()]
        [else (cons (cons (first sez1) (first sez2)) (zip (rest sez1) (rest sez2)))]))

; range, ki sprejme tri števila, začetek, konec in korak. Vrne seznam števil, ki se začne s številom začetek,
;  vsako naslednje število pa je za korak večje od prejšnjega. Največje število v seznamu je manjše ali enako od števila konec.
;  Korak bo vedno pozitiven, konec pa vedno večji od začetka.

(define (range a b c)
  (cons a
        (cond [(> (+ a c) b) '()]
              [else (range (+ a c) b c)])))

; is-palindrome, ki sprejme seznam ter vrne true, če je seznam palindrom in false v nasprotnem primeru.

(define (is-palindrome sez)
  (equal? sez (reverse sez)))