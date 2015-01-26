#lang typed/racket

(provide UUID-Storeof
         make-uuid-store
         uuid-store-contains?
         uuid-store-read
         uuid-store-write!
         make-uuid-store-adaptor
         uuid-store-adaptor-encode
         uuid-store-adaptor-decode
         adapt-uuid-store)

(require/typed libuuid
               [uuid-generate (→ String)])

(module+ test
  (require (except-in typed/rackunit test-case))
  (define-syntax-rule (test-case name body ...)
    (begin body ...)))

(struct: (Store A) uuid-store ([storage : Store]
                               [contains?-proc : (→ Store String Boolean)]
                               [read-proc : (→ Store String A)]
                               [write!-proc : (→ Store String A Void)]))

(define-type (UUID-Storeof S A) (uuid-store S A))

(: make-uuid-store (∀ (S A) (→ S
                               (→ S String Boolean)
                               (→ S String A)
                               (→ S String A Void) (UUID-Storeof S A))))
(define (make-uuid-store store contains reader writer)
  (uuid-store store contains reader writer))

(module+ test
  (: make-simple-store (∀ (V) ((HashTable String V) → (UUID-Storeof (HashTable String V) V))))
  (define (make-simple-store (store (make-hash)))
    (#{make-uuid-store @ (HashTable String V) V} store hash-has-key? hash-ref hash-set!)))

(: uuid-store-contains? (∀ (S A) (→ (UUID-Storeof S A) String Boolean)))
(define (uuid-store-contains? store uuid)
  ((uuid-store-contains?-proc store) (uuid-store-storage store) uuid))

(module+ test
  (test-case "uuid-store-contains?"
    (let* ([uuid0 (uuid-generate)]
           [uuid1 (uuid-generate)]
           [store (make-simple-store (make-hash `((,uuid0 . important-thing))))])
      (check-true (uuid-store-contains? store uuid0) "store doesn't have uuid0")
      (check-false (uuid-store-contains? store uuid1) "store contains uuid1"))))

(: uuid-store-read (∀ (S A) (→ (UUID-Storeof S A) String A)))
(define (uuid-store-read store uuid)
  ((uuid-store-read-proc store) (uuid-store-storage store) uuid))

(module+ test
  (test-case "uuid-store-read"
    (let* ([uuid0 (uuid-generate)]
           [uuid1 (uuid-generate)]
           [store (make-simple-store (make-hash `((,uuid0 . important-thing))))])
      (check-eq? (uuid-store-read store uuid0) 'important-thing
                 "store incorrectly reads data")
      (check-exn #rx"hash-ref: no value found for key"
                 (λ () (uuid-store-read store uuid1))
                 "store read nonexistant data"))))

(: uuid-store-write! (∀ (S A) (→ (UUID-Storeof S A) String A Void)))
(define (uuid-store-write! store uuid value)
  ((uuid-store-write!-proc store) (uuid-store-storage store) uuid value))

(module+ test
  (test-case "uuid-store-write!"
    (let* ([uuid0 (uuid-generate)]
           [uuid1 (uuid-generate)]
           [backing-store (make-hash `((,uuid0 . important-thing)))]
           [store (make-simple-store backing-store)])
      (uuid-store-write! store uuid0 'replacement-thing)
      (check-equal? backing-store
                    (make-hash `((,uuid0 . replacement-thing)))
                    "failed to overwrite uuid0")
      (check-false (uuid-store-contains? store uuid1) "modified other uuid")
      (uuid-store-write! store uuid1 'other-thing)
      (check-true (uuid-store-contains? store uuid1) "failed to add uuid1")
      (check-equal? backing-store
                    (make-hash `((,uuid0 . replacement-thing) (,uuid1 . other-thing)))
                    "uuid1 failed to write correctly"))))

(struct: (Src Res) uuid-store-adaptor ([encode-proc : (→ Res Src)]
                                       [decode-proc : (→ Src Res)]))

(: make-uuid-store-adaptor (∀ (Src Res) (→ (→ Res Src) (→ Src Res) (uuid-store-adaptor Src Res))))
(define (make-uuid-store-adaptor encoder decoder)
  (uuid-store-adaptor encoder decoder))

(: uuid-store-adaptor-encode (∀ (Src Res) (→ (uuid-store-adaptor Src Res) Res Src)))
(define (uuid-store-adaptor-encode adaptor res)
  ((uuid-store-adaptor-encode-proc adaptor) res))

(module+ test
  (: string->number* (→ String Number))
  (define (string->number* str)
    (let ([num (string->number str)])
      (assert (number? num))
      num))
  
  (test-case "uuid-store-adaptor-encode"
    (let ([simple-adaptor (make-uuid-store-adaptor number->string string->number*)])
      (check-equal? (uuid-store-adaptor-encode simple-adaptor 22/7)
                    (number->string 22/7)
                    "failed to encode correctly"))))

(: uuid-store-adaptor-decode (∀ (Src Res) (→ (uuid-store-adaptor Src Res) Src Res)))
(define (uuid-store-adaptor-decode adaptor src)
  ((uuid-store-adaptor-decode-proc adaptor) src))

(module+ test
  (test-case "uuid-store-adaptor-decode"
    (let ([simple-adaptor (make-uuid-store-adaptor number->string string->number*)])
      (check-equal? (uuid-store-adaptor-decode simple-adaptor "13/8")
                    13/8
                    "failed to decode correctly"))))

(module+ test
  (test-case "uuid-store-adaptor round trip"
    (let ([simple-adaptor (make-uuid-store-adaptor number->string string->number*)])
      (check-equal? (uuid-store-adaptor-decode simple-adaptor
                                               (uuid-store-adaptor-encode simple-adaptor 22/7))
                    22/7
                    "uuid-store-adaptor doesn't round trip"))))

(: adapt-uuid-store (∀ (S A B) (→ (UUID-Storeof S A) (uuid-store-adaptor A B) (UUID-Storeof S B))))
(define (adapt-uuid-store store adaptor)
  (uuid-store (uuid-store-storage store)
              (uuid-store-contains?-proc store)
              (λ: ([s : S] [uuid : String])
                (#{uuid-store-adaptor-decode @ A B} adaptor
                                                    ((uuid-store-read-proc store) s uuid)))
              (λ: ([s : S] [uuid : String] [value : B])
                ((uuid-store-write!-proc store) s
                                                uuid
                                                (#{uuid-store-adaptor-encode @ A B} adaptor value)))))

(module+ test
  (test-case "adapt-uuid-store"
    (let* ([uuid0 (uuid-generate)]
           [uuid1 (uuid-generate)]
           [store (make-simple-store (make-hash `((,uuid0 . "7"))))]
           [simple-adaptor (make-uuid-store-adaptor number->string string->number*)]
           [store* (adapt-uuid-store store simple-adaptor)])
      (check-equal? (uuid-store-read store* uuid0) 7
                    "adapted store fails to read")
      (uuid-store-write! store* uuid1 55)
      (check-equal? (uuid-store-read store uuid1) "55"
                    "adapted store fails to write"))))