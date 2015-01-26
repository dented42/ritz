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

(: uuid-store-contains? (∀ (S A) (→ (UUID-Storeof S A) String Boolean)))
(define (uuid-store-contains? store uuid)
  ((uuid-store-contains?-proc store) (uuid-store-storage store) uuid))

(: uuid-store-read (∀ (S A) (→ (UUID-Storeof S A) String A)))
(define (uuid-store-read store uuid)
  ((uuid-store-read-proc store) (uuid-store-storage store) uuid))

(: uuid-store-write! (∀ (S A) (→ (UUID-Storeof S A) String A Void)))
(define (uuid-store-write! store uuid value)
  ((uuid-store-write!-proc store) (uuid-store-storage store) uuid value))

(struct: (Src Res) uuid-store-adaptor ([encode-proc : (→ Res Src)]
                                       [decode-proc : (→ Src Res)]))

(: make-uuid-store-adaptor (∀ (Src Res) (→ (→ Res Src) (→ Src Res) (uuid-store-adaptor Src Res))))
(define (make-uuid-store-adaptor encoder decoder)
  (uuid-store-adaptor encoder decoder))

(: uuid-store-adaptor-encode (∀ (Src Res) (→ (uuid-store-adaptor Src Res) Res Src)))
(define (uuid-store-adaptor-encode adaptor res)
  ((uuid-store-adaptor-encode-proc adaptor) res))

(: uuid-store-adaptor-decode (∀ (Src Res) (→ (uuid-store-adaptor Src Res) Src Res)))
(define (uuid-store-adaptor-decode adaptor src)
  ((uuid-store-adaptor-decode-proc adaptor) src))

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