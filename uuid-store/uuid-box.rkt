#lang typed/racket

(provide UUID-Boxof
         make-uuid-box
         fetch-uuid-box
         uuid-unbox
         uuid-unbox/refresh
         set-uuid-box!)

(require "uuid-storage.rkt")

(require/typed libuuid
               [uuid-generate (→ String)])

(module+ test
  (require (except-in typed/rackunit test-case))
  (define-syntax-rule (test-case name body ...)
    (begin body ...)))

;(module+ test
  (: make-simple-store (∀ (V) ((HashTable String V) → (UUID-Storeof (HashTable String V) V))))
  (define (make-simple-store (store (make-hash)))
    (#{make-uuid-store @ (HashTable String V) V} store hash-has-key? hash-ref hash-set!));)

(struct (A) baked-uuid-store ([read : (→ String A)]
                              [write! : (→ String A Void)]))

(: bake-uuid-store (∀ (S A) (→ (UUID-Storeof S A) (baked-uuid-store A))))
(define (bake-uuid-store store)
  (baked-uuid-store (λ: ([uuid : String])
                      (uuid-store-read store uuid))
                    (λ: ([uuid : String] [value : A])
                      (uuid-store-write! store uuid value))))

(: make-optional (∀ (A) (→ A (Option A))))
(define (make-optional thing)
  thing)

(define-type (UUID-Boxof A) (uuid-box A))

(struct: (A) uuid-box ([uuid : String]
                       [store : (baked-uuid-store A)]
                       [content : (Boxof (Option (Ephemeronof A)))]))

(: make-uuid-box (∀ (S A) (→ (UUID-Storeof S A) A (UUID-Boxof A))))
(define (make-uuid-box store value)
  (let ([uuid (uuid-generate)])
    (begin
      (uuid-store-write! store uuid value)
      (uuid-box uuid
                (bake-uuid-store store)
                (box (make-optional (make-ephemeron value value)))))))

(module+ test
  (test-case "make-uuid-box"
    (fail "write test")))

(: fetch-uuid-box (∀ (S A) (→ (UUID-Storeof S A) String (Option (UUID-Boxof A)))))
(define (fetch-uuid-box store uuid)
  (if (uuid-store-contains? store uuid)
      (uuid-box uuid (bake-uuid-store store) (box (ann #f (Option (Ephemeronof A)))))
      #f))

(module+ test
  (test-case "fetch-uuid-box"
    (let* ([uuid0 (uuid-generate)]
                      [store (make-simple-store (make-hash `((,uuid0 . thing))))]
                      #;[thing-box (fetch-uuid-box:hs store uuid0)])
      (fail "write test"))))

;;; these uuid-box mutating functions might need to be made thread safe in the future...
(: uuid-unbox (∀ (A) (→ (UUID-Boxof A) A)))
(define (uuid-unbox ref-box)
  (let ([outer (unbox (uuid-box-content ref-box))])
    (if outer ; this is sort of like a backwards cond, that would be a fun macro
        (let ([inner (ephemeron-value outer)])
          (if inner
              inner
              (uuid-unbox/refresh ref-box)))
        (uuid-unbox/refresh ref-box))))

(module+ test
  (test-case "uuid-unbox"
    (fail "write test")
    (test-case "uuid-unbox fetched box"
      (fail "write test"))
    (test-case "uuid-unbox new box"
      (fail "write test"))
    (test-case "uuid-boxes reset during garbage collection"
      (fail "write test"))
    (test-case "uuid-boxes don't reset if there isn't a garbage collection"
      (fail "write-test"))))

(: uuid-unbox/refresh (∀ (A) (→ (UUID-Boxof A) A)))
(define (uuid-unbox/refresh ref-box)
  (let ([value ((baked-uuid-store-read (uuid-box-store ref-box)) (uuid-box-uuid ref-box))])
    (set-box! (uuid-box-content ref-box)
              (make-ephemeron value value))
    value))

(module+ test
  (test-case "uuid-unbox/refresh"
    (fail "write test")))

(: set-uuid-box! (∀ (A) (→ (UUID-Boxof A) A Void)))
(define (set-uuid-box! ref-box value)
  ((baked-uuid-store-write! (uuid-box-store ref-box)) (uuid-box-uuid ref-box) value)
  (set-box! (uuid-box-content ref-box) (make-ephemeron value value)))

(module+ test
  (test-case "set-uuid-box!"
    (fail "write test")))