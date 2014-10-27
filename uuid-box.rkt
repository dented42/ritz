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

(struct (A) baked-uuid-store ([read : (→ String A)]
                              [write! : (→ String A Void)]))

(: bake-uuid-store (∀ (S A) (→ (UUID-Storeof S A) (baked-uuid-store A))))
(define (bake-uuid-store store)
  (baked-uuid-store (λ: ([uuid : String])
                      (uuid-store-read store uuid))
                    (λ: ([uuid : String] [value : A])
                      (uuid-store-write! store uuid value))))

(define-type (UUID-Boxof A) (uuid-box A))

(: make-optional (∀ (A) (→ A (Option A))))
(define (make-optional thing)
  thing)

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

(: fetch-uuid-box (∀ (S A) (→ (UUID-Storeof S A) String (Option (UUID-Boxof A)))))
(define (fetch-uuid-box store uuid)
  (if (uuid-store-contains? store uuid)
      (uuid-box uuid (bake-uuid-store store) (box (ann #f (Option (Ephemeronof A)))))
      #f))

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

(: uuid-unbox/refresh (∀ (A) (→ (UUID-Boxof A) A)))
(define (uuid-unbox/refresh ref-box)
  (let ([value ((baked-uuid-store-read (uuid-box-store ref-box)) (uuid-box-uuid ref-box))])
    (set-box! (uuid-box-content ref-box)
              (make-ephemeron value value))
    value))

(: set-uuid-box! (∀ (A) (→ (UUID-Boxof A) A Void)))
(define (set-uuid-box! ref-box value)
  ((baked-uuid-store-write! (uuid-box-store ref-box)) (uuid-box-uuid ref-box) value)
  (set-box! (uuid-box-content ref-box) (make-ephemeron value value)))