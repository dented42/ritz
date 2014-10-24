#lang typed/racket

(provide make-uuid-store
         uuid-store-contains?
         uuid-store-read
         uuid-store-write!
         make-uuid-store-adaptor
         uuid-store-adaptor-encode
         uuid-store-adaptor-decode
         adapt-uuid-store
         UUID-Boxof
         make-uuid-box
         fetch-uuid-box
         uuid-unbox
         uuid-unbox/refresh
         set-uuid-box!)

(require/typed libuuid
               [uuid-generate (→ String)])

(struct: (Store A) uuid-store ([storage : Store]
                               [contains?-proc : (→ Store String Boolean)]
                               [read-proc : (→ Store String A)]
                               [write!-proc : (→ Store String A Void)]))

(: make-uuid-store (∀ (S A) (→ S
                               (→ S String Boolean)
                               (→ S String A)
                               (→ S String A Void) (uuid-store S A))))
(define (make-uuid-store store contains reader writer)
  (uuid-store store contains reader writer))

(: uuid-store-contains? (∀ (S A) (→ (uuid-store S A) String Boolean)))
(define (uuid-store-contains? store uuid)
  ((uuid-store-contains?-proc store) (uuid-store-storage store) uuid))

(: uuid-store-read (∀ (S A) (→ (uuid-store S A) String A)))
(define (uuid-store-read store uuid)
  ((uuid-store-read-proc store) (uuid-store-storage store) uuid))

(: uuid-store-write! (∀ (S A) (→ (uuid-store S A) String A Void)))
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

(: adapt-uuid-store (∀ (S A B) (→ (uuid-store S A) (uuid-store-adaptor A B) (uuid-store S B))))
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

(struct (A) baked-uuid-store ([read : (→ String A)]
                              [write! : (→ String A Void)]))

(: bake-uuid-store (∀ (S A) (→ (uuid-store S A) (baked-uuid-store A))))
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

(: make-uuid-box (∀ (S A) (→ (uuid-store S A) A (UUID-Boxof A))))
(define (make-uuid-box store value)
  (let ([uuid (uuid-generate)])
    (begin
      (uuid-store-write! store uuid value)
      (uuid-box uuid
                (bake-uuid-store store)
                (box (make-optional (make-ephemeron value value)))))))

(: fetch-uuid-box (∀ (S A) (→ (uuid-store S A) String (Option (UUID-Boxof A)))))
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

