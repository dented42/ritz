#lang racket

(require libuuid
         rackunit
         "../uuid-storage.rkt")


(module+ test
  
  (define (make-simple-store (store (make-hash)))
    (make-uuid-store store hash-has-key? hash-ref hash-set!))
  
  (test-case "uuid-store-contains?"
    (let* ([uuid0 (uuid-generate)]
           [uuid1 (uuid-generate)]
           [store (make-simple-store (make-hash `((,uuid0 . important-thing))))])
      (check-true (uuid-store-contains? store uuid0) "store doesn't have uuid0")
      (check-false (uuid-store-contains? store uuid1) "store contains uuid1")))
  
  (test-case "uuid-store-read"
    (let* ([uuid0 (uuid-generate)]
           [uuid1 (uuid-generate)]
           [store (make-simple-store (make-hash `((,uuid0 . important-thing))))])
      (check-eq? (uuid-store-read store uuid0) 'important-thing
                 "store incorrectly reads data")
      (check-exn #rx"hash-ref: no value found for key"
                 (λ () (uuid-store-read store uuid1))
                 "store read nonexistant data")))
  
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
                    "uuid1 failed to write correctly")))
  

  (test-case "uuid-store-adaptor-encode"
    (let ([simple-adaptor (make-uuid-store-adaptor number->string string->number)])
      (check-equal? (uuid-store-adaptor-encode simple-adaptor 22/7)
                    (number->string 22/7)
                    "failed to encode correctly")))

  (test-case "uuid-store-adaptor-decode"
    (let ([simple-adaptor (make-uuid-store-adaptor number->string string->number)])
      (check-equal? (uuid-store-adaptor-decode simple-adaptor "13/8")
                    13/8
                    "failed to decode correctly")))

  (test-case "uuid-store-adaptor round trip"
    (let ([simple-adaptor (make-uuid-store-adaptor number->string string->number)])
      (check-equal? (uuid-store-adaptor-decode simple-adaptor
                                               (uuid-store-adaptor-encode simple-adaptor 22/7))
                    22/7
                    "uuid-store-adaptor doesn't round trip")))

  (test-case "adapt-uuid-store"
    (let* ([uuid0 (uuid-generate)]
           [uuid1 (uuid-generate)]
           [store (make-simple-store (make-hash `((,uuid0 . "7"))))]
           [simple-adaptor (make-uuid-store-adaptor number->string string->number)]
           [store* (adapt-uuid-store store simple-adaptor)])
      (check-equal? (uuid-store-read store* uuid0) 7
                    "adapted store fails to read")
      (uuid-store-write! store* uuid1 55)
      (check-equal? (uuid-store-read store uuid1) "55"
                    "adapted store fails to write"))))