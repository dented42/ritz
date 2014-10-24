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
                    [store (make-simple-store (make-hash `((,uuid0 . important-thing))))])
               (uuid-store-write! store uuid0 'replacement-thing)
               (check-eq? (uuid-store-read store uuid0) 'replacement-thing
                          "failed to overwrite uuid0")
               (check-false (uuid-store-contains? store uuid1) "modified other uuid")
               (uuid-store-write! store uuid1 'other-thing)
               (check-true (uuid-store-contains? store uuid1) "failed to add uuid1")
               (check-eq? (uuid-store-read store uuid1) 'other-thing
                          "uuid1 failed to write correctly")
               (check-eq? (uuid-store-read store uuid0) 'replacement-thing
                          "writing uuid1 changed uuid0")))
  
  (test-case "make-uuid-store-adaptor"
             (fail "write test"))
  
  (test-case "uuid-store-adaptor-encode"
             (fail "write test"))
  
  (test-case "uuid-store-adaptor-decode"
             (fail "write test"))
  
  (test-case "adapt-uuid-store"
             (fail "write test"))
  
  (test-case "make-uuid-box"
             (fail "write test"))
  
  (test-case "fetch-uuid-box"
             (fail "write test"))
  
  (test-case "uuid-unbox"
             (fail "write test"))
  
  (test-case "uuid-unbox/refresh"
             (fail "write test"))
  
  (test-case "set-uuid-box!"
             (fail "write test")))