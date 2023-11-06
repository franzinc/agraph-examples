(require :agraph)

(in-package :db.agraph.user)

(close-all-triple-stores)

;;;------------------------------------------------
;; Create triple-store on remote AGraph server.
;;;------------------------------------------------
(defparameter ground-ts
  (create-triple-store "test-remote"
                       :triple-store-class 'remote-triple-store
                       :server "localhost" :port 10035
                       :user "test" :password "xyzzy"))

;; As with a local triple-store, the *db* var is set:
*db*

(close-triple-store)

(defparameter ground-ts
  (open-triple-store "test-remote"
                     :triple-store-class 'remote-triple-store
                     :server "localhost" :port 10035
                     :user "test" :password "xyzzy"))

;; Most other functions work the same as with a local triple-store.
;; For example:

(register-namespace 
 "ex" "http://www.franz.com/things#" 
 :errorp nil)

(display-namespaces)

;; examples copied from reasoner-tutorial.cl

(add-triple !ex:jans !ex:owns !ex:birra)

(add-triple !ex:jans !owl:sameAs !ex:jannes)

;; no reasoning, ask about ex:owns and get one triple
(get-triples-list :p !ex:owns)

(defparameter inferred-ts (apply-rdfs++-reasoner :remote-reasoning t
                                                 :db ground-ts))

;; we'll get two triples now
(get-triples-list :p !ex:owns)

;; using the :db argument
(get-triples-list :p !ex:owns :db ground-ts)
(get-triples-list :p !ex:owns :db (ground-triple-store inferred-ts))

(close-all-triple-stores)
