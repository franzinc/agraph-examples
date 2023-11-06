;; -*- mode: common-lisp; package: db.agraph.tests; readtable: allegrograph -*-

;; copyright (c) 2006-2016 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; load allegrograph code
  (require :agraph)
  
  (db.agraph:enable-readtable :allegrograph)
  )

;; Switch to the AllegrGraph "user" package  
(in-package :db.agraph.user)
 
;; you'll need to change this to match where your tutorial files are located.
(defparameter *tutorial-directory* "../tutorial/")

;; turn on the convenience !-reader  
(enable-!-reader)  
 
;; tell AllegroGraph to print triples more readably  
(enable-print-decoded t)

;; List the catalogs by name (the root catalog will be named by `nil`).
(catalog-names)

;;;------------------------------------------------
;; create new triple-store (overwrite existing 
;; AllegroGraph files if any)
;;;------------------------------------------------
(create-triple-store "ag-test")

;; When you're done with a triple-store, you can close it
(close-triple-store)

;; re-open it
(open-triple-store "ag-test")

;;;------------------------------------------------
;;; Namespaces
;;;------------------------------------------------
(register-namespace 
 "ex" "http://www.franz.com/things#" 
 :errorp nil)

(display-namespaces)

;;;------------------------------------------------
;;; Parts, nodes, UPIs
;;;------------------------------------------------
!ex:Dog
!<http://www.franz.com/things#Dog>
!"Dog"

(defparameter dog-upi (upi !ex:dog))

(describe !ex:dog)

(part->string dog-upi)

!"A string"

(defparameter string-upi (upi !"A string"))

(part->string string-upi)

(part->concise dog-upi)
(part->concise string-upi)

;;;------------------------------------------------
;;; Adding triples by hand
;;;------------------------------------------------
(add-triple !ex:Mammal !rdf:type !owl:Class)
(add-triple !ex:Dog !rdfs:subClassOf !ex:Mammal)
(add-triple !ex:Fido !rdf:type !ex:Dog)
(add-triple !ex:Fido !ex:has-owner !ex:John)
(add-triple !ex:Company !rdf:type !owl:Class)
(add-triple !ex:CommercialCompany !rdfs:subClassOf !ex:Company)
(add-triple !ex:Franz !rdf:type !owl:CommercialCompany)
(add-triple !ex:John !ex:works-at !ex:Franz)
(add-triple !ex:Franz !ex:has-product !ex:Agraph)

;;;------------------------------------------------
;;; looking at triples and parts
;;;------------------------------------------------
(defparameter tr (get-triple-by-id 4)) 

(triple-id tr)
(subject tr)

;; turn off encoded triple printing
(enable-print-decoded nil)

;; and look again
(triple-id tr)
(subject tr)

;; turn it back on
(enable-print-decoded t)

(print-triple tr :format :concise)

(dolist (e (get-triples-list))
  (print-triple e :format :concise))

(print-triple tr :format :long)
(print-triple tr :format :ntriple)

(pprint-subject !ex:Fido :format :concise)
(pprint-subject !ex:Fido :maximum-depth 3 :format :concise)
(pprint-object !ex:Mammal :maximum-depth 2 :format :concise)

;;;------------------------------------------------
;;; Querying
;;;------------------------------------------------
(get-triples-list :limit nil)

(print-triples
 (get-triples-list :o !ex:John) :format :concise)
(print-triples
 (get-triples-list :s !ex:Franz :p !rdf:type)
 :format :concise)
(print-triples
 (get-triples-list :s !ex:John) :format :concise)

;;; cursor version
(let ((cursor (get-triples :s !ex:Franz)))
  (print-triple (cursor-next-row cursor))
  ;; it is important to close cursors after you use them. If you 
  ;; do not close them yourself, warning messages about cursor leakage
  ;; will be logged.
  (db.agraph.cursor:discard-cursor cursor))

(defun my-get-triples-list (&optional s p o)
  (let ((result nil))
    ;; iterate-cursor calls cursor-next-row on the cursor until
    ;; it is exhausted. iterate-cursor also takes care of discarding
    ;; the cursor for you.
    (iterate-cursor (triple (get-triples :s s :p p :o o))
      ;; cursors typically reuse their row so you must copy the
      ;; triples it yields.
      (push (copy-triple triple) result))
    (nreverse result)))

;;; add-triple
(add-triple !ex:John !rdf:comment !"john")

(print-triples (get-triples-list :s !ex:John) :format :concise)

(resource "http://www.franz.com/simple#Peter")
(literal "Peter")

(defparameter triple-id 0)

(let ((str1 "http://www.franz.com/simple#Peter")
      (str2 "Peter"))
  (setf triple-id
	(add-triple (resource str1) !rdf:comment (literal str2))))

(print-triple (get-triple-by-id triple-id) :format :concise)

(close-triple-store)

(delete-triple-store "ag-test")

;;;------------------------------------------------
;;; Loading triples from a file
;;;------------------------------------------------

(create-triple-store "ag-test")

(load-ntriples (merge-pathnames "wilburwine.ntriples" *tutorial-directory*) :commit t)

(register-namespace 
 "vin" "http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine#")

(triple-count)

(get-triples-list)

(length (get-triples-list))

(length (get-triples-list :limit nil))

(part->string !<http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine#Wine>)
(part->string !vin:Wine)
(get-triples-list :s !vin:Wine)
(print-triple (get-triple-by-id 14) :format :concise)

(print-triples (get-triples-list :s !vin:Wine) :format :concise)

(register-namespace "ex" "http://www.franz.com/things#"
		    :errorp nil)
(register-namespace 
 "guide" "http://www.w3.org/TR/2003/WD-owl-guide-20030331/")

(add-triple !guide:wine !ex:better-than !ex:beer)

(print-triples (get-triples-list :s !guide:wine) :format :concise)

(triple-count)

;;;------------------------------------------------
;;; deleting
;;;------------------------------------------------
(delete-triples :s !guide:wine :o !ex:beer)
(count-cursor (get-triples :s !guide:wine))

(time 
 (count-cursor (get-triples :s !vin:Wine)))

(time
 (count-query :s !vin:Wine))

;;; add it back
(add-triple !guide:wine !ex:better-than !ex:beer)

;;;------------------------------------------------
;;; reification - RDF style
;;;------------------------------------------------
(let ((bn (new-blank-node)))
  (add-triple bn !rdf:type !rdf:Statement)
  (add-triple bn !rdf:subject !guide:wine)
  (add-triple bn !rdf:predicate !ex:better-than)
  (add-triple bn !rdf:object !ex:beer)
  (add-triple bn !rdf:source !ex:Jans)
  (add-triple bn !rdf:content !ex:Nonsense))

(?- (q- ?st !rdf:type !rdf:Statement)
    (q- ?st !rdf:subject !guide:wine)
    (q- ?st !rdf:predicate !ex:better-than)
    (q- ?st !rdf:object !ex:beer)
    (q- ?st !rdf:source !ex:Jans)
    (q- ?st !rdf:content ?x)
    (lisp (print (part->string ?x))))

(setq tr
 (car (get-triples-list :s !guide:wine :p !ex:better-than :o !ex:beer)))

(print-triple tr :format :concise)

;;;------------------------------------------------
;;; reification - direct style
;;;------------------------------------------------
(defparameter nonsense-triple
      (add-triple !ex:Jans !ex:classifies-as-nonsense
		  (value->upi (triple-id tr) :triple-id)))

(print-triple (get-triple-by-id nonsense-triple) :format :concise)

(let ((bn (new-blank-node)))
  (add-triple
   bn !rdf:statementAbout
   (value->upi (triple-id tr) :triple-id))
  (add-triple bn !rdf:source !ex:Jans)
  (add-triple bn !rdf:content !ex:Nonsense))

(?- (q- ?st !rdf:statementAbout ?id)
    (q- ?st !rdf:source !ex:Jans)
    (q- ?st !rdf:content !ex:Nonsense)
    (lisp (print-triple (get-triple-by-id (upi->number ?id)) :format :concise)))

;;; clean up
(close-triple-store)

;;;------------------------------------------------
;; PROLOG tutorial
;;;------------------------------------------------

(open-triple-store  "ag-test")

(<-- (parent pam bob))
(<- (parent tom bob))
(<- (parent tom liz))
(<- (parent bob ann))
(<- (parent bob pat))
(<- (parent pat jim))

(<-- (female pam))
(<- (female liz))
(<- (female ann))
(<- (female pat))

(<-- (male bob))
(<- (male tom))
(<- (male jim))

;; Interactive Prolog queries
;;
;; (?- (parent ?x ?y))
;; (?- (parent pam ?x))
;; (?- (parent ?x bob))
;; (?- (parent pam bob))

(<-- (offspring ?x ?y)
     (parent ?y ?x))

(<-- (mother ?x ?y)
     (parent ?x ?y)
     (female ?x))

(<-- (grandparent ?x ?y)
     (parent ?x ?z)
     (parent ?z ?y))
     
(<-- (sister ?x ?y)
     (parent ?p ?x)
     (parent ?p ?y)
     (female ?x)
     (not (= ?x ?y)))

(<-- (ancestor ?x ?y)
     (parent ?x ?y))

(<- (ancestor ?x ?y)
    (parent ?x ?z)
    (ancestor ?x ?y))

(defun find-father-son-pairs ()
  (let ((e))
    (prolog 
     (parent ?x ?y)
     (male ?x)
     (male ?y)
     (lisp (setq e (cons ?x ?y))))
    e))
	   
(find-father-son-pairs)


;;;------------------------------------------------
;;; AllegroGraph and Prolog
;;;------------------------------------------------

(close-all-triple-stores)

(create-triple-store "ag-test" :if-exists :supersede)

(load-ntriples (merge-pathnames "wilburwine.ntriples" *tutorial-directory*) :commit t)

(get-triples-list :p !rdfs:subClassOf)
(register-namespace "ex" "http://www.franz.com/things#"
		    :errorp nil)
(register-namespace "guide"
       "http://www.w3.org/TR/2003/WD-owl-guide-20030331/")

(let ((bn (new-blank-node)))
  (add-triple bn !rdf:statementAbout 
	      (value->upi
	       (add-triple !guide:wine !ex:better-than !ex:beer)
	       :triple-id))
  (add-triple bn !rdf:source !ex:Jans)
  (add-triple bn !rdf:content !ex:Nonsense))

;; The main interface to prolog is the function 'q'

(?- (q- ?x ?y ?z))

;; you can stop it by typing a '.'

(?- (q- ?x !rdfs:subClassOf ?y))

(?- (q- ?st !rdf:statementAbout ?id)
    (q- ?st !rdf:source !ex:Jans)
    (q- ?st !rdf:content !ex:Nonsense)
    (lisp (print-triple (get-triple-by-id (upi->number ?id)))))


(select (?x ?y ?z)
        (q- ?x !rdfs:subClassOf ?y)
        (q- ?y !rdfs:subClassOf ?z))

(close-triple-store)

;;;------------------------------------------------
;;; Range Queries
;;;------------------------------------------------

(close-all-triple-stores)

(open-triple-store "ag-test")

(setf (predicate-mapping !ex:age) :int)

(add-triple !ex:alice !ex:age (literal "42" :datatype "int"))
(add-triple !ex:bob !ex:age (literal "24" :datatype "int"))
(add-triple !ex:carol !ex:age (literal "39" :datatype "int"))

(commit-triple-store)

(get-triples-list :p !ex:age
                  :o (literal "30" :datatype "int")
                  :o-end (literal "50" :datatype "int"))
;; => (<alice age 42> <carol age 39>)

(close-triple-store)

;;;------------------------------------------------
;;; Geospatial Reasoning
;;;------------------------------------------------

(close-all-triple-stores)

(open-triple-store "ag-test")

(defparameter stripe (register-cartesian-striping 0 100 0 100 10))
(add-geospatial-subtype-to-db stripe)

(progn (add-triple !ex:alice
                   !ex:location
                   (geospatial->upi stripe 30.0 30.0))
       (add-triple !ex:bob
                   !ex:location
                   (geospatial->upi stripe 40.0 40.0))
       (add-triple !ex:carol
                   !ex:location
                   (geospatial->upi stripe 50.0 50.0)))

;; Find people located within a box.
(iterate-cursor (tr
                 (get-triples-geospatial-bounding-box stripe !ex:location
                                                      20 40 20 40 :use-g nil))
  (print tr))

;; Find people located within circle
(iterate-cursor (tr
                 (get-triples-geospatial-radius stripe !ex:location
                                                35 35 10 :use-g nil))
  (print tr))

;; Find people located within polygon.
(defparameter *cartesian-10*  
  (register-cartesian-striping -1000 1000 -1000 1000 10))  

(add-geospatial-subtype-to-db *cartesian-10*)

(defparameter *my-pentagon*
  '((234.56 . 347.31)
    (243.07 . 348.40)
    (247.85 . 357.02)
    (230.68 . 349.52)
    (231.37 . 348.62)))

(defparameter *penta*
  (with-temp-upi (u)
    (loop with id = (new-blank-node)
       for vertex-number from 1
       for (x . y) in *my-pentagon*
       do (add-triple id
		      (value->upi vertex-number :subscript)
		      (geospatial->upi *cartesian-10* x y u))
       finally (return id))))

(polygon-vertexes *penta*)

(point-inside-polygon-p *my-pentagon* 235.5 350.0)
;; -> t

(close-triple-store)

;;;------------------------------------------------
;;; Transactions
;;;------------------------------------------------

(close-all-triple-stores)

(defparameter db1 (create-triple-store "ag-test" :if-exists :supersede))
(defparameter db2 (open-triple-store "ag-test" :if-exists :supersede))

(load-rdf/xml (merge-pathnames "lesmis.rdf" *tutorial-directory*) :db db1 :commit t)
(format t "~&Loaded ~a lesmis.rdf triples via db1.~%" (triple-count :db db1))
;; => 916

(load-ntriples (merge-pathnames "kennedy.ntriples" *tutorial-directory*) :db db2)
(format t "~&Loaded ~a kennedy.ntriples triples via db2.~%" (triple-count :db db2))
;; => 1214

;; db1 should find Valjean
(get-triples-list :o !"Valjean" :db db1)
;; => (<character11 title Valjean>)

;; db1 should not find Kennedy, because it was not committed
(get-triples-list :o !"Kennedy" :db db1)
;; => nil

;; db2 should not find Valjean until db2 is committed or rolled back
(get-triples-list :o !"Valjean" :db db2)
;; => nil

;; db2 is not committed, but will find Kennedy
(get-triples-list :o !"Kennedy" :db db2 :limit 1)
;; => (<person1 last-name Kennedy>)

;; Rolling back contents of db2.
(rollback-triple-store :db db2)

(format t "~&There are now ~a triples visible via db2." (triple-count :db db2))
;; => 916

;; db2 should find Valjean now
(get-triples-list :o !"Valjean" :db db2)
;; => (<character11 title Valjean>)

;; db2 is not committed, but will find Kennedy
(get-triples-list :o !"Kennedy" :db db2 :limit 1)
;; => nil

(load-ntriples (merge-pathnames "kennedy.ntriples" *tutorial-directory*) :db db2)
(format t "~&Reloaded kennedy.ntriples triples via db2.~%")

(format t "~&There are now ~a triples visible on db1.~%" (triple-count :db db1))
;; => 916
(format t "~&There are now ~a triples visible on db2.~%" (triple-count :db db2))
;; => 2130

(commit-triple-store :db db1)
(commit-triple-store :db db2)

(format t "~&There are now ~a triples visible on db1.~%" (triple-count :db db1))
;; => 2130
(format t "~&There are now ~a triples visible on db2.~%" (triple-count :db db2))
;; => 2130

;; both are committed, so will find Kennedy
(get-triples-list :o !"Kennedy" :db db1 :limit 1)
;; => (<person1 last-name Kennedy>)

(close-all-triple-stores)
