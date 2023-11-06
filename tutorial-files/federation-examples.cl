(in-package #:db.agraph.user)

#| Example 1

Show RDFS++ reasoning operating across 7 triples spread over
three triple-stores.

|#

(defun setup-federated-reasoning-example-1 ()
  (close-all-triple-stores)
  (register-namespace "ex" "http://www.franz.com/simple#" 
		      :errorp nil)
  ;; create the stores and add triples
  (with-triple-store (db (create-triple-store "a"))
    (add-triple !ex:SmallBlackDog !rdfs:subClassOf !ex:BlackDog)
    (add-triple !ex:BlackDog !rdfs:subClassOf !ex:Dog)
    (add-triple !ex:Dog !rdfs:subClassOf !ex:Mammal)
    (commit-triple-store))
  (with-triple-store (db (create-triple-store "b"))
    (add-triple !ex:Giraffe !rdfs:subClassOf !ex:Mammal)
    (add-triple !ex:Lion !rdfs:subClassOf !ex:Mammal)
    (commit-triple-store))
  (with-triple-store (db (create-triple-store "c"))
    (add-triple !ex:Mammal !rdfs:subClassOf !ex:Animal)
    (add-triple !ex:Abbey !rdf:type !ex:BlackDog)
    (commit-triple-store))

  ;; federate them
  (let ((fed (federate-triple-stores "fed" (list "a" "b" "c"))))
    (setf *db* (apply-rdfs++-reasoner :db fed))))

(defun test-federated-reasoning-example-1 ()
  (dolist (class (list !ex:SmallBlackDog
		       !ex:BlackDog
		       !ex:Dog
		       !ex:Mammal
		       !ex:Animal))
    (format t "~&Abbey is~:[ not~;~] a ~a.~%"
	    (get-triple :s !ex:Abbey :p !rdf:type :o class)
	    class)))

#|
(setup-federated-reasoning-example-1)
=> #<reasoning-triple-store
     inner #<federated-triple-store fed 3 sub-stores @ #x1298638a> @
     #x1298ecba>

(test-federated-reasoning-example-1)
Abbey is not a !<http://www.franz.com/simple#SmallBlackDog>.
Abbey is a !<http://www.franz.com/simple#BlackDog>.
Abbey is a !<http://www.franz.com/simple#Dog>.
Abbey is a !<http://www.franz.com/simple#Mammal>.
Abbey is a !<http://www.franz.com/simple#Animal>.

(print-triples *db*)
<triple 1: ex:SmallBlackDog rdfs:subClassOf ex:BlackDog default-graph>
<triple 2: ex:BlackDog rdfs:subClassOf ex:Dog default-graph>
<triple 3: ex:Dog rdfs:subClassOf ex:Mammal default-graph>
<triple 1: ex:Giraffe rdfs:subClassOf ex:Mammal default-graph>
<triple 2: ex:Lion rdfs:subClassOf ex:Mammal default-graph>
<triple 1: ex:Mammal rdfs:subClassOf ex:Animal default-graph>
<triple 2: ex:Abbey rdf:type ex:BlackDog default-graph>
<triple 0: ex:Abbey rdf:type ex:Dog default-graph>
<triple 0: ex:Abbey rdf:type ex:Mammal default-graph>
<triple 0: ex:Abbey rdf:type ex:Animal default-graph>
<triple 0: ex:Dog rdfs:subClassOf ex:Animal default-graph>
<triple 0: ex:BlackDog rdfs:subClassOf ex:Animal default-graph>
<triple 0: ex:BlackDog rdfs:subClassOf ex:Mammal default-graph>
<triple 0: ex:SmallBlackDog rdfs:subClassOf ex:Animal default-graph>
<triple 0: ex:SmallBlackDog rdfs:subClassOf ex:Mammal default-graph>
<triple 0: ex:SmallBlackDog rdfs:subClassOf ex:Dog default-graph>
<triple 0: ex:Giraffe rdfs:subClassOf ex:Animal default-graph>
<triple 0: ex:Lion rdfs:subClassOf ex:Animal default-graph>


(select (?o)
  (q- !ex:Abbey !rdf:type ?o))
=> (("http://www.franz.com/simple#BlackDog"))

(select (?o)
  (q !ex:Abbey !rdf:type ?o))
=> (("http://www.franz.com/simple#Dog")
    ("http://www.franz.com/simple#Mammal")
    ("http://www.franz.com/simple#Animal")
    ("http://www.franz.com/simple#BlackDog"))

(sparql:run-sparql "
 PREFIX ex: <http://www.franz.com/simple#> 
 PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
 SELECT ?o {
   ex:Abbey rdf:type ?o .
 }" :output-format :table)
-------------------
| o               |
===================
| ex:BlackDog     |
| ex:Dog          |
| ex:Mammal       |
| ex:Animal       |
-------------------

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Part two : LUBM and federation

(defun open-lubm-datasets (root partition-count)
  (let ((files (directory (string+ (namestring root)
				   (string+ "lubm-50-*-of-" partition-count)))))
    (unless files
      (error "No lubm datasets found in ~a" root))
    (let ((stores 
	   (loop for file in files collect
		(open-triple-store 
		 (subseq 
		  (namestring file) 0 (length (namestring file)))))))
      (setf *db* (federate-triple-stores "lubm-fed" stores))
      (apply-rdfs++-reasoner))))

(defmacro defquery (name &body body)
  `(compile (defun ,name () ,@body)))

(defun register-lubm-namespaces (lubm-uri)
  (register-namespace "ub" lubm-uri :errorp nil)
  (register-namespace "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  (register-namespace "rdfs" "http://www.w3.org/2000/01/rdf-schema#"))

(defun create-lubm-namespaces (university-count)
  (dotimes (i university-count)
    (dotimes (j 40)
      (register-namespace
       (format nil "u~ad~a" i j)
       (format nil "http://www.Department~a.University~a.edu/" j i)))))

(defun lubm-setup ()
  (create-lubm-namespaces 50)
  (register-lubm-namespaces 
   "http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#")
  t)

(defquery q1
    (select (?x)
	    (q ?x !ub:takesCourse !u0d0:GraduateCourse0)
	    (q ?x !rdf:type !ub:GraduateStudent)))

(defquery q1b
    (select (?x)
	    (q- ?x !ub:takesCourse !u0d0:GraduateCourse0)
	    (q- ?x !rdf:type !ub:GraduateStudent)))

(defquery q4
    (select (?x ?name ?email ?telephone)	
	    (q ?x !ub:worksFor !<http://www.Department0.University0.edu>)
	    (q ?x !rdf:type !ub:Professor)
	    (q ?x !ub:name ?name)
	    (q ?x !ub:emailAddress ?email)
	    (q ?x !ub:telephone ?telephone)))

#|

(lubm-setup)
=> t

(open-lubm-datasets "/x/gwking/triple-stores/lubm-50-federation/"
                      6)
#<reasoning-triple-store
  inner #<federated-triple-store lubm-fed 6 sub-stores @ #x1004f8dce2>
  @ #x1004f8e1b2>

(q1)
(("http://www.Department0.University0.edu/GraduateStudent124")
 ("http://www.Department0.University0.edu/GraduateStudent44")
 ("http://www.Department0.University0.edu/GraduateStudent142")
 ("http://www.Department0.University0.edu/GraduateStudent101"))

(q1b)
(("http://www.Department0.University0.edu/GraduateStudent124")
 ("http://www.Department0.University0.edu/GraduateStudent44")
 ("http://www.Department0.University0.edu/GraduateStudent142")
 ("http://www.Department0.University0.edu/GraduateStudent101"))

(q4)
(("http://www.Department0.University0.edu/FullProfessor7"
  "FullProfessor7" "FullProfessor7@Department0.University0.edu"
  "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssociateProfessor5"
  "AssociateProfessor5"
  "AssociateProfessor5@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssociateProfessor4"
  "AssociateProfessor4"
  "AssociateProfessor4@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssociateProfessor2"
  "AssociateProfessor2"
  "AssociateProfessor2@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssistantProfessor1"
  "AssistantProfessor1"
  "AssistantProfessor1@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssociateProfessor6"
  "AssociateProfessor6"
  "AssociateProfessor6@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssistantProfessor7"
  "AssistantProfessor7"
  "AssistantProfessor7@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssociateProfessor7"
  "AssociateProfessor7"
  "AssociateProfessor7@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssociateProfessor3"
  "AssociateProfessor3"
  "AssociateProfessor3@Department0.University0.edu" "xxx-xxx-xxxx")
 ("http://www.Department0.University0.edu/AssistantProfessor9"
  "AssistantProfessor9"
  "AssistantProfessor9@Department0.University0.edu" "xxx-xxx-xxxx")
 ...)

;;; now try with just part of the store
(setf *db* (find-triple-store "lubm-50-2-of-6"))
#<db.agraph::triple-db
  /x/gwking/triple-stores/lubm-50-federation/lubm-50-2-of-6, open @
  #x10023ebe32>

(apply-rdfs++-reasoner)
#<reasoning-triple-store
  inner #<triple-db
          /x/gwking/triple-stores/lubm-50-federation/lubm-50-2-of-6, open
          @ #x10023ebe32>
  @ #x10043aeae2>

(q4)
=> nil

|#


#|

This example shows that using a federated triple store is just
as simple as using a regular one. As data, we take Gov.ntriples 
and split it into two pieces (Gov-1.ntriples and Gov-2.ntriples).
Then we build three disk-based persistent triple-stores: gov, gov-1 
and gov-2. Finally, we federate gov-1 and gov-2 into fed-12:

  (federate-triple-stores "fed-21" (list "gov-1" "gov-2"))

Now we can query either the entire store (using gov), either half
of the store (using gov-1 or gov-2) or the federated-store (using
fed-12).

|#

(defparameter *source-directory* "/repository/data/")

(defun shared-setup ()
  (register-namespace "cyc" "http://www.cyc.com/2002/04/08/cyc#")
  )

(defun create-example-triple-stores (&key 
				     (source-directory *source-directory*))
  (shared-setup)
  (flet ((source-path (name)
	   (make-pathname
	    :name name :type "ntriples" :defaults source-directory)))
    (create-triple-store "gov")
    (load-ntriples (source-path "Gov"))

    (create-triple-store "gov-1")
    (load-ntriples (source-path "Gov-1"))

    (create-triple-store "gov-2")
    (load-ntriples (source-path "Gov-2"))))

(defun open-example-triple-stores ()
  (shared-setup)
  (setf gov (open-triple-store "gov"))
  (setf gov1 (open-triple-store "gov-1"))
  (setf gov2 (open-triple-store "gov-2"))
  (setf fed12 (federate-triple-stores "fed-12" (list "gov-1" "gov-2"))))

;; Create the stores 
(create-example-triple-stores)

;; Open the stores
(open-example-triple-stores)

;;; triple-count

(format t "Gov: ~:d, Federated Gov: ~:d."
	(triple-count :db gov) (triple-count :db fed12))

;;; Tell me about CargoShips

(count-cursor
 (get-triples
  :p !rdf:type
  :o !cyc:CargoShip
  :db gov))
;==> 3

(count-cursor
 (get-triples
  :p !rdf:type
  :o !cyc:CargoShip
  :db "fed-12"))
;===> 3

(print-triples
 (get-triples
  :p !rdf:type
  :o !cyc:CargoShip
  :db "gov"))
 
(print-triples
 (get-triples
  :p !rdf:type
  :o !cyc:CargoShip
  :db "fed-12"))

;;; prolog select works too
;; 
;; Here, I chosen a query that can only be answered by looking in
;; both stores.

(defun find-arg66 (triple-store)
  (with-triple-store (db triple-store)
    (select0 (?p ?o)
      (q- !<http://www.cyc.com/2002/04/08/cyc#arg6GenlAttribute> ?p ?o))))

(length (find-arg66 gov))		;8
(length (find-arg66 gov1))		;4
(length (find-arg66 gov2))		;4
(length (find-arg66 fed12))		;8 !

;; at very close to the _same_ speed
(defun speed-test ()
  (print "Gov")
  (time (loop repeat 1000 do (find-arg66 gov)))
  (terpri)
  (print "Federated")
  (time (loop repeat 1000 do (find-arg66 fed12))))

(compile 'speed-test)
(speed-test)

#|
"Gov" 
; cpu time (non-gc) 1,300 msec user, 20 msec system
; cpu time (gc)     130 msec user, 0 msec system
; cpu time (total)  1,430 msec user, 20 msec system
; real time  1,521 msec
; space allocation:
;  3,025,031 cons cells, 37,964,864 other bytes, 0 static bytes

"Federated" 
; cpu time (non-gc) 1,370 msec user, 20 msec system
; cpu time (gc)     140 msec user, 0 msec system
; cpu time (total)  1,510 msec user, 20 msec system
; real time  1,723 msec
; space allocation:
;  3,099,184 cons cells, 40,213,112 other bytes, 23,736 static bytes
|#


;; A more complex select
(defun find-terrorist (triple-store-name)
  (with-triple-store (db triple-store-name)
    (select (?A ?B ?X ?P)
      (q- !cyc:OsamaBinLaden !cyc:influencesAgent ?Z)
      (q- ?Z !cyc:friends ?B)
      (q- ?Z !cyc:residesInRegion ?D)
      (q- ?B !cyc:hasBeenIn ?D)
      (q- ?A !cyc:acquaintedWith ?B)
      (q- ?A !cyc:occupation ?X) 
      (q- ?B !cyc:occupation ?X)
      (q- ?B !cyc:birthPlace ?E)
      (q- ?F !cyc:birthPlace ?E)
      (q- ?F !cyc:hasBeliefSystems ?P))))

(compile 'find-terrorist)

(time
 (find-terrorist "gov"))

(find-terrorist "gov-1")

(find-terrorist "gov-2")

(time
 (find-terrorist "fed-12"))



(in-package #:db.agraph)

;;;; setup - split wilburwine into four stores and federate them
;;;;

(setf ww-full (create-triple-store "wilburwine-full"))
;;(setf ww-full (open-triple-store "wilburwine-full"))

(load-ntriples "wilburwine.ntriples" :commit t)

(let ((ww (loop for index from 1 to 4
             collect (cons index (create-triple-store (string+ "wilburwine-" index)
                                                      :if-exists :supersede)))))
  (map-cursor nil (lambda (triple)
                    (let* ((id (triple-id triple))
                           (store (1+ (mod id 4))))
                      (with-triple-store (db (cdr (assoc store ww)))
                        (let ((*db* ww-full))
                          (add-triple
                           (part->string (subject triple) :format :ntriple)
                           (part->string (predicate triple) :format :ntriple)
                           (part->string (object triple) :format :ntriple) 
                           :db db)))))
              (get-triples :db ww-full))
  (mapcar (lambda (x)
            (format t "~&Split repo wilburwine-~a has ~a triples.~%" (car x) (triple-count :db (cdr x)))
            (commit-triple-store :db (cdr x))
            (close-triple-store :db (cdr x)))
          ww))

(setf ww-fed
      (federate-triple-stores
       "wilburwine-combine"
       (loop for index from 1 to 4
          collect (string+ "wilburwine-" index))))

;;; get-triples works

(count-cursor 
 (get-triples 
  :p !rdf:type 
  :o !<http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine#Winery>
  :db ww-full))
==> 84

(count-cursor 
 (get-triples 
  :p !rdf:type 
  :o !<http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine#Winery>
  :db ww-fed))
==> 84

;;; prolog select works too

(register-namespace 
 "vin" 
 "http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine#")

;; for UPIs
(let ((*db* ww-fed))
  (select0 (?wine ?type)
    (q- ?wine !vin:locatedIn !vin:GermanyRegion)
    (q- ?wine !vin:hasSugar !vin:Sweet)
    (q- ?wine !rdf:type ?type)))


;; and strings
(time
 (let ((*db* ww-fed))
   (select (?wine ?type)
     (q- ?wine !vin:locatedIn !vin:GermanyRegion)
     (q- ?wine !vin:hasSugar !vin:Sweet)
     (q- ?wine !rdf:type ?type))))

;; same answer, same time too
(time
 (let ((*db* ww-full))
   (select (?wine ?type)
     (q- ?wine !vin:locatedIn !vin:GermanyRegion)
     (q- ?wine !vin:hasSugar !vin:Sweet)
     (q- ?wine !rdf:type ?type))))


;;;; SPARQL seems to work too

;####
;# In principle, running this query with a 'depth' of 1 will have the same
;# effect as running it once with a higher 'depth'.
;# This generates the triples to emulate the transitivity of locatedIn.
;# Produces 23 new triples.
;####

(setf query-01
"PREFIX vin:  <http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine#>
CONSTRUCT {
  ?x vin:locatedIn vin:CaliforniaRegion .
}
WHERE {
  { ?x vin:locatedIn [ vin:locatedIn vin:CaliforniaRegion ] . }
  UNION
  { ?wine vin:locatedIn
    [ vin:locatedIn [ vin:locatedIn vin:CaliforniaRegion ] ] }
  UNION
  { ?wine vin:locatedIn
    [ vin:locatedIn
      [ vin:locatedIn
        [ vin:locatedIn vin:CaliforniaRegion ] ] ] }
}")


(let ((*db* ww-full))
  (with-output (*standard-output* #p"/tmp/query-01-full.xml")
    (sparql:run-sparql query-01 :output-format :xml)))

(let ((*db* ww-fed))
  (with-output (*standard-output* #p"/tmp/query-01-fed.xml")
    (sparql:run-sparql query-01 :output-format :xml)))


;;?? The file differ but I believe the contain the same information. 
;; I need a better comparison function...

;; Missing bits: 
;; caching strings
;; reification -- need to associate triples with particular stores
;;  use UUIDs, add triples to name siblings, alter triple-ids to 
;;  include a short store ID that can be associated with full UUIDs,
;;  and more bookkeeping of the same sort
;; no reasoning
;; efficiency -- tracking which stores contain which information
