(require :agraph)

(in-package :triple-store-user)

;------------------------------------------
;; the setup
;------------------------------------------
(enable-!-reader)

(enable-print-decoded t)

;; short for print-triples-list
(defun ptl (s p o)
  (print-triples (get-triples :s s :p p :o o) :format :concise))


(register-namespace
 "ex" "http://www.franz.com/simple#"
 :errorp nil)

;------------------------------------------
;; first steps
;------------------------------------------

(defparameter ground-ts (create-triple-store "sample"))

(add-triple !ex:jans !ex:owns !ex:birra)

(add-triple !ex:jans !owl:sameAs !ex:jannes)

;; make sure the triples are there
(print-triples *db*  :format :concise)

;; no reasoning, ask about ex:owns and get one triple
(get-triples-list :p !ex:owns)

;; apply reasoning and ask again
(defparameter inferred-ts (apply-rdfs++-reasoner))

;; we'll get two triples now
(get-triples-list :p !ex:owns)

;; using the :db argument
(get-triples-list :p !ex:owns :db ground-ts)
(get-triples-list :p !ex:owns :db (ground-triple-store *db*))

;; using ptl
(ptl !ex:Jans !ex:has nil)

;------------------------------------------
;; testing inverse of
;------------------------------------------

;; assuming that you've set ground-ts above
(delete-triples)

(add-triple !ex:jans !ex:owns !ex:birra)
(add-triple !ex:owned-by !owl:inverseOf !ex:owns)
(add-triple !ex:has !owl:inverseOf !ex:owned-by)

(ptl !ex:birra !ex:owned-by nil)
(ptl nil !ex:owned-by nil)
(ptl nil !ex:owned-by !ex:jans)

(ptl !ex:jans !ex:has nil)
(ptl nil !ex:has nil)
(ptl nil !ex:has !ex:birra)

(ptl nil nil nil)
; this will return nothing
; because it works on only the triples explicity 
; added to the triple-store

(select (?x)
	(q- !ex:birra !ex:owned-by ?x))

; this will return something
; because it works on inferred triples

(select (?x)
	(q !ex:birra !ex:owned-by ?x))

;------------------------------------------
;; testing subproperty of
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:jans !ex:has-pet !ex:birra)
(add-triple !ex:has-pet !rdfs:subPropertyOf !ex:owns)
(add-triple !ex:birra !ex:friend-of !ex:samira)

(ptl !ex:jans !ex:owns !ex:birra)
(ptl !ex:jans !ex:owns nil)
(ptl nil !ex:owns !ex:birra)

(ptl !ex:jans !ex:has-pet !ex:birra)
(ptl !ex:jans !ex:has-pet nil)
(ptl nil !ex:has-pet !ex:birra)

(select (?x ?y)
	(q !ex:jans !ex:owns ?x)
	(q ?x !ex:friend-of ?y))


;------------------------------------------
;; testing inverse + subproperty
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:jans !ex:has-pet !ex:birra)
(add-triple !ex:owned-by !owl:inverseOf !ex:owns)
(add-triple !ex:has !owl:inverseOf !ex:owned-by)
(add-triple !ex:has-pet !rdfs:subPropertyOf !ex:owns)
(add-triple !ex:pet-of !owl:inverseOf !ex:has-pet)

;; direct triples

(ptl !ex:jans !ex:has-pet !ex:birra)
(ptl nil !ex:has-pet !ex:birra)
(ptl !ex:jans !ex:has-pet nil)

;; inverse of !ex:has-pet

(ptl !ex:birra !ex:pet-of !ex:jans)
(ptl nil !ex:pet-of !ex:jans)
(ptl !ex:birra !ex:pet-of nil)


;; subproperty

(ptl !ex:jans !ex:owns !ex:birra)
(ptl !ex:jans !ex:owns nil)
(ptl nil !ex:owns !ex:birra)


;; inverse of subproperty

(ptl !ex:birra !ex:owned-by !ex:jans)
(ptl nil !ex:owned-by !ex:jans)
(ptl !ex:birra !ex:owned-by nil)

;; inverse of inverse

(ptl !ex:jans !ex:has !ex:birra)
(ptl nil !ex:has !ex:birra)
(ptl !ex:jans !ex:has nil)

;------------------------------------------
;; testing sameas
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:jans !ex:owns !ex:birra)
(add-triple !ex:jans !owl:sameAs !ex:jannes)
(add-triple !ex:aasman !owl:sameAs !ex:jannes)
(add-triple !ex:birra !owl:sameAs !ex:son-of-samira)

(ptl !ex:aasman !ex:owns !ex:son-of-samira)
(ptl !ex:aasman !ex:owns nil)
(ptl nil !ex:owns !ex:son-of-samira)

(ptl nil !ex:owns nil)

;------------------------------------------
;; testing !owl:sameAs with inverse and subproperty
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:jans !ex:has-pet !ex:birra)
(add-triple !ex:owned-by !owl:inverseOf !ex:owns)
(add-triple !ex:has !owl:inverseOf !ex:owned-by)
(add-triple !ex:has-pet !rdfs:subPropertyOf !ex:owns)
(add-triple !ex:pet-of !owl:inverseOf !ex:has-pet)
(add-triple !ex:birra !ex:age !ex:twelve)

(add-triple !ex:jans !owl:sameAs !ex:jannes)
(add-triple !ex:aasman !owl:sameAs !ex:jannes)
(add-triple !ex:birra !owl:sameAs !ex:son-of-samira)

;; direct triples

(ptl !ex:aasman !ex:has-pet !ex:son-of-samira)
(ptl nil !ex:has-pet !ex:son-of-samira)
(ptl !ex:aasman !ex:has-pet nil)

;; inverse of !ex:owns

(ptl !ex:son-of-samira !ex:pet-of !ex:aasman)
(ptl nil !ex:pet-of !ex:aasman)
(ptl !ex:son-of-samira !ex:pet-of nil)

;; inverse of inverse

(ptl !ex:aasman !ex:has !ex:son-of-samira)
(ptl nil !ex:has !ex:son-of-samira)
(ptl !ex:aasman !ex:has nil)

;; subproperty

(ptl !ex:aasman !ex:owns !ex:son-of-samira)
(ptl !ex:aasman !ex:owns nil)
(ptl nil !ex:owns !ex:son-of-samira)

;; inverse of subproperty

(ptl !ex:son-of-samira !ex:owned-by !ex:aasman)
(ptl nil !ex:owned-by !ex:aasman)
(ptl !ex:son-of-samira !ex:owned-by nil)

;; what to do with this?

(ptl nil nil nil)

;; but what if predicate is unknown?

(ptl !ex:jans nil !ex:birra) ;; this returns only one valid result

;; what should i do here, find all the predicates defined for !ex:aasman (and the sames) 
;; and then try them all?

(ptl !ex:aasman nil !ex:birra)

(ptl !ex:aasman nil nil)

;; 

;------------------------------------------
;; testing type with subclass
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:mammal !rdfs:subClassOf !ex:animal)
(add-triple !ex:human !rdfs:subClassOf !ex:mammal)
(add-triple !ex:man !rdfs:subClassOf !ex:human)
(add-triple !ex:jans !rdf:type !ex:man)
(add-triple !ex:jans !owl:sameAs !ex:jannes)
(add-triple !ex:aasman !owl:sameAs !ex:jannes)

(ptl !ex:jans !rdf:type !ex:man)
(ptl !ex:jans !rdf:type !ex:human)
(ptl !ex:jans !rdf:type nil)

(ptl !ex:aasman !rdf:type !ex:man)
(ptl !ex:aasman !rdf:type !ex:human)
(ptl !ex:aasman !rdf:type nil)

(ptl nil !rdf:type !ex:man)
(ptl nil !rdf:type !ex:human)
(ptl nil !rdf:type nil)


;------------------------------------------
;; testing type with !rdfs:range
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:jans !ex:has-pet !ex:birra)
(add-triple !ex:has-pet !rdfs:range !ex:pet)
(add-triple !ex:pet !rdfs:subClassOf !ex:mammal)
(add-triple !ex:fatcat !owl:sameAs !ex:birra)

(ptl !ex:birra !rdf:type !ex:pet)
(ptl !ex:birra !rdf:type nil) 
(ptl nil !rdf:type !ex:pet) 

(ptl !ex:birra !rdf:type !ex:mammal)
(ptl !ex:fatcat !rdf:type !ex:mammal)


;------------------------------------------
;; testing type with !rdfs:domain
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:jans !ex:has-pet !ex:birra)
(add-triple !ex:has-pet !rdfs:domain !ex:human)
(add-triple !ex:human !rdfs:subClassOf !ex:mammal)
(add-triple !ex:jans !owl:sameAs !ex:aasman)

(ptl !ex:jans !rdf:type !ex:human)
(ptl !ex:jans !rdf:type nil)
(ptl nil !rdf:type !ex:human)

;; not returning all solutions..

(ptl nil !rdf:type nil)

;------------------------------------------
;; testing transitivity with !owl:sameAs in the chain
;------------------------------------------

;; start fresh, see above if you need to create a triple-store
(delete-triples)

(add-triple !ex:contains !rdf:type !owl:TransitiveProperty)
(add-triple !ex:usa !ex:contains !ex:california)
(add-triple !ex:golden-state !ex:contains !ex:contra-costa)
(add-triple !ex:contra-costa !ex:contains !ex:moraga)

(add-triple !ex:usa !owl:sameAs !ex:uncle-sam)
(add-triple !ex:moraga !owl:sameAs !ex:mytown)
(add-triple !ex:california !owl:sameAs !ex:golden-state)

(ptl !ex:usa !ex:contains !ex:moraga)
(ptl !ex:uncle-sam !ex:contains !ex:mytown)

(ptl !ex:uncle-sam !ex:contains !ex:mytown)
(ptl !ex:golden-state !ex:contains !ex:moraga)
(ptl !ex:california !ex:contains !ex:moraga)
(ptl !ex:california !ex:contains !ex:mytown)

(ptl !ex:usa !ex:contains nil)
(ptl !ex:uncle-sam !ex:contains nil)

(ptl nil !ex:contains !ex:moraga)
(ptl nil !ex:contains !ex:mytown)




