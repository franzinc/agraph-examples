(require :agraph)             ;load AllegroGraph  

(enable-!-reader)             ;turn on the !-reader  

(in-package :db.agraph.user)  ;switch to the AllegroGraph package  

(enable-print-decoded t)      ;print UPIs and triples readably

(close-all-triple-stores)

(create-triple-store "ag-test")

;; Examples from sparql-exploration.html

(load-ntriples "http://www.w3.org/2000/10/rdf-tests/rdfcore/ntriples/test.nt")

(commit-triple-store)

(sparql:run-sparql "
  SELECT DISTINCT ?o {
    ?s <http://example.org/property> ?o .
    FILTER (isIRI(?o))
  }" :output-format :sparql-json)

(sparql.algebra:sparql->algebra "
  SELECT DISTINCT ?o {
    ?s <http://example.org/property> ?o .
    FILTER (isIRI(?o))
  }")

;; Free text search in SPARQL

(create-freetext-index "sparql-tutorial"
                       :predicates '(!<http://example.org/property>)
                       :index-resources t)

;; The lisp run-sparql API does not use registered or
;; *standard-namespaces* by default.
;; Passing a parameter is an alternative to using PREFIX
;; in the sparql query text.
(setf *prefixes* (append
                  '(("kdy" "http://www.franz.com/simple#")
                    ("fti" "http://franz.com/ns/allegrograph/2.2/textindex/"))
                  *standard-namespaces*))

(sparql:run-sparql "
  SELECT DISTINCT ?s {
    ?s fti:match 'resou*' .
  }" :default-prefixes *prefixes*
  :output-format :alists)

;; Ask, Construct, and Describe queries

(time
 (load-ntriples "kennedy.ntriples"))

(commit-triple-store)

(sparql:run-sparql "
  select ?s where { ?s rdf:type kdy:person} limit 5
  " :default-prefixes *prefixes*
  :output-format :alists)

(sparql:run-sparql "
  ask { ?s kdy:first-name 'John' }
  " :default-prefixes *prefixes*)
;; => t

;; construct triples, and add to the db
(dolist (tr (sparql:run-sparql "
  construct {?a kdy:has-grandchild ?c} 
    where { ?a kdy:has-child ?b .
            ?b kdy:has-child ?c . }
  " :default-prefixes *prefixes*
  :output-format :triples :engine :allegrograph-2))
  (add-triple (subject tr) (predicate tr) (object tr)))

(triple-count)

(commit-triple-store)

(sparql:run-sparql "
  select ?s ?o where { ?s kdy:has-grandchild ?o} limit 5
  " :default-prefixes *prefixes*)

(sparql:run-sparql "
  describe ?s ?o where { ?s kdy:has-grandchild ?o . } limit 1
  " :default-prefixes *prefixes*)

;; with-variables

(register-namespace "kdy" "http://www.franz.com/simple#" :errorp nil)

;; replace variables in the queries with values
(let* ((p !kdy:has-grandchild))
  (sparql:run-sparql "select distinct ?s where { ?s ?p ?o}"
                     :default-prefixes *prefixes*
                     :output-format :alists
                     :with-variables `((?p . ,p))))
;; => (((?s . {person1})) ((?s . {person2})))

