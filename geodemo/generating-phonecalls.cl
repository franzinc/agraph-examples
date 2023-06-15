(eval-when (compile load eval)
  (require :agraph "~/ag731smp/lib/agraph.fasl"))


(in-package :triple-store-user)

(eval-when (compile load eval)
  (enable-!-reader))

(defparameter .test-port. 10035)

(defparameter g3
    (geo-register-nd-subtype
     :ordinates '((:name :longitude :minimum -123.0 :maximum -122.0 :strip-width 0.001 :resolution 0.00001)
		  (:name :latitude  :minimum  +37.0 :maximum  +38.0 :strip-width 0.001 :resolution 0.00001)
		  (:name :time :resolution 1
		   :minimum "2014-01-01T00:00:00Z"
		   :maximum "2015-01-01T00:00:00Z"))
     :install t))

(defun create-store (&key (name "PhoneCalls") (num 1000000))
  (ignore-errors (close-triple-store))
  (register-namespace "phone" "http://spooks.gov/phone-calls#")
  (create-triple-store name :port .test-port.) 
  (add-geospatial-subtype-to-db g3)
  (setf (predicate-mapping !phone:phoneCallOriginLocation) g3)
  (setf (predicate-mapping !phone:phoneCallTargetLocation) g3)
  (time (setup-geo-phone-calls :num num))
  (commit-triple-store)
  *db*)

(defun setup-geo-phone-calls (&key (num 1000))
  (loop for i from 1 to num
      as lat-from = (+   37.750 (random 0.050))
      as lon-from = (+ -122.480 (random 0.090))
      as lat-to   = (+   37.750 (random 0.050))
      as lon-to   = (+ -122.480 (random 0.090))
      as time = (+ (load-time-value (string-to-universal-time "Mon Aug 18 08:00:00 2014"))
		   (random (load-time-value (* 12 3600))))
      as from = (random-phone-number)	; use encoding?
      as to   = (random-phone-number)	; use encoding?
      as event = (new-blank-node)
      do
	;; (format t "~a ~a ~a~%" from to o)
	(add-triple event !phone:phoneCallOriginator (literal from))
	(add-triple event !phone:phoneCallReceiver   (literal to))
	(add-triple event !phone:phoneCallOriginLocation (geo-plist->geo g3 :latitude lat-from :longitude lon-from :time time))
	(add-triple event !phone:phoneCallTargetLocation (geo-plist->geo g3 :latitude lat-to   :longitude lon-to   :time time))
      when (= 0 (mod i 100000)) do (commit-triple-store))
  (commit-triple-store))

(defun random-phone-number ()
  (macrolet ((r (a)
	       `(svref ,a (random (load-time-value (length ,a))))))
    (let ((area-code (r #("415" "510" "408")))
	  (ex1 (r #("2" "3" "4" "5" "6" "7" "8" "9")))
	  (ex2 (r #("1" "2" "3" "4" "5" "6" "7" "8" "9")))
	  (ex3 (r #("1" "2" "3" "4" "5" "6" "7" "8" "9")))
	  (num (format nil "~4,'0d" (1+ (random 9999)))))
      (util.string:string+ #\( area-code #\) ex1 ex2 ex3 #\- num))))

#+never
(select (?from ?to)
  (get-triples-nd-within-radius ?triple :p !phone:phoneCallTargetLocation
				:latitude +37.756 :longitude -122.415 :units :km :radius 0.1
				:time-min "2014-08-18T15:20:00Z"
				:time-max "2014-08-18T15:25:00Z")
  (subject ?phonecall ?triple)
  (q ?phonecall !phone:phoneCallOriginator ?from)
  (q ?phonecall !phone:phoneCallReceiver   ?to))


#+never
(db.agraph.sparql:run-sparql
 "
PREFIX ndfn:  <http://franz.com/ns/allegrograph/4.0/geo/nd/f/>
PREFIX nd:    <http://franz.com/ns/allegrograph/4.0/geo/nd#>
PREFIX phone: <http://spooks.gov/phone-calls#>

SELECT *{

?call phone:phoneCallOriginLocation ?where .

}
LIMIT 5
")

#+never
(db.agraph.sparql:run-sparql
 "
PREFIX ndfn:  <http://franz.com/ns/allegrograph/4.0/geo/nd/f/>
PREFIX nd:    <http://franz.com/ns/allegrograph/4.0/geo/nd#>
PREFIX phone: <http://spooks.gov/phone-calls#>

SELECT *{

?call nd:inBoundingBox (phone:phoneCallOriginLocation
                        nd:latitude-min +37.755 nd:latitude-max +37.758
                        nd:longitude-mion -122.416 nd:longitude-max -122.414
                        nd:time-min \"2014-08-18T15:20:00Z\"
                        nd:time-max \"2014-08-18T15:25:00Z\") .

}
LIMIT 5
")
