(in-package :acl-socket)
;; set the stale dns time to near zero so that 
;; when pods are changed to different IP addreses we know about this
(setq *stale-entry-remove-time* 5) ; 5 seconds

