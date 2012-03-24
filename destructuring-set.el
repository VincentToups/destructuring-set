(require 'cl)
(provide 'destructuring-set)

(defun dss:lambda-list-token (o)
  "Return t when o is a lambda list token."
  (or (eq o '&rest)
      (eq o '&key)
      (eq o '&optional)))

(defun dss:split-at-destruct-token (list)
  "Split the list LIST at any lambda-list token.  Returns a list
with the pre-token list and subsequent elements list."
  (assert (listp list)
	  ()
	  "dss:split-at-destruct-token expects a list.")
  (loop with pre = nil 
	while (and list (not (dss:lambda-list-token (car list))))
	do
	(push (pop list) pre)
	finally 
	(return (list (reverse pre)
		      list))))

(defun dss:collect-bindings (lambda-list)
  "Recursively collect all the bindings implied by the LAMBDA-LIST."
  (when lambda-list
    (let ((last-signifier :regular))
      (loop for item in lambda-list append 
	    (cond 
	     ((dss:lambda-list-token item)
	      (setq last-signifier item)
	      nil)
	     (:otherwise 
	      (case last-signifier 
		((:regular &rest)
		 (if (symbolp item)
		     (list item)
		   (dss:collect-bindings item)))
		((&key &optional)
		 (if (symbolp item)
		     (list item)
		   (let ((item (car item)))
		     (if (symbolp item)
			 (list item)
		       (dss:collect-bindings item))))))))))))

(defmacro destructuring-set 
  (binding value)
  "Like destructuring-bind, but sets the values in the current scope."
  (let ((values-flat-name (gensym))
	(bound-names (dss:collect-bindings binding)))
    `(let ((,values-flat-name
	    (destructuring-bind ,binding ,value
	      (vector ,@bound-names))))
       ,@(loop for sym in bound-names and
	       i from 0 collect
	       `(setq ,sym (elt ,values-flat-name ,i))))))





