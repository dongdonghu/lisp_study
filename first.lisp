(defvar *db* nil)
(defun make-cd (name price author)
  (list :name name :price price :author author))
(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "vol1" 25 "hdd2"))
(defun dump-db ()
  (dolist (record *db*)
    (format t "~{~a:~10t~a~%~}~%" record)))
(print *db*)
(dump-db)

(defun prompt-read (prompt)
  (format *query-io* "~a:" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun save-db (path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(print *db*)
(setf *path*  "/Users/dhu/study/lisp_wk/first.db")
(save-db  *path*)
(defun load-db (path)
  (with-open-file (in path)
    (with-standard-io-syntax
      (setf *db* (read in)))))
(load-db *path*)

(defun  select-by-author (author)
  (remove-if-not #'(lambda (x) (equal author (getf x :author))) *db*))
(select-by-author "hdd")

(defun select-by-fn (fn)
  (remove-if-not fn *db*))

(defun make-selector-fn (key value)
  #'(lambda (x)
      (equal (getf x key) value)))
(select-by-fn (make-selector-fn :author "hdd"))

(defun where2 (&key name price author)
  #'(lambda (cd)
      (and
       (if name (equal (getf cd :name) name) t)
       (if price (equal (getf cd :price) price) t)
       (if author (equal (getf cd :author) author) t)
       )

      ))
(select-by-fn (where2 :author "hdd" :name "vol1"))

(defun make-where-sub-cluase (key value)
  `(equal (getf cd ,key) ,value))
(defun make-where-list (fields)
  (loop while fields
     collecting (make-where-sub-cluase (pop fields) (pop fields))))
(defmacro where (&rest clause)
  `#'(lambda (cd) (and ,@(make-where-list clause))))
(macroexpand-1 '(where :name "dd" :author))

(select-by-fn (where :name "vol1" :author ))
(defparameter *tdata* '(:name "hdd" :age "22"))
(print *tdata*)
(mapcar #'(lambda (x y) (print (list x y)))  *tdata* (rest *tdata*))
