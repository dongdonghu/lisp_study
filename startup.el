(defun scope-test (x)
  (lambda (y) (concat x y)))
(defun ft ()
  (lambda (x) (message x)))
((ft) "123")
((scope-test 1) 2)
;;((scope-test 1) 2)

(defun scopex (x y)
  (message (string  (+ x y))))

(scopex 1 5)
