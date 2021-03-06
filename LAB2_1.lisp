
;;Исходный список
(defun testList()
    `(54 33 68 44 55 69)
    )

;;Вставка эл-та
(defun insNode (lst num elt)
  (if (zerop num) (return-from insNode (cons elt lst)))
  (rplacd (nthcdr (- num 1) lst) (cons elt (nthcdr num lst)))
  lst)

;;Удаление эл-та
(defun rmNode (lst &optional (n x))
  (cond ((zerop n) (cdr lst))
        (t (cons (car lst) (rmNode (cdr lst) (- n 1))))))
  
;;Поиск эл-та
(defun findNode(x s position)
    (cond ((eq s nil) "NO ELEMENT")
          ((= x (car s)) position)
          (T (findNode x (cdr s) (+ position 1)))
))


;;Вывод данных
(format t "~a~%" (testList))
 
(format t "~a~%" (insNode  (testList) 2 33))
 
(format t "~a~%" (rmNode (testList) 3))

(format t "~a~%" (findNode 668 (testList) 0)) 
