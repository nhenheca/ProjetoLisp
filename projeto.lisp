(defun get-solucao(tabuleiro)
  (second tabuleiro)
)

(defun get-tab-size(tabuleiro)
  (length (caar tabuleiro))
)

;;NOVO
(defun my-get-tab (i path)
 (with-open-file (stream "C:/Users/202001391/Documents/GitHub/ProjetoLisp/problem.dat" :direction :input) 
  (tab stream i) 
 )
)

;; Contagem a partir de 0
(defun tab (stream i)
 (let ((problema (read stream)))
  (cond
   ((zerop i) problema)
   (t (tab stream (- i 1)))
  )
 )
)
