;;NOVO
(defun start ()
 (menu-escolher-tabuleiro)
)

(defun my-get-tab (i)
 (with-open-file (stream "C:/Users/nhenhecas/Documents/GitHub/ProjetoLisp/problem.dat" :direction :input) 
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

(defun get-number-of-lines ()
 (with-open-file (stream "C:/Users/nhenhecas/Documents/GitHub/ProjetoLisp/problem.dat" :direction :input) 
  (get-number-of-lines-aux stream) 
 )
)

;; Contagem a partir de 0
(defun get-number-of-lines-aux (stream &optional (i 0))
 (cond
  ((eq nil (read stream nil nil)) i)
  (t (get-number-of-lines-aux stream (+ i 1)))
 )   
)

;;;##################################################################################################################################################
;;;TABULEIRO ########################################################################################################################################
;;;##################################################################################################################################################

(defun menu-escolher-tabuleiro ()
 (format t "|-------------------------|~%")
 (format t "|                         |~%")
 (format t "|   ESCOLHA O TABULEIRO   |~%")
 (format t "|                         |~%")
 (menu-escolher-tabuleiro-content)
 (ler-op-tabuleiro)
 (menu-escolher-algoritmo)
)

(defun menu-escolher-tabuleiro-content (&optional (i (get-number-of-lines)))
 (cond
  ((zerop i) (format t "|                         |~%|-------------------------|~%"))
  (t (format t "|    ~d - Tabuleiro ~d      |~%" i i)(menu-escolher-tabuleiro-content (- i 1)))
 )
)

(defun ler-op-tabuleiro ()
 (format t "Digite o numero do tabuleiro indicados: ")
 (with-open-file (str "C:/Users/nhenhecas/Documents/GitHub/ProjetoLisp/memoria.pt" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format str "~a" (my-get-tab (- (read) 1)))
 )
)

(defun menu-escolher-algoritmo ()
 (format t "|-------------------------|~%")
 (format t "|                         |~%")
 (format t "|   ESCOLHA O ALGORITMO   |~%")
 (format t "|                         |~%")
 (format t "|       1 - BFS           |~%")
 (format t "|       2 - DLS           |~%")
 (format t "|       3 - A*            |~%")
 (format t "|                         |~%")
 (format t "|       0 - Voltar        |~%")
 (format t "|                         |~%")
 (format t "|-------------------------|~%")
 (ler-op-algoritmo)
)

(defun ler-op-algoritmo ()
 (format t "Digite o algoritmo do tabuleiro indicados: ")
 (let ((op (read)))
  (cond
   ((eq 2 op) (format t "~%Digite a profundidade: ")(dls (read)))
   ((eq 1 op) (bfs))
   ((eq 3 op) (a*))
   ((eq 0 op) (menu-escolher-tabuleiro))
   (t (menu-escolher-algoritmo))
  )
 )
)
