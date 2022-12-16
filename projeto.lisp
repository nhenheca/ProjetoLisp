;FORMULA (+ i (- i 1))
;Aqui queremos uma lista de tabuleiros com o que pretendemos no final
(defun get-file (i)
  (with-open-file (stream "~/Desktop/problem.dat") 
    (handler-case ;try
        (loop repeat (+ i (1- i)) collect (read stream))
      (error (c) ;catch
        (format t "Tabuleiro ~d inválido." i)
        (values))))
)



;######################################
;  (cond ((= i 1) (get-file i)) ;0
;        ((= i 2) (cddr (get-file i))) ;2
;        ((= i 3) (cddddr (get-file i))) ;4
;        ((= i 4) (cddr (cddddr (get-file i)))) ;6
;        ((= i 5) (cddddr (cddddr (get-file i)))) ;8
;        ((= i 6) (cddr (cddddr (cddddr (get-file i))))) ;10
;  )
; FORMULA: (* (- i 1) 2)
; TESTE: (get-tabs 1)
(defun get-tab (i &optional (temp 0)(cdrs 0))          
  (cond ((= temp 0) (car (get-tab i 1 (* (- i 1) 2))))      ;Calcula quantos CDRs vai ter de fazer
        ((> cdrs 0) (cdr (get-tab i 1 (- cdrs 1))))         ;Faz todos os CDRs até ao que pretendemos
        ((= cdrs 0) (get-file i)))                          ;Vai buscar a lista de todos os tabuleiros com o pretendido no final
)
;;; BURRAO #############################################

(defun get-solucao(tabuleiro)
  (second tabuleiro)
)

(defun get-tab-size(tabuleiro)
  (length (caar tabuleiro))
)

;;NOVO
(defun my-get-tab (i)
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