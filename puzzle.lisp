
;;;MIGUEL ########################################################

(defun heuristica (no)
  0
)

 "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) Profundidade Heuristica Pai"
(defun no-teste ()
 (list (tabuleiro-teste) 0 (heuristica (tabuleiro-teste)) nil)
)

;;; SELETORES ;;;
(defun tabuleiro (no)
 (car no)
)

;;;MIGUEL #########################################################

;;; Tabuleiro
 "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
(defun tabuleiro-teste ()
    '(
        ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
        ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
    )
)

;;; SELETORES ;;;
;;;(get-arcos-horizontais (no-teste))
;;;((0 0 0) (0 0 1) (0 1 1) (0 0 1))
(defun get-arcos-horizontais(no)
 (car (tabuleiro no))
)

;;;(get-arcos-verticais (no-teste))
;;;((0 0 0) (0 1 1) (1 0 1) (0 1 1))
(defun get-arcos-verticais(no)
 (car (cdr (tabuleiro no)))
)

;;(get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))
;;1
(defun get-arco-na-posicao (x y l)
 (nth (- y 1)(nth (- x 1) l))
)

;;; AUXILIARES ;;;
;;;(substituir 1 (car (get-arcos-horizontais (no-teste))))
;;;(1 0 0)
;;;(substituir 2 (car (get-arcos-verticais (no-teste))) 2)
;;;(0 2 0)
(defun substituir(x l &optional (y 1))
 (cond
  ((= x 1) (cons y (cdr l)))
  (t (cons (car l) (substituir (1- x) (cdr l) y)))
 )
)

;;;(arco-na-posicao 2 2 (get-arcos-horizontais (no-teste)))
;;;((0 0 0) (0 1 1) (0 1 1) (0 0 1))
;;;(arco-na-posicao 4 1 (get-arcos-verticais (tabuleiro-teste)))
;;;((0 0 0) (0 1 1) (1 0 1) (1 1 1))
(defun arco-na-posicao (x y l &optional (z 1))
 (cond
  ((= x 1) (cons (substituir y (car l) z) (cdr l)))
  (t (cons (car l) (arco-na-posicao (1- x) y (cdr l) z)))
 )
)

;;; OPERADORES ;;;
(defun arco-horizontal (pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-horizontais (tabuleiro-teste)))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-horizontais (tabuleiro-teste)))) nil)
  (t (append (arco-na-posicao pos i (get-arcos-horizontais (tabuleiro-teste)) z)  l))
 )
)

(defun arco-vertical (pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-verticais (tabuleiro-teste)))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-verticais (tabuleiro-teste)))) nil)
  (t (append (arco-na-posicao pos i (get-arcos-verticais (tabuleiro-teste)) z)  l))
 )
)