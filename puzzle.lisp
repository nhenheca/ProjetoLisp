;;; OPERADORES ;;;
;;;Devolve a lista de operadores
(defun operadores (cl &optional (pos 1) (i 1))
 (cond
  ((equal pos (+ 1 cl)) nil)
  ((not (equal cl i)) (append (list(list 'arco-vertical pos i)) (list(list 'arco-horizontal pos i)) (criar-operadores cl pos (+ i 1))) )
  ((equal cl i) (criar-operadores cl (1+ pos) 1))
 )
)
;;;;;;;;;;;;;;;;;;


;;;MIGUEL GABRIEL MARQUES ########################################################

(defun n-caixas-fechadas (tabuleiro &optional (x 0)(y 0)(count 0))
 (cond
  (())
  (())
 )
)

;;Devolve o valor da heuristica de acordo com o estado do n�
(defun heuristica (no)
  0
)

;;;Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) Profundidade Heuristica Pai"
(defun no-teste ()
 (list (tabuleiro-teste) 0 (heuristica (tabuleiro-teste)) nil)
)

;;;Seleciona dentro do no o tabuleiro
(defun no-estado (no)
 (car no)
)



(defun no-profundidade (no-teste)
 (second no-teste)
)

(defun no-pai (no-teste)
 (fourth no-teste)
)

(defun no-heuristica (no-teste)
 (third no-teste)
)

(defun no-custo(no-teste)
  (+ (no-profundidade no-teste) (no-heuristica no-teste))
)

;;;Construtor
(defun cria-no (tabuleiro h &optional (g 0)(pai nil))
 (list tabuleiro g h pai)
)

(defun novo-sucessor (no op)
 (cria-no (funcall op) (+ 1 (no-profundidade no)) 0 no)
)

(defun sucessores (no opsList h &optional depth)
 (list (mapcar #'(lambda (op) (novo-sucessor no op)) opsList))
)
;;;MIGUEL GABRIEL MARQUES #########################################################

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
 (car (no-estado no))
)

;;;(get-arcos-verticais (no-teste))
;;;((0 0 0) (0 1 1) (1 0 1) (0 1 1))
(defun get-arcos-verticais(no)
 (car (cdr (no-estado no)))
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
;;;(arco-na-posicao 4 1 (get-arcos-verticais (no-teste)))
;;;((0 0 0) (0 1 1) (1 0 1) (1 1 1))
(defun arco-na-posicao (x y l &optional (z 1))
 (cond
  ((= x 1) (cons (substituir y (car l) z) (cdr l)))
  (t (cons (car l) (arco-na-posicao (1- x) y (cdr l) z)))
 )
)

;;; OPERADORES ;;;
;;;(arco-horizontal 3 1 (get-arcos-verticais (no-teste)))
;;;((0 0 0) (0 0 1) (1 1 1) (0 0 1) (0 0 0) (0 1 1) (1 0 1) (0 1 1))
(defun arco-horizontal (pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-horizontais (no-teste)))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-horizontais (no-teste)))) nil)
  (t (list (arco-na-posicao pos i (get-arcos-horizontais (no-teste)) z)  l))
 )
)

;;;(arco-vertical 1 2 (get-arcos-verticais (no-teste)))
;;;(((0 0 0) (0 0 1) (0 1 1) (0 0 1))((0 1 0) (0 1 1) (1 0 1) (0 1 1)))
(defun arco-vertical (pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-verticais (no-teste)))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-verticais (no-teste)))) nil)
  (t (list l (arco-na-posicao pos i (get-arcos-verticais (no-teste)) z)))
 )
)