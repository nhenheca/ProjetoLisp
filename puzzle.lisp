;;;MIGUEL GABRIEL MARQUES ########################################################

;;;Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) Profundidade Heuristica Pai"
(defun no-teste ()
 (list (tabuleiro-teste) 0 (heuristica (list (tabuleiro-teste) 0 0 nil) (get-objective)) nil)
)

;;;Seleciona dentro do no o tabuleiro
(defun no-estado (no)
 (car no)
)

(defun no-profundidade (no)
 (second no)
)

(defun no-pai (no)
 (fourth no)
)

(defun no-heuristica (no)
 (third no)
)

(defun no-custo(no)
  (+ (no-profundidade no) (no-heuristica no))
)

;;;Construtor
(defun cria-no (tabuleiro g h pai)
 (list tabuleiro g h pai)
)

(defun novo-sucessor (pai op)
 (cond
  ((equal nil (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai))) nil)
  (t (cria-no (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai)) (+ 1 (no-profundidade pai)) (heuristica (list (funcall (first op) pai (second op) (third op) (funcall (car (fourth op)) pai))) (get-objective))  pai))
 )
)

(defun sucessores (no opsList)
 (remove nil (mapcar #'(lambda (op) (novo-sucessor no op)) opsList))
)
;;;MIGUEL GABRIEL MARQUES #########################################################

;;; Tabuleiro
 "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"

(defun tabuleiro-teste ()
 (get-tabuleiro)
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
(defun arco-horizontal (no pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-horizontais no))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-horizontais no))) nil)
  (t (list (arco-na-posicao pos i (get-arcos-horizontais no) z)  l))
 )
)

;;;(arco-vertical 1 2 (get-arcos-horizontais (no-teste)))
;;;(((0 0 0) (0 0 1) (0 1 1) (0 0 1))((0 1 0) (0 1 1) (1 0 1) (0 1 1)))
(defun arco-vertical (no pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-verticais no))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-verticais no))) nil)
  (t (list l (arco-na-posicao pos i (get-arcos-verticais no) z)))
 )
)

;;;Devolve a lista de operadores
(defun operadores (cl &optional (pos 1) (i 1))
 (cond
  ((equal pos (+ 1 cl)) nil)
  ((not (equal cl i)) (append (list(list 'arco-vertical pos i '(get-arcos-horizontais ))) (list(list 'arco-horizontal pos i '(get-arcos-verticais ))) (operadores cl pos (+ i 1))) )
  ((equal cl i) (operadores cl (1+ pos) 1))
 )
)

;;(get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))
(defun nCaixasFechadas (no &optional (cl (length (car (no-estado no)))) (iL 1)(posL 1)(iC 1)(posC 1)(caixas 0)(itNumber 1))
 (cond
  ((equal itNumber cl) caixas)
  ((equal cl posC) (nCaixasFechadas no cl 1 (+ posL 1) (+ iC 1) 1 caixas (+ itNumber 1)))
  ((and (equal 1 (get-arco-na-posicao posL iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao posC iC (get-arcos-verticais no)))(equal 1 (get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no)))) (nCaixasFechadas no cl (+ iL 1) posL iC (+ posC 1)(+ caixas 1) itNumber))
  ((not(and (equal 1 (get-arco-na-posicao posL iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao posC iC (get-arcos-verticais no)))(equal 1 (get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no))))) (nCaixasFechadas no cl (+ iL 1) posL iC (+ posC 1) caixas itNumber))
 )
)

;;(get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))
(defun bom-vizinho (no &optional (cl (length (car (no-estado no)))) (iL 1)(posL 1)(iC 1)(posC 1)(soma 0)(itNumber 1))
 (cond
  ((equal itNumber cl) soma)
  ((equal cl posC) (bom-vizinho no cl 1 (+ posL 1) (+ iC 1) 1 soma (+ itNumber 1)))
  ((equal nil (remove nil (list (get-arco-na-posicao posL iL (get-arcos-horizontais no))(get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no))(get-arco-na-posicao posC iC (get-arcos-verticais no))(get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no))))) (bom-vizinho no cl (+ iL 1) posL iC (+ posC 1)(+ soma 4) itNumber))
  (t (bom-vizinho no cl (+ iL 1) posL iC (+ posC 1)  (+ soma (- 4 (apply '+ (remove nil (list (get-arco-na-posicao posL iL (get-arcos-horizontais no))(get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no))(get-arco-na-posicao posC iC (get-arcos-verticais no))(get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no))))))) itNumber))
 )
)

(defun heuristica (no &optional objective i)
 (cond
  ((equal i 1) (- objective (nCaixasFechadas no)))
  ((equal i 2) (bom-vizinho no))
 )
)

(defun no-objetivop (no objetivo &optional (cl (length (car (no-estado no)))))
 (cond
  ((equal objetivo (nCaixasFechadas no cl)) T)
  (t nil)
 )
)