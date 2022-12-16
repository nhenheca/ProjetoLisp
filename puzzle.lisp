;;;MIGUEL GABRIEL MARQUES ########################################################

(defun mostrar-solucao (lst &optional (count 1))
  (cond ((null lst) nil)
        (t (progn (format t "~A: ~A ~%" count (car lst)) (mostrar-solucao (cdr lst) (1+ count)))))
)

;;;Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) Profundidade Heuristica Pai"
(defun no-teste ()
 (list (tabuleiro-teste) 0 (heuristica (list (tabuleiro-teste) 0 0 nil) (get-cl) (get-objective)) nil)
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
(defun cria-no (tabuleiro g h pai)
 (list tabuleiro g h pai)
)

(defun novo-sucessor (pai op)
 (cond
  ((null (funcall (first op) (second op) (third op) (funcall (car (fourth op)) pai))) nil)
  (t (cria-no (funcall (first op) (second op) (third op) (funcall (car (fourth op)) pai)) (+ 1 (no-profundidade pai)) (heuristica (list (funcall (first op) (second op) (third op) (funcall (car (fourth op)) pai))) (get-cl) (get-objective))  pai))
 )
)

(defun sucessores (no opsList)
 (remove nil (mapcar #'(lambda (op) (novo-sucessor no op)) opsList))
)
;;;MIGUEL GABRIEL MARQUES #########################################################

;;; Tabuleiro
 "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
(defun tabuleiro-testeOG ()
    '(
        ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
        ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
    )
)
(defun tabuleiro-teste ()
    '(
        ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
        ((0 0 0) (0 1 0) (0 0 1) (0 1 1))
    )
)
(defun tabuleiro-teste1 ()
    '(
        ((0 0 1 0) (1 1 1 1) (0 0 1 1) (0 0 1 1) (0 0 1 1))
        ((0 0 1 1) (0 0 1 1) (1 1 1 1) (1 0 1 1) (0 1 1 1))
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

;;;(arco-vertical 1 2 (get-arcos-horizontais (no-teste)))
;;;(((0 0 0) (0 0 1) (0 1 1) (0 0 1))((0 1 0) (0 1 1) (1 0 1) (0 1 1)))
(defun arco-vertical (pos i l &optional (z 1))
 (cond
  ((equal nil (get-arco-na-posicao pos i (get-arcos-verticais (no-teste)))) nil)
  ((= 1 (get-arco-na-posicao pos i (get-arcos-verticais (no-teste)))) nil)
  (t (list l (arco-na-posicao pos i (get-arcos-verticais (no-teste)) z)))
 )
)

;;;Devolve a lista de operadores
(defun operadores (&optional (cl (length (car (no-estado (no-teste))))) (pos 1) (i 1))
 (cond
  ((equal pos (+ 1 cl)) nil)
  ((not (equal cl i)) (append (list(list 'arco-vertical pos i '(get-arcos-horizontais ))) (list(list 'arco-horizontal pos i '(get-arcos-verticais ))) (operadores cl pos (+ i 1))) )
  ((equal cl i) (operadores cl (1+ pos) 1))
 )
)

;;(get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))
(defun nCaixasFechadas (no cl &optional (iL 1)(posL 1)(iC 1)(posC 1)(caixas 0)(itNumber 1))
 (cond
  ((equal itNumber cl) caixas)
  ((equal cl posC) (nCaixasFechadas no cl 1 (+ posL 1) (+ iC 1) 1 caixas (+ itNumber 1)))
  ((and (equal 1 (get-arco-na-posicao posL iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao posC iC (get-arcos-verticais no)))(equal 1 (get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no)))) (nCaixasFechadas no cl (+ iL 1) posL iC (+ posC 1)(+ caixas 1) itNumber))
  ((not(and (equal 1 (get-arco-na-posicao posL iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no)))(equal 1 (get-arco-na-posicao posC iC (get-arcos-verticais no)))(equal 1 (get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no))))) (nCaixasFechadas no cl (+ iL 1) posL iC (+ posC 1) caixas itNumber))
 )
)

(defun nCaixasAbertas (no cl &optional (iL 1)(posL 1)(iC 1)(posC 1)(caixas 0)(itNumber 1))
 (cond
  ((equal itNumber cl) caixas)
  ((equal cl posC) (nCaixasAbertas no cl 1 (+ posL 1) (+ iC 1) 1 caixas (+ itNumber 1)))
  ((equal 3 (+ (get-arco-na-posicao posL iL (get-arcos-horizontais no))(get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no))(get-arco-na-posicao posC iC (get-arcos-verticais no))(get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no)))) (nCaixasAbertas no cl (+ iL 1) posL iC (+ posC 1)(+ caixas 1) itNumber))
  ((not (equal 3 (+ (get-arco-na-posicao posL iL (get-arcos-horizontais no))(get-arco-na-posicao (+ 1 posL) iL (get-arcos-horizontais no))(get-arco-na-posicao posC iC (get-arcos-verticais no))(get-arco-na-posicao (+ 1 posC) iC (get-arcos-verticais no))))) (nCaixasAbertas no cl (+ iL 1) posL iC (+ posC 1) caixas itNumber))
 )
)

(defun heuristica (no cl objective)
 (- objective (nCaixasFechadas no cl))
)

(defun get-cl ()
 4
)
(defun get-objective ()
 3
)

;;; Algoritmos
;; procura na largura
(defun bfs (operadores &optional (abertos (list (no-teste))) fechados pop-no (it 3))
 (cond
  ((eq it 3) (bfs operadores (cdr abertos) (cons (car abertos) fechados) (car abertos) (+ it 1)))
  ((eq it 4) (bfs operadores (append abertos (sucessores (car fechados) operadores)) fechados pop-no (+ it 1)))
  ((and (eq it 5) (not (eq nil (sucessor-e-objetivo-a* (sucessores pop-no operadores))))) (list (sucessor-e-objetivo-a* (sucessores pop-no operadores))(+(length abertos)(length fechados)) (length fechados)))
  ((null abertos) nil)
  (t (bfs operadores abertos fechados 3))
 )
)

(defun dfs (operadores depth &optional (abertos (list (no-teste))) fechados pop-no (it 3)(bool 0))
 (cond
  ((and (not(eq bool 1))(null abertos)) nil)
  ((eq it 3) (dfs operadores depth (cdr abertos) (cons (car abertos) fechados) (car abertos) 4 (+ bool 1)))
  ((and (eq it 4)(> (no-profundidade (car fechados)) depth)) (dfs operadores depth abertos fechados pop-no 3 1))
  ((and (eq it 4)(< (no-profundidade (car fechados)) depth)) (dfs operadores depth abertos fechados pop-no 5 1))
  ((eq it 5) (dfs operadores depth (append (sucessores (car fechados) operadores) abertos) fechados pop-no 6 1))
  ((and (eq it 6) (not (eq nil (sucessor-e-objetivo-a* (sucessores pop-no operadores))))) (list (sucessor-e-objetivo-a* (sucessores pop-no operadores))(+(length abertos)(length fechados)) (length fechados)))
  (t (dfs operadores depth abertos fechados pop-no 3 1))
 )
)

;;; A* ##################################################################

(defun a* (operadores &optional (abertos (list (no-teste))) fechados pop-no (it 3)(bool 0))
 (cond
  ;((and (not(eq bool 1))(null abertos)) nil)
  ((eq it 3) (a* operadores (abertos-sem-no-menor-custo-a* abertos (pos-no-menor-custo-a* abertos)) fechados (no-menor-custo-a* abertos) 0 (+ bool 1)))
  ((not (eq nil (sucessor-e-objetivo-a* (sucessores pop-no operadores)))) (list (sucessor-e-objetivo-a* (sucessores pop-no operadores))(+(length abertos)(length fechados)) (length fechados)))
  (t (a* operadores (compile-sucessores-fechados (compile-sucessores-abertos (sucessores pop-no operadores) abertos) fechados) (append pop-no fechados) pop-no 3 1))
  
 )
)

(defun pos-no-menor-custo-a* (abertos &optional (no (car abertos)) (i 0)(pos 0))
 (cond
  ((null abertos) pos)
  ((< (no-custo (car abertos)) (no-custo no)) (pos-no-menor-custo-a* (cdr abertos) (car abertos) (+ i 1) i))
  (t (pos-no-menor-custo-a* (cdr abertos) no (+ i 1) pos))
 )
)

(defun no-menor-custo-a* (abertos &optional (no (car abertos)) (i 0)(pos 0))
 (cond
  ((null abertos) no)
  ((< (no-custo (car abertos)) (no-custo no)) (no-menor-custo-a* (cdr abertos) (car abertos) (+ i 1) i))
  (t (no-menor-custo-a* (cdr abertos) no (+ i 1) pos))
 )
)

(defun abertos-sem-no-menor-custo-a* (abertos pos)
 (cond
  ((eq 1(length abertos)) (cdr abertos))
  (t (append (subseq abertos 0 pos)(subseq abertos (+ pos 1))))
 )
)

(defun sucessor-e-objetivo-a* (sucessoresL)
 (cond
  ((null sucessoresL) nil)
  ((eq (no-heuristica (car sucessoresL)) 0) (car sucessoresL))
  (t (sucessor-e-objetivo-a* (cdr sucessoresL)))
 )
)

(defun no-existe-abertos (no abertos)
 (cond
  ((null abertos) no)
  ((and (eq (no-estado no)(no-estado (car abertos)))(< (no-custo (car abertos))(no-custo no))) nil)
  (t (no-existe-abertos no (cdr abertos)))
 )
)

(defun no-existe-fechados (no fechados)
 (cond
  ((null fechados) no)
  ((and (eq (no-estado no)(no-estado (car fechados)))(< (no-custo (car fechados))(no-custo no))) nil)
  (t (no-existe-fechados no (cdr fechados)))
 )
)

(defun compile-sucessores-abertos (sucessoresL abertos)
 (mapcar #'(lambda (x) (no-existe-abertos x abertos)) sucessoresL)
)

(defun compile-sucessores-fechados (sucessoresL fechados)
 (mapcar #'(lambda (x) (no-existe-fechados x fechados)) sucessoresL)
)
