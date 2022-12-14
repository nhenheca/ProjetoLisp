;;;MIGUEL GABRIEL MARQUES ########################################################

(defun mostrar-solucao (lst &optional (count 1))
  (cond ((null lst) nil)
        (t (progn (format t "~A: ~A ~%" count (car lst)) (mostrar-solucao (cdr lst) (1+ count)))))
)

;;;Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal) Profundidade Heuristica Pai"
(defun no-teste ()
 (list (tabuleiro-teste) 0 (heuristica (list (tabuleiro-teste) 0 0 nil) 4 5) nil)
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
  (t (cria-no (funcall (first op) (second op) (third op) (funcall (car (fourth op)) pai)) (+ 1 (no-profundidade pai)) (heuristica (list (funcall (first op) (second op) (third op) (funcall (car (fourth op)) pai))) 4 5)  pai))
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
(defun operadores (cl &optional (pos 1) (i 1))
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

;;; Algoritmos
;; procura na largura
+
(defun bfs (no sucessores operadores &optional abertos fechados)
  (cond ((eq (no-existep no fechados) nil) (bfs no 'sucessores operadores (funcall sucessores no operadores) (cons no fechados))) ;; primeira vez que correr o código, gerar sucessores e acrescentar no inicial nos fechados
        ((null abertos) nil)  ;;se abertos tiver vazio, retorna nil
        ((eq (no-heuristica (car abertos)) 0) (car abertos))  ;; se o primeiro valor de abertos == solução, então retorna primeiro valor de abertos
        ((eq (no-existep (car abertos) fechados) nil) (bfs no 'sucessores operadores (append (cdr abertos) (funcall sucessores (car abertos) operadores)) (cons (car abertos) fechados))) ;;se primeiro de abertos nao tiver nos fechados, remover de abertos, gerar sucessores e acrescentar nos fechados
        (t (bfs no 'sucessores operadores (cdr abertos) fechados))) ;; se ja existir nos fechados, apenas tirar no dos abertos e nao acrescentar
)

;; no-existep
(defun no-existep(no lst) ;; perguntar ao professor porque position, find & member doesn't work with searching lists
  (cond ((null lst) nil)
        ((equal (no-estado no) (no-estado (car lst))) t)
        (t (no-existep no (cdr lst))))
)