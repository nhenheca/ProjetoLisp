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

(defun no-objetivop (no)
 (cond
  ((eq 0 (no-heuristica no)) T)
  (t nil)
 )
 
)

(defun get-cl ()
 4
)
(defun get-objective ()
 3
)

;;; ####################################################################################################################################################################################################################################
;;; BFS ################################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

(defun bfs (&optional (operadores (operadores)) (abertos (list (no-teste))) fechados pop-no (it 3))
 (cond
  ((eq it 3) (bfs operadores (cdr abertos) (append (list (car abertos))fechados) (car abertos) 5))
  ((not (eq nil (sucessor-e-objetivo (sucessores pop-no operadores)))) (list (sucessor-e-objetivo (sucessores pop-no operadores))(+(length abertos)(length fechados))(- (length fechados) 1)))
  ((eq it 5) (bfs operadores (append abertos (compile-sucessores-fechados (compile-sucessores-abertos (sucessores pop-no operadores) abertos) fechados)) fechados pop-no 0))  
  ((null abertos) nil)
  (t (bfs operadores abertos fechados 3))
 )
)

(defun no-existe-abertos (no abertos)
 (cond
  ((null abertos) no)
  ((eq (no-estado no)(no-estado (car abertos))) nil)
  (t (no-existe-abertos no (cdr abertos)))
 )
)

;;; Verifica se o no já existe em fechados com maior custo
;;; (no-existe-abertos [no: no a comparar][fechados: lista de fechados)
(defun no-existe-fechados (no fechados)
 (cond
  ((null fechados) no)
  ((eq (no-estado no)(no-estado (car fechados))) nil)
  (t (no-existe-fechados no (cdr fechados)))
 )
)

;;;Compila todos os nos que não existem em abertos com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessoresr][abertos: lista de abertos)
(defun compile-sucessores-abertos (sucessoresL abertos)
 (mapcar #'(lambda (x) (no-existe-abertos x abertos)) sucessoresL)
)

;;;Compila todos os nos que não existem em fechados com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessores][fechados: lista de fechados)
(defun compile-sucessores-fechados (sucessoresL fechados)
 (mapcar #'(lambda (x) (no-existe-fechados x fechados)) sucessoresL)
)

;;; ####################################################################################################################################################################################################################################
;;; DFS ################################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

(defun dls (depth &optional (operadores (operadores)) (stack (list (no-teste))) visited pop-no (it 1)(bool 0))
 (cond
  ((and (not(eq bool 1))(null stack)) nil)
  ((eq it 1)(dls depth operadores (cdr stack) (append (list (car stack)) visited) (car stack) 2 (+ bool 1)))
  ((and (eq it 2)(< (no-profundidade pop-no) depth))(dls depth operadores stack visited pop-no 3 1))
  ((and (eq it 2)(>= (no-profundidade pop-no) depth))(dls depth operadores stack visited pop-no 1 1))
  ((and (eq it 3)(sucessor-e-objetivo (sucessores pop-no operadores))) (list (sucessor-e-objetivo (sucessores pop-no operadores))(+(length stack)(length visited))(+ 1 (length visited))))
  (t (dls depth operadores (append (compile-sucessores-fechados (compile-sucessores-abertos (sucessores pop-no operadores) stack) visited) stack) visited pop-no 1 1))
 )
)

;;; ####################################################################################################################################################################################################################################
;;; A* #################################################################################################################################################################################################################################
;;; ####################################################################################################################################################################################################################################

;;; Implementação do A*.
;;; (a* [operadores: Conjunto de Operadores] [abertos: Lista de abertos inicializada com o no incial] [fechados: Lista de fechados vazia] [pop-no: Atomo que guarda o no retirado da lista de abertos] [it: Atmo de controlo dentro da função recursiva] [bool: Atmo que impede que a função recursiva de devolver nil na primeira iteração]
(defun a* (&optional (operadores (operadores)) (abertos (list (no-teste))) fechados pop-no (it 3)(bool 0))
 (cond
  ((and (not(eq bool 1))(null abertos)) nil) ;;; SE A LISTA DE ABERTOS FOR NULL ENTÃO NÃO EXSITE NO OBJETIVO
  ((eq it 3) (a* operadores (abertos-sem-no-menor-custo-a* abertos (pos-no-menor-custo-a* abertos)) fechados (no-menor-custo-a* abertos) 0 (+ bool 1))) ;;; RETIRA O NO DE MENOR CUSTO DE ABERTOS, GUARDA REFERENCIA NO "NO-POP"
  ((sucessor-e-objetivo (sucessores pop-no operadores)) (list (sucessor-e-objetivo (sucessores pop-no operadores))(+(length abertos)(length fechados)) (+ 1 (length fechados)))) ;;; SE ALGUM NO SUCESSOR FOR NO OBJETIVO DEVOLVE
  (t (a* operadores (append abertos (compile-sucessores-fechados-a* (compile-sucessores-abertos-a* (sucessores pop-no operadores) abertos) fechados)) (append (list pop-no) fechados) pop-no 3 1)) ;;; SENAO ABERTOS RECEBE NOS SUCESSORES NAO REPETIDOS OU REPETIDOS COM MENOR CUSTO , FECHADOS RECEBE O "NO-POP"
 )
)

;;; Devolve a posição na lista de abertos do no de menor custo.
;;; (pos-no-menor-custo-a* [abertos: Lista de abertos] [no: Primeiro no da Lista de abertos] [i: Atmo auxiliar] [pos: Atmo devolve a posição do no] )
(defun pos-no-menor-custo-a* (abertos &optional (no (car abertos)) (i 0)(pos 0))
 (cond
  ((null abertos) pos)
  ((< (no-custo (car abertos)) (no-custo no)) (pos-no-menor-custo-a* (cdr abertos) (car abertos) (+ i 1) i))
  (t (pos-no-menor-custo-a* (cdr abertos) no (+ i 1) pos))
 )
)

;;; Devolve o no da lista de abertos de menor custo.
;;; (no-menor-custo-a* [abertos: Lista de abertos] [no: Primeiro no da Lista de abertos] [i: Atmo auxiliar] [pos: Atmo devolve a posição do no] )
(defun no-menor-custo-a* (abertos &optional (no (car abertos)) (i 0)(pos 0))
 (cond
  ((null abertos) no)
  ((< (no-custo (car abertos)) (no-custo no)) (no-menor-custo-a* (cdr abertos) (car abertos) (+ i 1) i))
  (t (no-menor-custo-a* (cdr abertos) no (+ i 1) pos))
 )
)

;;; Devolve a lista de abertos sem o no de menor custo
;;; (pos-no-menor-custo-a* [abertos: Lista de abertos] [pos: Posição do no na lista])
(defun abertos-sem-no-menor-custo-a* (abertos pos)
 (cond
  ((eq 1(length abertos)) (cdr abertos))
  (t (append (subseq abertos 0 pos)(subseq abertos (+ pos 1))))
 )
)

;;; Verifica se no é objetivo
;;; (sucessor-e-objetivo [sucessoresL: Lista de sucessores])
(defun sucessor-e-objetivo (sucessoresL)
 (cond
  ((null sucessoresL) nil)
  ((no-objetivop (car sucessoresL)) (car sucessoresL))
  (t (sucessor-e-objetivo (cdr sucessoresL)))
 )
)

;;; Verifica se o no já existe em abertos com maior custo
;;; (no-existe-abertos [no: no a comparar][abertos: lista de abertos])
(defun no-existe-abertos-a* (no abertos)
 (cond
  ((null abertos) no)
  ((and (eq (no-estado no)(no-estado (car abertos)))(< (no-custo (car abertos))(no-custo no))) nil)
  (t (no-existe-abertos-a* no (cdr abertos)))
 )
)

;;; Verifica se o no já existe em fechados com maior custo
;;; (no-existe-abertos [no: no a comparar][fechados: lista de fechados)
(defun no-existe-fechados-a* (no fechados)
 (cond
  ((null fechados) no)
  ((and (eq (no-estado no)(no-estado (car fechados)))(< (no-custo (car fechados))(no-custo no))) nil)
  (t (no-existe-fechados-a* no (cdr fechados)))
 )
)

;;;Compila todos os nos que não existem em abertos com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessoresr][abertos: lista de abertos)
(defun compile-sucessores-abertos-a* (sucessoresL abertos)
 (mapcar #'(lambda (x) (no-existe-abertos-a* x abertos)) sucessoresL)
)

;;;Compila todos os nos que não existem em fechados com menor custo
;;; (no-existe-abertos [sucessoresL: lista de sucessores][fechados: lista de fechados)
(defun compile-sucessores-fechados-a* (sucessoresL fechados)
 (mapcar #'(lambda (x) (no-existe-fechados-a* x fechados)) sucessoresL)
)









