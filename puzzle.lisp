
CL-USER 1 > (get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))

Error: Undefined operator GET-ARCO-NA-POSICAO in form (GET-ARCO-NA-POSICAO 2 3 (GET-ARCOS-HORIZONTAIS (NO-TESTE))).
  1 (continue) Try invoking GET-ARCO-NA-POSICAO again.
  2 Return some values from the form (GET-ARCO-NA-POSICAO 2 3 (GET-ARCOS-HORIZONTAIS (NO-TESTE))).
  3 Try invoking something other than GET-ARCO-NA-POSICAO with the same arguments.
  4 Set the symbol-function of GET-ARCO-NA-POSICAO to another function.
  5 Set the macro-function of GET-ARCO-NA-POSICAO to another function.
  6 (abort) Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 2 : 1 > (get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))

Error: NTHCDR reached dotted end of list 0 before 2 elements.
  1 (abort) Return to debug level 1.
  2 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 3 : 2 > (get-arcos-verticais (no-teste))
((0 0 0) (0 1 1) (1 0 1) (0 1 1))

CL-USER 4 : 2 > (get-arcos-horizontais (no-teste))
((0 0 0) (0 0 1) (0 1 1) (0 0 1))

CL-USER 5 : 2 > (get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))

Error: NTHCDR reached dotted end of list 0 before 2 elements.
  1 (abort) Return to debug level 2.
  2 Return to debug level 1.
  3 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 6 : 3 > (nth (- x 1)(tabuleiro no))

Error: The variable X is unbound.
  1 (continue) Try evaluating X again.
  2 Return the value of :X instead.
  3 Specify a value to use this time instead of evaluating X.
  4 Specify a value to set X to.
  5 (abort) Return to debug level 3.
  6 Return to debug level 2.
  7 Return to debug level 1.
  8 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 7 : 4 > (nth (- 2 1)(tabuleiro no))

Error: The variable NO is unbound.
  1 (continue) Try evaluating NO again.
  2 Return the value of :NO instead.
  3 Specify a value to use this time instead of evaluating NO.
  4 Specify a value to set NO to.
  5 (abort) Return to debug level 4.
  6 Return to debug level 3.
  7 Return to debug level 2.
  8 Return to debug level 1.
  9 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 8 : 5 > (nth (- x 1)(tabuleiro (no-teste)))

Error: The variable X is unbound.
   1 (continue) Try evaluating X again.
   2 Return the value of :X instead.
   3 Specify a value to use this time instead of evaluating X.
   4 Specify a value to set X to.
   5 (abort) Return to debug level 5.
   6 Return to debug level 4.
   7 Return to debug level 3.
   8 Return to debug level 2.
   9 Return to debug level 1.
  10 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 9 : 6 > (nth (- 2 1)(tabuleiro (no-teste)))
((0 0 0) (0 1 1) (1 0 1) (0 1 1))

CL-USER 10 : 6 > (get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))

Error: NTHCDR reached dotted end of list 0 before 2 elements.
  1 (abort) Return to debug level 6.
  2 Return to debug level 5.
  3 Return to debug level 4.
  4 Return to debug level 3.
  5 Return to debug level 2.
  6 Return to debug level 1.
  7 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 11 : 7 > (nth (- 2 1)(get-arcos-horizontais(tabuleiro (no-teste))))
0

CL-USER 12 : 7 > (get-arcos-horizontais (no-teste))
((0 0 0) (0 0 1) (0 1 1) (0 0 1))
Error while reading: Unmatched right parenthesis.

CL-USER 13 : 7 > )

Error: Unmatched right parenthesis.
  1 (abort) Return to debug level 7.
  2 Return to debug level 6.
  3 Return to debug level 5.
  4 Return to debug level 4.
  5 Return to debug level 3.
  6 Return to debug level 2.
  7 Return to debug level 1.
  8 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.
Error while reading: Unmatched right parenthesis.

CL-USER 13 : 8 > (get-arcos-horizontais (no-teste))
((0 0 0) (0 0 1) (0 1 1) (0 0 1))

CL-USER 14 : 8 > (nth (- 2 1)(get-arcos-horizontais (no-teste)))
(0 0 1)

CL-USER 15 : 8 > (nth (- 3 1)(nth (- 2 1)(get-arcos-horizontais (no-teste))))
1

CL-USER 16 : 8 > (get-arco-na-posicao 2 3 (get-arcos-horizontais (no-teste)))
1

CL-USER 17 : 8 > 