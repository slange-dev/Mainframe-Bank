
       IDENTIFICATION DIVISION.
           PROGRAM-ID. CONTACORRENTE.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
               SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
              SELECT F-CADASTRO ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS cod-conta
               ALTERNATE RECORD KEY nome
                   WITH DUPLICATES
                   FILE STATUS   ARQ-OK.

       DATA DIVISION.
       FILE SECTION.
           FD F-CADASTRO LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "F-DATA.txt".
           01 DADOS-CADASTRO.
               02 cod-conta   PIC 9(5).
               02 cod-agencia PIC 9(5).
               02 nome        PIC AAAAAAAAAA VALUE SPACES.
      *         02 saldo       PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
               02 saldo       PIC $---.---.---.--9,99.

       WORKING-STORAGE SECTION.
       01 busca-nome    PIC AAAAAAAAAA VALUE SPACES.
       01 opcao    PIC X(1) VALUE SPACES.
       01 continua PIC X(1) VALUE SPACES.
       01 fim      PIC X.
       01 prosseguir PIC X.
       01 rodar    PIC X.
       01 WS-DADOS-CADASTRO.
          02 ws-cod-conta   PIC 9(5).
          02 ws-cod-agencia PIC 9(5).
          02 ws-nome        PIC AAAAAAAAAA VALUE SPACES.
      *    02 ws-saldo       PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
          02 ws-saldo       PIC $---.---.---.--9,99.
          02 ARQ-OK    PIC X(02) VALUE ZEROES.
      *   02 QQRMERDA  PIC

       SCREEN SECTION.
       01 TELA-INFO.
        03 BLANK SCREEN.
        03 LINE 13 COL 01 VALUE "Codigo da conta: ".
        03 LINE 14 COL 01 VALUE "Codigo da agencia: ".
        03 LINE 15 COL 01 VALUE "Dono da conta: ".
        03 LINE 16 COL 01 VALUE "Saldo na conta: ".

       01 TELA-MENU.
        03 BLANK SCREEN BACKGROUND-COLOR 3.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "               MENU                "
         HIGHLIGHT.
        03 LINE 03 COL 01 VALUE "***********************************".
        03 LINE 04 COL 01 VALUE "ESCOLHA UMA OPCAO ABAIXO:          ".
        03 LINE 05 COL 01 VALUE "(1) INCLUIR NOVO CADASTRO          ".
        03 LINE 06 COL 01 VALUE "(2) EXCLUIR CADASTRO               ".
        03 LINE 07 COL 01 VALUE "(3) ALTERAR CADASTRO               ".
        03 LINE 08 COL 01 VALUE "(4) CONSULTAR  CADASTRO            ".
        03 LINE 09 COL 01 VALUE "(5) LISTAR TODOS OS CADASTROS      ".
        03 LINE 11 COL 01 VALUE "(S) SAIR                           ".
        03 LINE 12 COL 01 VALUE "OPCAO:                             ".

       01 TELA-INCLUSAO.
        03 BLANK SCREEN.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "     INCLUSAO DE NOVO CADASTRO".
        03 LINE 03 COL 01 VALUE "***********************************".
        03 LINE 04 COL 01 VALUE "Conta Corrente: ".
        03 LINE 05 COL 01 VALUE "Codigo da agencia: ".
        03 LINE 06 COL 01 VALUE "Nome: ".
        03 LINE 07 COL 01 VALUE "Saldo:          R$".

       01 TELA-CONSULTA.
        03 BLANK SCREEN.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "              CONSULTA".
        03 LINE 03 COL 01 VALUE "***********************************".
        03 LINE 05 COL 01 VALUE "Consultar pelo codigo da conta ou".
        03 LINE 06 COL 01 VALUE "pelo nome do cliente?".
        03 LINE 08 COL 01 VALUE "     (1)    CODIGO DA CONTA".
        03 LINE 09 COL 01 VALUE "     (2)    NOME DO CLIENTE".
        03 LINE 10 COL 01 VALUE "     (S)    SAIR E VOLTAR AO MENU".
        03 LINE 11 COL 01 VALUE "     OPCAO: ".

       01 TELA-CONSULTA-CONTA.
        03 BLANK SCREEN.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "         CONSULTA POR CONTA".
        03 LINE 03 COL 01 VALUE "***********************************".
        03 LINE 05 COL 01 VALUE "Digite o codigo da conta: ".

       01 TELA-CONSULTA-NOME.
        03 BLANK SCREEN.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "         CONSULTA POR NOME".
        03 LINE 03 COL 01 VALUE "***********************************".
        03 LINE 05 COL 01 VALUE "Digite um nome de dono da conta: ".

       01 TELA-INFO2.
        03 BLANK SCREEN.
        03 LINE 07 COL 01 VALUE "Codigo da conta: ".
        03 LINE 08 COL 01 VALUE "Codigo da agencia: ".
        03 LINE 09 COL 01 VALUE "Dono da conta: ".
        03 LINE 10 COL 01 VALUE "Saldo na conta: ".

       01 TELA-EXCLUSAO.
        03 BLANK SCREEN.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "         TELA DE EXCLUSAO".
        03 LINE 03 COL 01 VALUE "***********************************".
        03 LINE 04 COL 01 VALUE "Digite o codigo da conta:".

       01 TELA-LISTAR.
        03 BLANK SCREEN.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "         LISTA DE CADASTROS".
        03 LINE 03 COL 01 VALUE "***********************************".

       01 TELA-ALTERACAO.
        03 BLANK SCREEN.
        03 LINE 01 COL 01 VALUE "***********************************".
        03 LINE 02 COL 01 VALUE "         TELA DE ALTERACAO".
        03 LINE 03 COL 01 VALUE "***********************************".
        03 LINE 04 COL 01 VALUE "     DIGITE O CODIGO DA CONTA      ".
        03 LINE 05 COL 01 VALUE "     A SER ALTERADA:".

       01 TELA-INFO-ALTERACAO.
        03 BLANK SCREEN.
        03 LINE 07 COL 01 VALUE
             "======================================================".
        03 LINE 08 COL 01 VALUE "  Codigo da conta:".
        03 LINE 09 COL 01 VALUE
             "------------------------------------------------------".
        03 LINE 10 COL 01 VALUE "Codigo da agencia:".
        03 LINE 11 COL 01 VALUE "       Novo valor:".
        03 LINE 12 COL 01 VALUE
             "------------------------------------------------------".
        03 LINE 13 COL 01 VALUE "    Dono da conta:".
        03 LINE 14 COL 01 VALUE "       Novo valor:".
        03 LINE 15 COL 01 VALUE
             "------------------------------------------------------".
        03 LINE 16 COL 01 VALUE "   Saldo na conta:".
        03 LINE 17 COL 01 VALUE "       Novo valor:".
        03 LINE 18 COL 01 VALUE
             "======================================================".


*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
       PROCEDURE DIVISION.
       PRINCIPAL.
       MOVE "f" TO fim.
       PERFORM MENU-PRINCIPAL UNTIL fim = "v".
       STOP RUN.

      ****************************** MENU PRINCIPAL *****************************
       MENU-PRINCIPAL.
       INITIALIZE opcao.
      *DISPLAY erase AT 0101.
       DISPLAY TELA-MENU.
       PERFORM OPCAO-MENU.

       OPCAO-MENU.
       INITIALIZE rodar.
       INITIALIZE opcao.
       ACCEPT opcao AT 1208.
        EVALUATE opcao
         WHEN "1" PERFORM INCLUSAO
         WHEN "2"  PERFORM EXCLUSAO UNTIL rodar = "v"
         WHEN "3"  PERFORM ALTERACAO UNTIL rodar = "v"
         WHEN "4"  PERFORM CONSULTA
         WHEN "5"  PERFORM LISTAR
         WHEN "S"  PERFORM sair
         WHEN "s"  PERFORM sair
         WHEN OTHER DISPLAY " Op��o Inv�lida"
        END-EVALUATE.

      ******************************* INCLUSAO ************************************
       INCLUSAO.
       INITIALIZE opcao.
       INITIALIZE dados-cadastro.
       INITIALIZE ws-dados-cadastro.
       MOVE "f" TO prosseguir.
      *DISPLAY erase AT 0101.
       DISPLAY TELA-INCLUSAO.
       OPEN OUTPUT F-CADASTRO.
       IF ARQ-OK NOT = "00" THEN
           DISPLAY "Erro de Arquivo. Erro:", ARQ-OK AT 3001
           CLOSE F-CADASTRO
       ELSE
           DISPLAY "ARQUIVO OK", ARQ-OK AT 3001
       END-IF
       OPEN EXTEND F-CADASTRO.
        PERFORM ENTRADA-CODIGO  UNTIL prosseguir = "v".
        PERFORM ENTRADA-AGENCIA UNTIL ws-cod-agencia NOT EQUAL ZEROES.
        PERFORM ENTRADA-NOME    UNTIL ws-nome NOT EQUAL SPACES.
        PERFORM ENTRADA-SALDO.
        MOVE "f" TO prosseguir.
        PERFORM PERGUNTA-SALVAR UNTIL prosseguir = "v".
       CLOSE F-CADASTRO.

       ENTRADA-CODIGO.
       MOVE "v" TO prosseguir.
       INITIALIZE cod-conta.
       ACCEPT cod-conta AT 0420.
       IF cod-conta = ZEROES
       THEN
         DISPLAY "Cadastro deve ser diferente de zero!" AT 0430
         MOVE "f" TO prosseguir
       ELSE
         DISPLAY "                                    " AT 0430
         READ F-CADASTRO
           NOT INVALID KEY
             DISPLAY "Ja cadastrado" AT 0430
           MOVE cod-conta TO ws-cod-conta

           DISPLAY "COD-CONTA    ", cod-conta
           DISPLAY "WS-COD-CONTA ", ws-cod-conta

            MOVE "f" TO prosseguir
         END-READ
       END-IF.

       ENTRADA-AGENCIA.
       INITIALIZE ws-cod-agencia.
       ACCEPT ws-cod-agencia AT 0520.
       IF ws-cod-agencia = ZEROES
       THEN
         DISPLAY "Agencia deve ser diferente de zero!" AT 0530
       ELSE
         DISPLAY "                                   " AT 0530
         MOVE ws-cod-agencia TO cod-agencia
       END-IF.

       ENTRADA-NOME.
       INITIALIZE ws-nome.
       ACCEPT ws-nome AT 0620.
       IF ws-nome = SPACES
       THEN
         DISPLAY "Nome esta em branco!" AT 0730
       ELSE
         DISPLAY "                    " AT 0730
         MOVE ws-nome TO nome
       END-IF.

       ENTRADA-SALDO.
       INITIALIZE ws-saldo.
       ACCEPT ws-saldo AT 0720.
       MOVE ws-saldo TO saldo.

       PERGUNTA-SALVAR.
       INITIALIZE opcao.
       DISPLAY "Salvar os dados? (S\N):  " AT 0910.
       ACCEPT opcao AT 0935.
       IF opcao = "s" or "S"
       THEN
          PERFORM ESCREVER-DADOS
      *     DISPLAY "Cadastrado com Sucesso"
           MOVE "v" TO prosseguir
       ELSE
           IF opcao equals "n" or "N"
           THEN
               DISPLAY " Dados nao foram salvos" AT 0937
               MOVE "v" TO prosseguir
           ELSE
               DISPLAY " Opcao invalida!       " AT 0937
               MOVE "f" TO prosseguir
           END-IF
       END-IF.

       ESCREVER-DADOS.
       WRITE DADOS-CADASTRO
       INVALID KEY
           DISPLAY "!!??" AT 1215
               NOT INVALID KEY
               DISPLAY "Registrado com sucesso! " AT 1210
      *        PERFORM MOSTRAR-DADOS-SALVOS
               ACCEPT continua
       END-WRITE.

       MOSTRAR-DADOS.
       READ F-CADASTRO RECORD INTO WS-DADOS-CADASTRO
           KEY IS cod-conta
       END-READ.
       DISPLAY TELA-INFO.
       DISPLAY ws-cod-conta AT 1320.
       DISPLAY ws-cod-agencia AT 1420.
       DISPLAY ws-nome AT 1520.
       DISPLAY ws-saldo AT 1620.

       MOSTRAR-DADOS-SALVOS.
       READ F-CADASTRO RECORD INTO WS-DADOS-CADASTRO
           KEY IS cod-conta
       END-READ.
       DISPLAY SPACES.
       DISPLAY SPACES.
       DISPLAY "======================================================".
       DISPLAY "Codigo da conta:       " ws-cod-conta.
       DISPLAY "Codigo da agencia:     " ws-cod-agencia.
       DISPLAY "Nome do dono da conta: " ws-nome.
       DISPLAY "Saldo da conta:        " ws-saldo.
       DISPLAY "======================================================".
       INITIALIZE WS-DADOS-CADASTRO.
       DISPLAY SPACES.
       DISPLAY "Registrado com sucesso! ".
       DISPLAY "Enter para continuar.".

      *******************************************************************

       RODAR-ALTERACAO.
       MOVE "f" TO rodar.
       PERFORM ALTERACAO.


       ALTERACAO.
       MOVE "f" TO rodar.
       INITIALIZE opcao.
       MOVE "f" TO prosseguir.
      *DISPLAY erase AT 0101.
       DISPLAY TELA-ALTERACAO.
       OPEN I-O F-CADASTRO.
           PERFORM UNTIL prosseguir = "v"
               INITIALIZE cod-conta
               INITIALIZE WS-DADOS-CADASTRO
               ACCEPT cod-conta AT 0522
               READ F-CADASTRO INTO WS-DADOS-CADASTRO
                 KEY IS cod-conta
                 INVALID KEY
                  DISPLAY "Codigo invalido!" AT 0530
                  PERFORM PERGUNTA-SAIR-ALTERACAO UNTIL prosseguir = "v"
                  IF rodar = "f" THEN
                       MOVE "v" TO prosseguir
                  ELSE
                       MOVE "f" TO prosseguir
                 NOT INVALID KEY
                   DISPLAY "                " AT 0530
                   PERFORM ALTERAR-CONTA UNTIL prosseguir = "v"
                   MOVE "v" TO prosseguir
               END-READ
            END-PERFORM.
       CLOSE F-CADASTRO.
       MOVE "v" TO rodar.


       PERGUNTA-SAIR-ALTERACAO.
       MOVE "f" TO  prosseguir.
       DISPLAY         "Deseja voltar ao menu? (S/N):" AT 0601.
       INITIALIZE opcao.
       ACCEPT opcao AT 0631.
       IF opcao = "S" or "s" THEN
           MOVE "v" TO prosseguir
           MOVE "f" TO rodar
       ELSE
           IF opcao = "N" or "n" THEN
               MOVE "v" TO prosseguir
               MOVE "v" TO rodar
               DISPLAY "Entre com uma conta valida!       " AT 0601
           ELSE
               DISPLAY "Opcao invalida"
               MOVE "f" TO prosseguir
               MOVE "v" TO rodar
           END-IF
       END-IF.

       ALTERAR-CONTA.
       DISPLAY SPACES.
       DISPLAY TELA-INFO-ALTERACAO.
       DISPLAY ws-cod-conta AT 0820.
       DISPLAY ws-cod-agencia AT 1020.
       MOVE "f" TO prosseguir.
       PERFORM UNTIL prosseguir = "v"
      *    MOVE "f" TO prosseguir
           ACCEPT ws-cod-agencia AT 1120
           IF ws-cod-agencia equals ZEROES THEN
               DISPLAY "Codigo deve ser diferente de zero!" AT 1127
               MOVE "f" TO prosseguir
           ELSE
               DISPLAY "                                  " AT 1127
               MOVE "v" TO prosseguir
           END-IF
       END-PERFORM.
       DISPLAY ws-nome AT 1320.
       MOVE "f" TO prosseguir.
       PERFORM UNTIL prosseguir = "v"
           MOVE "f" TO prosseguir
           ACCEPT ws-nome AT 1420
           IF ws-nome equals SPACES THEN
               DISPLAY "O nome esta em branco!" AT 1435
               MOVE "f" TO prosseguir
           ELSE
               DISPLAY "                      " AT 1435
               MOVE "v" TO prosseguir
           END-IF
       END-PERFORM.
       DISPLAY ws-saldo AT 1620.
       ACCEPT ws-saldo AT 1720.
       MOVE "f" TO prosseguir.
       PERFORM PERTGUNTA-ALTERAR UNTIL prosseguir = "v".
       DISPLAY SPACES.
       DISPLAY "ENTER para continuar."
       ACCEPT continua.
       MOVE "v" TO prosseguir.

       PERTGUNTA-ALTERAR.
       MOVE "f" TO prosseguir.
       DISPLAY "Tem certeza que quer alterar esta conta (S/N)?" AT 1901.
       INITIALIZE opcao.
       ACCEPT opcao AT 1947.
       IF opcao = "s" or "S" THEN
           MOVE WS-DADOS-CADASTRO TO DADOS-CADASTRO
           REWRITE DADOS-CADASTRO
           END-REWRITE
           DISPLAY "Os dados foram salvos!    " AT 2001
           MOVE "v" TO prosseguir
       ELSE
           IF opcao = "n" OR "N" THEN
               DISPLAY "Os dados nao foram salvos." AT 2001
               MOVE "v" TO prosseguir
           ELSE
               DISPLAY "Comando invalido!!!       " AT 2001
               MOVE "f" TO prosseguir
           END-IF
       END-IF.

      *******************************************************************


       EXCLUSAO.
       MOVE "f" TO rodar.
       INITIALIZE opcao.
       MOVE "f" TO prosseguir.
      *DISPLAY erase AT 0101.
       DISPLAY TELA-EXCLUSAO.
       OPEN I-O F-CADASTRO.
           PERFORM UNTIL prosseguir = "v"
               INITIALIZE cod-conta
               INITIALIZE WS-DADOS-CADASTRO
               ACCEPT cod-conta AT 0427
               READ F-CADASTRO INTO WS-DADOS-CADASTRO
                 KEY IS cod-conta
                 INVALID KEY
                   DISPLAY "Conta inexistente!" AT 0435
                   PERFORM PERGUNTA-SAIR-EXCLUSAO UNTIL prosseguir = "v"
                   IF rodar = "f" THEN
                       MOVE "v" TO prosseguir
                   ELSE
                       MOVE "f" TO prosseguir
                 NOT INVALID KEY
                   DISPLAY "                  " AT 0435
                   PERFORM PERGUNTA-EXCLUIR UNTIL prosseguir = "v"
                   MOVE "v" TO prosseguir
               END-READ
            END-PERFORM.
       CLOSE F-CADASTRO.
       MOVE "v" TO rodar.
       DISPLAY " ENTER para sair.".
       ACCEPT continua.

       PERGUNTA-SAIR-EXCLUSAO.
       MOVE "f" TO  prosseguir.
       DISPLAY         "Deseja voltar ao menu? (S/N):" AT 0501.
       INITIALIZE opcao.
       ACCEPT opcao AT 0531.
       IF opcao = "S" or "s" THEN
           MOVE "v" TO prosseguir
           MOVE "f" TO rodar
       ELSE
           IF opcao = "N" or "n" THEN
               MOVE "v" TO prosseguir
               MOVE "v" TO rodar
               DISPLAY "Entre com uma conta valida!       " AT 0501
           ELSE
               DISPLAY "Opcao invalida"
               MOVE "f" TO prosseguir
               MOVE "v" TO rodar
           END-IF
       END-IF.

       PERGUNTA-EXCLUIR.
       MOVE "f" TO  prosseguir.
       DISPLAY "                                  " AT 0501
       DISPLAY "Tem certeza que quer excluir esta conta (S/N)?" AT 0601.
       DISPLAY SPACES.
       DISPLAY SPACES.
       DISPLAY SPACES.
       PERFORM MOSTRAR-EXCLUSAO.
       INITIALIZE opcao.
       ACCEPT opcao AT 0650.
       IF opcao = "n" or "N" THEN
        DISPLAY "Conta nao sera excluida! " AT 0701
        MOVE "v" TO prosseguir
       ELSE
        IF opcao = "s" OR "S" THEN
        DISPLAY "Conta excluida!          " AT 0701
        DELETE F-CADASTRO RECORD
        MOVE "v" TO prosseguir
       ELSE
        DISPLAY " Opcao invalida          " AT 0701.

       MOSTRAR-EXCLUSAO.
       DISPLAY "======================================================".
       DISPLAY "Codigo da conta:       " ws-cod-conta.
       DISPLAY "Codigo da agencia:     " ws-cod-agencia.
       DISPLAY "Nome do dono da conta: " ws-nome.
       DISPLAY "Saldo da conta:        " ws-saldo.
       DISPLAY "======================================================".




      *******************************************************************
       CONSULTA.
       INITIALIZE opcao.
      *DISPLAY erase AT 0101.
       DISPLAY TELA-CONSULTA.
       PERFORM OPCAOCONSULTA UNTIL opcao = "1" OR "2" OR "s" OR "S".

       OPCAOCONSULTA.
           INITIALIZE opcao.
           ACCEPT opcao AT 1113.
           EVALUATE opcao
               WHEN "1"  PERFORM CONSULTAR-CONTA UNTIL rodar = "f"
               WHEN "2"  PERFORM CONSULTAR-NOME  UNTIL rodar = "f"
               WHEN "s"  DISPLAY "saindo"
               WHEN "S"  DISPLAY "saindo"
               WHEN OTHER DISPLAY "   comando nao existe".

       CONSULTAR-CONTA.
      *    DISPLAY erase AT 0101.
           DISPLAY TELA-CONSULTA-CONTA.
           MOVE "f" TO prosseguir.
           PERFORM CONTA-EXISTE UNTIL prosseguir = "v".
           DISPLAY "ENTER para continuar" AT 1111.
           ACCEPT continua.
           INITIALIZE opcao.
          PERFORM menu-principal.

       CONTA-EXISTE.
           INITIALIZE WS-DADOS-CADASTRO.
           INITIALIZE DADOS-CADASTRO.
           OPEN INPUT F-CADASTRO.
               ACCEPT cod-conta AT 0527.
               READ F-CADASTRO RECORD INTO WS-DADOS-CADASTRO
                   KEY IS cod-conta
                   INVALID KEY
                       DISPLAY "Conta inexistesnte!" AT 0535
                   NOT INVALID KEY
                       PERFORM MOSTRAR-CONSULTA-CONTA
                       MOVE "v" TO prosseguir
                       DISPLAY "                   " AT 0535
               END-READ.
           CLOSE F-CADASTRO.

       MOSTRAR-CONSULTA-CONTA.
       DISPLAY TELA-INFO2.
       DISPLAY ws-cod-conta AT 0720.
       DISPLAY ws-cod-agencia AT 0820.
       DISPLAY ws-nome AT 0920.
       DISPLAY ws-saldo AT 1020.

       CONSULTAR-NOME.
       OPEN INPUT F-CADASTRO.
      * DISPLAY erase AT 0101.
        DISPLAY TELA-CONSULTA-NOME.
        MOVE "f" TO prosseguir.
        PERFORM NOME-EXISTE UNTIL prosseguir = "v".
       CLOSE F-CADASTRO.

       NOME-EXISTE.
       MOVE "v" TO prosseguir.
       INITIALIZE WS-DADOS-CADASTRO.
       INITIALIZE DADOS-CADASTRO.
       INITIALIZE busca-nome.
       ACCEPT nome AT 0535.
       MOVE nome TO busca-nome.
       start F-CADASTRO KEY IS = nome
           INVALID KEY
               DISPLAY "Nome nao possui conta!" AT 0635
               MOVE "f" TO prosseguir
           NOT INVALID KEY
               DISPLAY "                      " AT 0635
               PERFORM LOOP-NOME
               MOVE "v" TO prosseguir
        END-START.

       LOOP-NOME.
       MOVE "f" TO prosseguir.
       MOVE nome TO busca-nome.
       DISPLAY SPACES.
       DISPLAY "Contas pertencentes a " busca-nome.
       DISPLAY SPACES.
       PERFORM UNTIL prosseguir = "v"
           READ F-CADASTRO NEXT RECORD INTO WS-DADOS-CADASTRO
               AT END MOVE "v" TO prosseguir
               NOT AT END
                   IF nome = busca-nome THEN
                     PERFORM MOSTRAR-CONSULTA-NOME
                   ELSE
                       MOVE "v" TO prosseguir
                   END-IF
               END-READ
       END-PERFORM.
       DISPLAY "======================================================".
       DISPLAY "Fim da lista".
       DISPLAY "ENTER para continuar ".
       ACCEPT continua.

       MOSTRAR-CONSULTA-NOME.
       DISPLAY "======================================================".
       DISPLAY "Codigo da conta:       " ws-cod-conta.
       DISPLAY "Codigo da agencia:     " ws-cod-agencia.
       DISPLAY "Nome do dono da conta: " ws-nome.
       DISPLAY "Saldo da conta:        " ws-saldo.

      *******************************************************************
       LISTAR.
       INITIALIZE WS-DADOS-CADASTRO.
       INITIALIZE DADOS-CADASTRO.
      *DISPLAY erase AT 0101.
       DISPLAY TELA-LISTAR.
       DISPLAY SPACES.
       MOVE "f" TO prosseguir.
       OPEN INPUT F-CADASTRO.
        PERFORM UNTIL prosseguir = "v"
         READ F-CADASTRO NEXT RECORD INTO WS-DADOS-CADASTRO
          AT END
           MOVE "v" TO prosseguir
          NOT AT END
           PERFORM MOSTRAR-CADASTROS
         END-READ
        END-PERFORM.
       CLOSE F-CADASTRO.
       DISPLAY "======================================================".
       DISPLAY "Fim da lista".
       DISPLAY "ENTER para continuar ".
       ACCEPT continua.

       MOSTRAR-CADASTROS.
       DISPLAY "======================================================".
       DISPLAY "Codigo da conta:       " ws-cod-conta.
       DISPLAY "Codigo da agencia:     " ws-cod-agencia.
       DISPLAY "Nome do dono da conta: " ws-nome.
       DISPLAY "Saldo da conta:        " ws-saldo.

      *******************************************************************
       SAIR.
           MOVE "v" TO fim.
           DISPLAY " saindo...                      ".
