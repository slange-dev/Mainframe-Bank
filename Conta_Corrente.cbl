000100******************************************************************
000200 IDENTIFICATION DIVISION.
000300******************************************************************
000400 PROGRAM-ID. CONTACORRENTE.
000500
000600******************************************************************
000700 ENVIRONMENT DIVISION.
000800******************************************************************
000900 CONFIGURATION SECTION.
001000     SPECIAL-NAMES.
001100     DECIMAL-POINT IS COMMA.
001200
001300 INPUT-OUTPUT SECTION.
001400 FILE-CONTROL.
001500     SELECT F-CADASTRO ASSIGN TO disk
001600        ORGANIZATION IS INDEXED
001700        ACCESS IS DYNAMIC
001800        RECORD KEY IS cod-conta
001900        ALTERNATE RECORD KEY nome
002000           WITH DUPLICATES
002100           FILE STATUS ARQ-OK.
002200
002300******************************************************************
002400 DATA DIVISION.
002500******************************************************************
002600 FILE SECTION.
002700 FD F-CADASTRO LABEL RECORD STANDARD
002800     VALUE OF FILE-ID IS "F-DATA.txt".
002900 01 DADOS-CADASTRO.
003000     02 cod-conta      PIC 9(5).
003100     02 cod-agencia    PIC 9(5).
003200     02 nome           PIC AAAAAAAAAA VALUE SPACES.
003300*     02 saldo          PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
003400     02 saldo          PIC $---.---.---.--9,99.
003500
003600 WORKING-STORAGE SECTION.
003700 01 busca-nome         PIC AAAAAAAAAA VALUE SPACES.
003800 01 opcao              PIC X(1) VALUE SPACES.
003900 01 continua           PIC X(1) VALUE SPACES.
004000 01 fim                PIC X.
004100 01 prosseguir         PIC X.
004200 01 rodar              PIC X.
004300 01 WS-DADOS-CADASTRO.
004400     02 ws-cod-conta   PIC 9(5).
004500     02 ws-cod-agencia PIC 9(5).
004600     02 ws-nome        PIC AAAAAAAAAA VALUE SPACES.
004700*     02 ws-saldo       PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
004800     02 ws-saldo       PIC $---.---.---.--9,99.
004900     02 ARQ-OK         PIC X(02) VALUE ZEROES.
005000*     02 QQRMERDA       PIC.
005100
005200 SCREEN SECTION.
005300 01 TELA-INFO.
005400     03 BLANK SCREEN.
005500     03 LINE 13 COL 01 VALUE "Codigo da conta: ".
005600     03 LINE 14 COL 01 VALUE "Codigo da agencia: ".
005700     03 LINE 15 COL 01 VALUE "Dono da conta: ".
005800     03 LINE 16 COL 01 VALUE "Saldo na conta: ".
005900
006000 01 TELA-MENU.
006100     03 BLANK SCREEN BACKGROUND-COLOR 3.
006200     03 LINE 01 COL 01 VALUE "**********************************".
006300     03 LINE 02 COL 01 VALUE "               MENU     " HIGHLIGHT.
006400     03 LINE 03 COL 01 VALUE "**********************************".
006500     03 LINE 04 COL 01 VALUE "ESCOLHA UMA OPCAO ABAIXO:         ".
006600     03 LINE 05 COL 01 VALUE "(1) INCLUIR NOVO CADASTRO         ".
006700     03 LINE 06 COL 01 VALUE "(2) EXCLUIR CADASTRO              ".
006800     03 LINE 07 COL 01 VALUE "(3) ALTERAR CADASTRO              ".
006900     03 LINE 08 COL 01 VALUE "(4) CONSULTAR  CADASTRO           ".
007000     03 LINE 09 COL 01 VALUE "(5) LISTAR TODOS OS CADASTROS     ".
007100     03 LINE 11 COL 01 VALUE "(S) SAIR                          ".
007200     03 LINE 12 COL 01 VALUE "OPCAO:                            ".
007300
007400 01 TELA-INCLUSAO.
007500     03 BLANK SCREEN.
007600     03 LINE 01 COL 01 VALUE "**********************************".
007700     03 LINE 02 COL 01 VALUE "     INCLUSAO DE NOVO CADASTRO".
007800     03 LINE 03 COL 01 VALUE "**********************************".
007900     03 LINE 04 COL 01 VALUE "Conta Corrente: ".
008000     03 LINE 05 COL 01 VALUE "Codigo da agencia: ".
008100     03 LINE 06 COL 01 VALUE "Nome: ".
008200     03 LINE 07 COL 01 VALUE "Saldo:          R$".
008300
008400 01 TELA-CONSULTA.
008500     03 BLANK SCREEN.
008600     03 LINE 01 COL 01 VALUE "**********************************".
008700     03 LINE 02 COL 01 VALUE "              CONSULTA".
008800     03 LINE 03 COL 01 VALUE "**********************************".
008900     03 LINE 05 COL 01 VALUE "Consultar pelo codigo da conta ou".
009000     03 LINE 06 COL 01 VALUE "pelo nome do cliente?".
009100     03 LINE 08 COL 01 VALUE "     (1)    CODIGO DA CONTA".
009200     03 LINE 09 COL 01 VALUE "     (2)    NOME DO CLIENTE".
009300     03 LINE 10 COL 01 VALUE "     (S)    SAIR E VOLTAR AO MENU".
009400     03 LINE 11 COL 01 VALUE "     OPCAO: ".
009500
009600 01 TELA-CONSULTA-CONTA.
009700     03 BLANK SCREEN.
009800     03 LINE 01 COL 01 VALUE "**********************************".
009900     03 LINE 02 COL 01 VALUE "         CONSULTA POR CONTA".
010000     03 LINE 03 COL 01 VALUE "**********************************".
010100     03 LINE 05 COL 01 VALUE "Digite o codigo da conta: ".
010200
010300 01 TELA-CONSULTA-NOME.
010400     03 BLANK SCREEN.
010500     03 LINE 01 COL 01 VALUE "**********************************".
010600     03 LINE 02 COL 01 VALUE "         CONSULTA POR NOME".
010700     03 LINE 03 COL 01 VALUE "**********************************".
010800     03 LINE 05 COL 01 VALUE "Digite um nome de dono da conta: ".
010900
011000 01 TELA-INFO2.
011100     03 BLANK SCREEN.
011200     03 LINE 07 COL 01 VALUE "Codigo da conta: ".
011300     03 LINE 08 COL 01 VALUE "Codigo da agencia: ".
011400     03 LINE 09 COL 01 VALUE "Dono da conta: ".
011500     03 LINE 10 COL 01 VALUE "Saldo na conta: ".
011600
011700 01 TELA-EXCLUSAO.
011800     03 BLANK SCREEN.
011900     03 LINE 01 COL 01 VALUE "**********************************".
012000     03 LINE 02 COL 01 VALUE "         TELA DE EXCLUSAO".
012100     03 LINE 03 COL 01 VALUE "**********************************".
012200     03 LINE 04 COL 01 VALUE "Digite o codigo da conta:".
012300
012400 01 TELA-LISTAR.
012500     03 BLANK SCREEN.
012600     03 LINE 01 COL 01 VALUE "***********************************".
012700     03 LINE 02 COL 01 VALUE "         LISTA DE CADASTROS".
012800     03 LINE 03 COL 01 VALUE "***********************************".
012900
013000 01 TELA-ALTERACAO.
013100     03 BLANK SCREEN.
013200     03 LINE 01 COL 01 VALUE "***********************************".
013300     03 LINE 02 COL 01 VALUE "         TELA DE ALTERACAO".
013400     03 LINE 03 COL 01 VALUE "***********************************".
013500     03 LINE 04 COL 01 VALUE "     DIGITE O CODIGO DA CONTA      ".
013600     03 LINE 05 COL 01 VALUE "     A SER ALTERADA:".
013700
013800 01 TELA-INFO-ALTERACAO.
013900     03 BLANK SCREEN.
014000     03 LINE 07 COL 01 VALUE
014100       "======================================================".
014200     03 LINE 08 COL 01 VALUE "  Codigo da conta:".
014300     03 LINE 09 COL 01 VALUE
014400       "------------------------------------------------------".
014500     03 LINE 10 COL 01 VALUE "Codigo da agencia:".
014600     03 LINE 11 COL 01 VALUE "       Novo valor:".
014700     03 LINE 12 COL 01 VALUE
014800       "------------------------------------------------------".
014900     03 LINE 13 COL 01 VALUE "    Dono da conta:".
015000     03 LINE 14 COL 01 VALUE "       Novo valor:".
015100     03 LINE 15 COL 01 VALUE
015200       "------------------------------------------------------".
015300     03 LINE 16 COL 01 VALUE "   Saldo na conta:".
015400     03 LINE 17 COL 01 VALUE "       Novo valor:".
015500     03 LINE 18 COL 01 VALUE
015600       "======================================================".
015700
015800******************************************************************
015900 PROCEDURE DIVISION.
016000******************************************************************
016100 PRINCIPAL.
016200 MOVE "f" TO fim.
016300 PERFORM MENU-PRINCIPAL UNTIL fim = "v".
016400 STOP RUN.
016500
016600****************************** MENU PRINCIPAL ********************
016700 MENU-PRINCIPAL.
016800 INITIALIZE opcao.
016900*DISPLAY erase AT 0101.
017000 DISPLAY TELA-MENU.
017100 PERFORM OPCAO-MENU.
017200
017300 OPCAO-MENU.
017400 INITIALIZE rodar.
017500 INITIALIZE opcao.
017600 ACCEPT opcao AT 1208.
017700 EVALUATE opcao
017800     WHEN "1" PERFORM INCLUSAO
017900     WHEN "2"  PERFORM EXCLUSAO UNTIL rodar = "v"
018000     WHEN "3"  PERFORM ALTERACAO UNTIL rodar = "v"
018100     WHEN "4"  PERFORM CONSULTA
018200     WHEN "5"  PERFORM LISTAR
018300     WHEN "S"  PERFORM sair
018400     WHEN "s"  PERFORM sair
018500     WHEN OTHER DISPLAY " Op��o Inv�lida"
018600 END-EVALUATE.
018700
018800******************************* INCLUSAO *************************
018900 INCLUSAO.
019000 INITIALIZE opcao.
019100 INITIALIZE dados-cadastro.
019200 INITIALIZE ws-dados-cadastro.
019300 MOVE "f" TO prosseguir.
019400*DISPLAY erase AT 0101.
019500 DISPLAY TELA-INCLUSAO.
019600 OPEN OUTPUT F-CADASTRO.
019700 IF ARQ-OK NOT = "00" THEN
019800     DISPLAY "Erro de Arquivo. Erro:", ARQ-OK AT 3001
019900     CLOSE F-CADASTRO
020000 ELSE
020100     DISPLAY "ARQUIVO OK", ARQ-OK AT 3001
020200 END-IF
020300
020400 OPEN EXTEND F-CADASTRO.
020500 PERFORM ENTRADA-CODIGO  UNTIL prosseguir = "v".
020600 PERFORM ENTRADA-AGENCIA UNTIL ws-cod-agencia NOT EQUAL ZEROES.
020700 PERFORM ENTRADA-NOME    UNTIL ws-nome NOT EQUAL SPACES.
020800 PERFORM ENTRADA-SALDO.
020900 MOVE "f" TO prosseguir.
021000 PERFORM PERGUNTA-SALVAR UNTIL prosseguir = "v".
021100 CLOSE F-CADASTRO.
021200
021300 ENTRADA-CODIGO.
021400 MOVE "v" TO prosseguir.
021500 INITIALIZE cod-conta.
021600 ACCEPT cod-conta AT 0420.
021700 IF cod-conta = ZEROES
021800 THEN
021900     DISPLAY "Cadastro deve ser diferente de zero!" AT 0430
022000     MOVE "f" TO prosseguir
022100 ELSE
022200     DISPLAY "                                    " AT 0430
022300     READ F-CADASTRO
022400     NOT INVALID KEY
022500     DISPLAY "Ja cadastrado" AT 0430
022600     MOVE cod-conta TO ws-cod-conta
022700
022800     DISPLAY "COD-CONTA    ", cod-conta
022900     DISPLAY "WS-COD-CONTA ", ws-cod-conta
023000
023100     MOVE "f" TO prosseguir
023200     END-READ
023300 END-IF.
023400
023500 ENTRADA-AGENCIA.
023600 INITIALIZE ws-cod-agencia.
023700 ACCEPT ws-cod-agencia AT 0520.
023800 IF ws-cod-agencia = ZEROES
023900 THEN
024000     DISPLAY "Agencia deve ser diferente de zero!" AT 0530
024100 ELSE
024200     DISPLAY "                                   " AT 0530
024300     MOVE ws-cod-agencia TO cod-agencia
024400 END-IF.
024500
024600 ENTRADA-NOME.
024700 INITIALIZE ws-nome.
024800 ACCEPT ws-nome AT 0620.
024900 IF ws-nome = SPACES
025000 THEN
025100     DISPLAY "Nome esta em branco!" AT 0730
025200 ELSE
025300     DISPLAY "                    " AT 0730
025400     MOVE ws-nome TO nome
025500 END-IF.
025600
025700 ENTRADA-SALDO.
025800 INITIALIZE ws-saldo.
025900 ACCEPT ws-saldo AT 0720.
026000 MOVE ws-saldo TO saldo.
026100
026200 PERGUNTA-SALVAR.
026300 INITIALIZE opcao.
026400 DISPLAY "Salvar os dados? (S\N):  " AT 0910.
026500 ACCEPT opcao AT 0935.
026600 IF opcao = "s" or "S"
026700 THEN
026800     PERFORM ESCREVER-DADOS
026900*     DISPLAY "Cadastrado com Sucesso"
027000     MOVE "v" TO prosseguir
027100 ELSE
027200     IF opcao equals "n" or "N"
027300     THEN
027400         DISPLAY " Dados nao foram salvos" AT 0937
027500         MOVE "v" TO prosseguir
027600     ELSE
027700         DISPLAY " Opcao invalida!       " AT 0937
027800         MOVE "f" TO prosseguir
027900     END-IF
028000 END-IF.
028100
028200 ESCREVER-DADOS.
028300 WRITE DADOS-CADASTRO
028400 INVALID KEY
028500     DISPLAY "!!??" AT 1215
028600         NOT INVALID KEY
028700         DISPLAY "Registrado com sucesso! " AT 1210
028800*        PERFORM MOSTRAR-DADOS-SALVOS
028900         ACCEPT continua
029000 END-WRITE.
029100
029200 MOSTRAR-DADOS.
029300 READ F-CADASTRO RECORD INTO WS-DADOS-CADASTRO
029400     KEY IS cod-conta
029500 END-READ.
029600 DISPLAY TELA-INFO.
029700 DISPLAY ws-cod-conta AT 1320.
029800 DISPLAY ws-cod-agencia AT 1420.
029900 DISPLAY ws-nome AT 1520.
030000 DISPLAY ws-saldo AT 1620.
030100
030200 MOSTRAR-DADOS-SALVOS.
030300 READ F-CADASTRO RECORD INTO WS-DADOS-CADASTRO
030400     KEY IS cod-conta
030500 END-READ.
030600 DISPLAY SPACES.
030700 DISPLAY SPACES.
030800 DISPLAY "======================================================".
030900 DISPLAY "Codigo da conta:       " ws-cod-conta.
031000 DISPLAY "Codigo da agencia:     " ws-cod-agencia.
031100 DISPLAY "Nome do dono da conta: " ws-nome.
031200 DISPLAY "Saldo da conta:        " ws-saldo.
031300 DISPLAY "======================================================".
031400 INITIALIZE WS-DADOS-CADASTRO.
031500 DISPLAY SPACES.
031600 DISPLAY "Registrado com sucesso! ".
031700 DISPLAY "Enter para continuar.".
031800******************************************************************
031900
032000 RODAR-ALTERACAO.
032100 MOVE "f" TO rodar.
032200 PERFORM ALTERACAO.
032300
032400 ALTERACAO.
032500 MOVE "f" TO rodar.
032600 INITIALIZE opcao.
032700 MOVE "f" TO prosseguir.
032800*DISPLAY erase AT 0101.
032900 DISPLAY TELA-ALTERACAO.
033000 OPEN I-O F-CADASTRO.
033100     PERFORM UNTIL prosseguir = "v"
033200         INITIALIZE cod-conta
033300         INITIALIZE WS-DADOS-CADASTRO
033400         ACCEPT cod-conta AT 0522
033500         READ F-CADASTRO INTO WS-DADOS-CADASTRO
033600           KEY IS cod-conta
033700           INVALID KEY
033800            DISPLAY "Codigo invalido!" AT 0530
033900            PERFORM PERGUNTA-SAIR-ALTERACAO UNTIL prosseguir = "v"
034000            IF rodar = "f" THEN
034100                 MOVE "v" TO prosseguir
034200            ELSE
034300                 MOVE "f" TO prosseguir
034400           NOT INVALID KEY
034500             DISPLAY "                " AT 0530
034600             PERFORM ALTERAR-CONTA UNTIL prosseguir = "v"
034700             MOVE "v" TO prosseguir
034800         END-READ
034900     END-PERFORM.
035000 CLOSE F-CADASTRO.
035100 MOVE "v" TO rodar.
035200
035300
035400 PERGUNTA-SAIR-ALTERACAO.
035500 MOVE "f" TO  prosseguir.
035600 DISPLAY         "Deseja voltar ao menu? (S/N):" AT 0601.
035700 INITIALIZE opcao.
035800 ACCEPT opcao AT 0631.
035900 IF opcao = "S" or "s" THEN
036000     MOVE "v" TO prosseguir
036100     MOVE "f" TO rodar
036200 ELSE
036300     IF opcao = "N" or "n" THEN
036400         MOVE "v" TO prosseguir
036500         MOVE "v" TO rodar
036600         DISPLAY "Entre com uma conta valida!       " AT 0601
036700     ELSE
036800         DISPLAY "Opcao invalida"
036900         MOVE "f" TO prosseguir
037000         MOVE "v" TO rodar
037100     END-IF
037200 END-IF.
037300
037400 ALTERAR-CONTA.
037500 DISPLAY SPACES.
037600 DISPLAY TELA-INFO-ALTERACAO.
037700 DISPLAY ws-cod-conta AT 0820.
037800 DISPLAY ws-cod-agencia AT 1020.
037900 MOVE "f" TO prosseguir.
038000 PERFORM UNTIL prosseguir = "v"
038100*    MOVE "f" TO prosseguir
038200     ACCEPT ws-cod-agencia AT 1120
038300     IF ws-cod-agencia equals ZEROES THEN
038400         DISPLAY "Codigo deve ser diferente de zero!" AT 1127
038500         MOVE "f" TO prosseguir
038600     ELSE
038700         DISPLAY "                                  " AT 1127
038800         MOVE "v" TO prosseguir
038900     END-IF
039000 END-PERFORM.
039100 DISPLAY ws-nome AT 1320.
039200 MOVE "f" TO prosseguir.
039300 PERFORM UNTIL prosseguir = "v"
039400     MOVE "f" TO prosseguir
039500     ACCEPT ws-nome AT 1420
039600     IF ws-nome equals SPACES THEN
039700         DISPLAY "O nome esta em branco!" AT 1435
039800         MOVE "f" TO prosseguir
039900     ELSE
040000         DISPLAY "                      " AT 1435
040100         MOVE "v" TO prosseguir
040200     END-IF
040300 END-PERFORM.
040400 DISPLAY ws-saldo AT 1620.
040500 ACCEPT ws-saldo AT 1720.
040600 MOVE "f" TO prosseguir.
040700 PERFORM PERTGUNTA-ALTERAR UNTIL prosseguir = "v".
040800 DISPLAY SPACES.
040900 DISPLAY "ENTER para continuar."
041000 ACCEPT continua.
041100 MOVE "v" TO prosseguir.
041200
041300 PERTGUNTA-ALTERAR.
041400 MOVE "f" TO prosseguir.
041500 DISPLAY "Tem certeza que quer alterar esta conta (S/N)?" AT 1901.
041600 INITIALIZE opcao.
041700 ACCEPT opcao AT 1947.
041800 IF opcao = "s" or "S" THEN
041900     MOVE WS-DADOS-CADASTRO TO DADOS-CADASTRO
042000     REWRITE DADOS-CADASTRO
042100     END-REWRITE
042200     DISPLAY "Os dados foram salvos!    " AT 2001
042300     MOVE "v" TO prosseguir
042400 ELSE
042500     IF opcao = "n" OR "N" THEN
042600         DISPLAY "Os dados nao foram salvos." AT 2001
042700         MOVE "v" TO prosseguir
042800     ELSE
042900         DISPLAY "Comando invalido!!!       " AT 2001
043000         MOVE "f" TO prosseguir
043100     END-IF
043200 END-IF.
043300
043400******************************************************************
043500
043600
043700 EXCLUSAO.
043800 MOVE "f" TO rodar.
043900 INITIALIZE opcao.
044000 MOVE "f" TO prosseguir.
044100*DISPLAY erase AT 0101.
044200 DISPLAY TELA-EXCLUSAO.
044300 OPEN I-O F-CADASTRO.
044400     PERFORM UNTIL prosseguir = "v"
044500         INITIALIZE cod-conta
044600         INITIALIZE WS-DADOS-CADASTRO
044700         ACCEPT cod-conta AT 0427
044800         READ F-CADASTRO INTO WS-DADOS-CADASTRO
044900           KEY IS cod-conta
045000           INVALID KEY
045100             DISPLAY "Conta inexistente!" AT 0435
045200             PERFORM PERGUNTA-SAIR-EXCLUSAO UNTIL prosseguir = "v"
045300             IF rodar = "f" THEN
045400                 MOVE "v" TO prosseguir
045500             ELSE
045600                 MOVE "f" TO prosseguir
045700           NOT INVALID KEY
045800             DISPLAY "                  " AT 0435
045900             PERFORM PERGUNTA-EXCLUIR UNTIL prosseguir = "v"
046000             MOVE "v" TO prosseguir
046100         END-READ
046200      END-PERFORM.
046300 CLOSE F-CADASTRO.
046400 MOVE "v" TO rodar.
046500 DISPLAY " ENTER para sair.".
046600 ACCEPT continua.
046700
046800 PERGUNTA-SAIR-EXCLUSAO.
046900 MOVE "f" TO  prosseguir.
047000 DISPLAY         "Deseja voltar ao menu? (S/N):" AT 0501.
047100 INITIALIZE opcao.
047200 ACCEPT opcao AT 0531.
047300 IF opcao = "S" or "s" THEN
047400     MOVE "v" TO prosseguir
047500     MOVE "f" TO rodar
047600 ELSE
047700     IF opcao = "N" or "n" THEN
047800         MOVE "v" TO prosseguir
047900         MOVE "v" TO rodar
048000         DISPLAY "Entre com uma conta valida!       " AT 0501
048100     ELSE
048200         DISPLAY "Opcao invalida"
048300         MOVE "f" TO prosseguir
048400         MOVE "v" TO rodar
048500     END-IF
048600 END-IF.
048700
048800 PERGUNTA-EXCLUIR.
048900 MOVE "f" TO  prosseguir.
049000 DISPLAY "                                  " AT 0501
049100 DISPLAY "Tem certeza que quer excluir esta conta (S/N)?" AT 0601.
049200 DISPLAY SPACES.
049300 DISPLAY SPACES.
049400 DISPLAY SPACES.
049500 PERFORM MOSTRAR-EXCLUSAO.
049600 INITIALIZE opcao.
049700 ACCEPT opcao AT 0650.
049800 IF opcao = "n" or "N" THEN
049900  DISPLAY "Conta nao sera excluida! " AT 0701
050000  MOVE "v" TO prosseguir
050100 ELSE
050200  IF opcao = "s" OR "S" THEN
050300  DISPLAY "Conta excluida!          " AT 0701
050400  DELETE F-CADASTRO RECORD
050500  MOVE "v" TO prosseguir
050600 ELSE
050700  DISPLAY " Opcao invalida          " AT 0701.
050800
050900 MOSTRAR-EXCLUSAO.
051000 DISPLAY "======================================================".
051100 DISPLAY "Codigo da conta:       " ws-cod-conta.
051200 DISPLAY "Codigo da agencia:     " ws-cod-agencia.
051300 DISPLAY "Nome do dono da conta: " ws-nome.
051400 DISPLAY "Saldo da conta:        " ws-saldo.
051500 DISPLAY "======================================================".
051600
051700
051800
051900
052000******************************************************************
052100 CONSULTA.
052200 INITIALIZE opcao.
052300*DISPLAY erase AT 0101.
052400 DISPLAY TELA-CONSULTA.
052500 PERFORM OPCAOCONSULTA UNTIL opcao = "1" OR "2" OR "s" OR "S".
052600
052700 OPCAOCONSULTA.
052800     INITIALIZE opcao.
052900     ACCEPT opcao AT 1113.
053000     EVALUATE opcao
053100         WHEN "1"  PERFORM CONSULTAR-CONTA UNTIL rodar = "f"
053200         WHEN "2"  PERFORM CONSULTAR-NOME  UNTIL rodar = "f"
053300         WHEN "s"  DISPLAY "saindo"
053400         WHEN "S"  DISPLAY "saindo"
053500         WHEN OTHER DISPLAY "   comando nao existe".
053600
053700 CONSULTAR-CONTA.
053800*    DISPLAY erase AT 0101.
053900     DISPLAY TELA-CONSULTA-CONTA.
054000     MOVE "f" TO prosseguir.
054100     PERFORM CONTA-EXISTE UNTIL prosseguir = "v".
054200     DISPLAY "ENTER para continuar" AT 1111.
054300     ACCEPT continua.
054400     INITIALIZE opcao.
054500    PERFORM menu-principal.
054600
054700 CONTA-EXISTE.
054800     INITIALIZE WS-DADOS-CADASTRO.
054900     INITIALIZE DADOS-CADASTRO.
055000     OPEN INPUT F-CADASTRO.
055100         ACCEPT cod-conta AT 0527.
055200         READ F-CADASTRO RECORD INTO WS-DADOS-CADASTRO
055300             KEY IS cod-conta
055400             INVALID KEY
055500                 DISPLAY "Conta inexistesnte!" AT 0535
055600             NOT INVALID KEY
055700                 PERFORM MOSTRAR-CONSULTA-CONTA
055800                 MOVE "v" TO prosseguir
055900                 DISPLAY "                   " AT 0535
056000         END-READ.
056100     CLOSE F-CADASTRO.
056200
056300 MOSTRAR-CONSULTA-CONTA.
056400 DISPLAY TELA-INFO2.
056500 DISPLAY ws-cod-conta AT 0720.
056600 DISPLAY ws-cod-agencia AT 0820.
056700 DISPLAY ws-nome AT 0920.
056800 DISPLAY ws-saldo AT 1020.
056900
057000 CONSULTAR-NOME.
057100 OPEN INPUT F-CADASTRO.
057200* DISPLAY erase AT 0101.
057300  DISPLAY TELA-CONSULTA-NOME.
057400  MOVE "f" TO prosseguir.
057500  PERFORM NOME-EXISTE UNTIL prosseguir = "v".
057600 CLOSE F-CADASTRO.
057700
057800 NOME-EXISTE.
057900 MOVE "v" TO prosseguir.
058000 INITIALIZE WS-DADOS-CADASTRO.
058100 INITIALIZE DADOS-CADASTRO.
058200 INITIALIZE busca-nome.
058300 ACCEPT nome AT 0535.
058400 MOVE nome TO busca-nome.
058500 start F-CADASTRO KEY IS = nome
058600     INVALID KEY
058700         DISPLAY "Nome nao possui conta!" AT 0635
058800         MOVE "f" TO prosseguir
058900     NOT INVALID KEY
059000         DISPLAY "                      " AT 0635
059100         PERFORM LOOP-NOME
059200         MOVE "v" TO prosseguir
059300  END-START.
059400
059500 LOOP-NOME.
059600 MOVE "f" TO prosseguir.
059700 MOVE nome TO busca-nome.
059800 DISPLAY SPACES.
059900 DISPLAY "Contas pertencentes a " busca-nome.
060000 DISPLAY SPACES.
060100 PERFORM UNTIL prosseguir = "v"
060200     READ F-CADASTRO NEXT RECORD INTO WS-DADOS-CADASTRO
060300         AT END MOVE "v" TO prosseguir
060400         NOT AT END
060500             IF nome = busca-nome THEN
060600               PERFORM MOSTRAR-CONSULTA-NOME
060700             ELSE
060800                 MOVE "v" TO prosseguir
060900             END-IF
061000         END-READ
061100 END-PERFORM.
061200 DISPLAY "======================================================".
061300 DISPLAY "Fim da lista".
061400 DISPLAY "ENTER para continuar ".
061500 ACCEPT continua.
061600
061700 MOSTRAR-CONSULTA-NOME.
061800 DISPLAY "======================================================".
061900 DISPLAY "Codigo da conta:       " ws-cod-conta.
062000 DISPLAY "Codigo da agencia:     " ws-cod-agencia.
062100 DISPLAY "Nome do dono da conta: " ws-nome.
062200 DISPLAY "Saldo da conta:        " ws-saldo.
062300
062400******************************************************************
062500 LISTAR.
062600 INITIALIZE WS-DADOS-CADASTRO.
062700 INITIALIZE DADOS-CADASTRO.
062800*DISPLAY erase AT 0101.
062900 DISPLAY TELA-LISTAR.
063000 DISPLAY SPACES.
063100 MOVE "f" TO prosseguir.
063200 OPEN INPUT F-CADASTRO.
063300  PERFORM UNTIL prosseguir = "v"
063400   READ F-CADASTRO NEXT RECORD INTO WS-DADOS-CADASTRO
063500    AT END
063600     MOVE "v" TO prosseguir
063700    NOT AT END
063800     PERFORM MOSTRAR-CADASTROS
063900   END-READ
064000  END-PERFORM.
064100 CLOSE F-CADASTRO.
064200 DISPLAY "======================================================".
064300 DISPLAY "Fim da lista".
064400 DISPLAY "ENTER para continuar ".
064500 ACCEPT continua.
064600
064700 MOSTRAR-CADASTROS.
064800 DISPLAY "======================================================".
064900 DISPLAY "Codigo da conta:       " ws-cod-conta.
065000 DISPLAY "Codigo da agencia:     " ws-cod-agencia.
065100 DISPLAY "Nome do dono da conta: " ws-nome.
065200 DISPLAY "Saldo da conta:        " ws-saldo.
065300
065400******************************************************************
065500 SAIR.
065600     MOVE "v" TO fim.
065700     DISPLAY " saindo...                      ".
065800
