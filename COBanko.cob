	IDENTIFICATION DIVISION.
        PROGRAM-ID.    COBanko.
        AUTHOR.        WESLEY A. M.

        ENVIRONMENT DIVISION.

        DATA DIVISION.

        WORKING-STORAGE SECTION.
        01 OPCAO-MENU   PIC 9(1).

        01 NUMERO-CONTA PIC 9(6).
        01 NOME-CLIENTE PIC X(30).
        01 SALDO           PIC S9(5)V99.

        01 STATUS-CONTA       PIC X(7).
            88 ATIVA     VALUE 'ATIVA'.
            88 INATIVA   VALUE 'INATIVA'.

        PROCEDURE DIVISION.
	   PERFORM MENU-PRINCIPAL UNTIL OPCAO-MENU = 6
	   STOP RUN.
	MENU-PRINCIPAL.
           DISPLAY "1 - CRIAR CONTA."
           DISPLAY "2 - CONSULTAR SALDO."
           DISPLAY "3 - SACAR."
           DISPLAY "4 - DEPOSITAR."
           DISPLAY "5 - ENCERRAR CONTA."
           DISPLAY "6 - SAIR."
           DISPLAY "ESCOLHA UMA OPÇÃO (EX: 1): "
           ACCEPT OPCAO-MENU


           EVALUATE OPCAO-MENU
              WHEN 1
                 DISPLAY "CRIANDO CONTA..."
              WHEN 2
                 DISPLAY "CONSULTANDO SALDO"
	      WHEN 3
                 DISPLAY "SACANDO..."
	      WHEN 4
	         DISPLAY "DEPOSITANDO..."
	      WHEN 5
	         DISPLAY "ENCERRANDO CONTA..."
	      WHEN 6
		 DISPLAY "SAINDO..."
	      WHEN OTHER
	         DISPLAY "OPÇÃO INVALIDA."
           END-EVALUATE.
