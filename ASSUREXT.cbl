      *    *************************************************************
      *    PROGRAMME ASSUREXT
      *    Ce programme génère un rapport global des assurances à partir
      *    de deux fichiers de données distincts. Il compte les 
      *    enregistrements actifs et inactifs, affiche les informations
      *    spécifiques de chaque enregistrement et génère un résumé 
      *    à la fin du rapport.
      *    *************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSUREXT.
       AUTHOR. Pierre.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
      *    Définition des fichiers utilisés par le programme.

      *    Fichier d'assurance partie 1.
           SELECT FICHIER-ASSUR-PART1 ASSIGN TO 'assurances-part1.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
               
      *    Fichier d'assurance partie 2.
           SELECT FICHIER-ASSUR-PART2 ASSIGN TO 'assurances-part2.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
               
      *    Fichier rapport d'assurances.
           SELECT FICHIER-RAPPORT ASSIGN TO 'rapport-assurances.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-ASSUR-PART1.
           01  ASSUR-REC.
               05  ASSUR-ID            PIC X(8).
               05  FILLER              PIC X(1).
               05  ASSUR-NOM           PIC X(14).
               05  FILLER              PIC X(1).
               05  ASSUR-DESC          PIC X(14).
               05  FILLER              PIC X(1).
               05  ASSUR-TYPE          PIC X(41).
               05  FILLER              PIC X(1).
               05  ASSUR-STATUT        PIC X(8).
               05  FILLER              PIC X(1).
               05  ASSUR-DATE-DEB      PIC X(8).
               05  FILLER              PIC X(1).
               05  ASSUR-DATE-FIN      PIC X(8).
               05  FILLER              PIC X(1).
               05  ASSUR-MONTANT       PIC X(9).
               05  FILLER              PIC X(1).
               05  ASSUR-DEVISE        PIC X(3).

       FD  FICHIER-ASSUR-PART2.
           01  ASSUR-REC-2.
               05  ASSUR-ID2           PIC X(8).
               05  FILLER              PIC X(1).
               05  ASSUR-NOM2          PIC X(14).
               05  FILLER              PIC X(1).
               05  ASSUR-DESC2         PIC X(14).
               05  FILLER              PIC X(1).
               05  ASSUR-TYPE2         PIC X(41).
               05  FILLER              PIC X(1).
               05  ASSUR-STATUT2       PIC X(8).
               05  FILLER              PIC X(1).
               05  ASSUR-DATE-DEB2     PIC X(8).
               05  FILLER              PIC X(1).
               05  ASSUR-DATE-FIN2     PIC X(8).
               05 FILLER               PIC X(1).
               05 ASSUR-MONTANT2       PIC X(9).
               05 FILLER               PIC X(1).
               05 ASSUR-DEVISE2        PIC X(3).

       FD  FICHIER-RAPPORT.
           01  RAPPORT-REC.
               05  WS-RAPPORT-LENGTH  PIC 9(3).
               05  WS-RAPPORT-DATA    PIC X(147).

             WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS        PIC XX.
       01  WS-CURRENT-TIME       PIC X(20).
       01  WS-REC-COUNT          PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-ACTIF        PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-RESILIE      PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-SUSPENDU     PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-RECORDS      PIC 9(6) VALUE ZERO.
       01  WS-FOOTER-INFO        PIC X(147).
       01  WS-BLANK-LINE         PIC X(147) VALUE SPACES.

       01  ACTIVE-RECORDS.
           05  AR-RECORDS OCCURS 500 TIMES INDEXED BY AR-IDX.
               10  AR-DATA     PIC X(147).
       01  RESILIE-RECORDS.
           05  RS-RECORDS OCCURS 500 TIMES INDEXED BY RS-IDX.
               10  RS-DATA     PIC X(147).
       01  SUSPENDU-RECORDS.
           05  SP-RECORDS OCCURS 500 TIMES INDEXED BY SP-IDX.
               10  SP-DATA     PIC X(147).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT FICHIER-ASSUR-PART1 FICHIER-ASSUR-PART2
                OUTPUT FICHIER-RAPPORT.
                
           MOVE "Rapport Global des Assurances" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.

           MOVE ZERO TO WS-FILE-STATUS
           PERFORM PROCESS-FILE-1
           PERFORM PROCESS-FILE-2

           STRING "Total Records: " WS-TOTAL-RECORDS 
                 DELIMITED BY SIZE
                 " Actifs: " WS-TOTAL-ACTIF 
                 " Résiliés: " WS-TOTAL-RESILIE 
                 " Suspendus: " WS-TOTAL-SUSPENDU 
                 DELIMITED BY SIZE
                 INTO WS-FOOTER-INFO
           MOVE WS-FOOTER-INFO TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA.
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.

      * Section pour les actifs
           MOVE "Enregistrements Actifs:" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           PERFORM WRITE-ACTIVE-RECORDS

      * Section pour les résiliés
           MOVE "Enregistrements Résiliés:" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.
           PERFORM WRITE-RESILIE-RECORDS

      * Section pour les suspendus
           MOVE "Enregistrements Suspendus:" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.
           PERFORM WRITE-SUSPENDU-RECORDS

           CLOSE FICHIER-ASSUR-PART1
           CLOSE FICHIER-ASSUR-PART2
           CLOSE FICHIER-RAPPORT.
           DISPLAY "FIN DE TRAITEMENT DES ENREGISTREMENTS."
           STOP RUN.

       PROCESS-FILE-1.
           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ FICHIER-ASSUR-PART1 INTO ASSUR-REC AT END
                   MOVE '10' TO WS-FILE-STATUS
               NOT AT END
                   EVALUATE ASSUR-STATUT
                       WHEN 'Actif'
                           ADD 1 TO WS-TOTAL-ACTIF
                           MOVE ASSUR-REC TO AR-DATA(WS-TOTAL-ACTIF)
                       WHEN 'Resilie'
                           ADD 1 TO WS-TOTAL-RESILIE
                           MOVE ASSUR-REC TO RS-DATA(WS-TOTAL-RESILIE)
                       WHEN 'Suspendu'
                           ADD 1 TO WS-TOTAL-SUSPENDU
                           MOVE ASSUR-REC TO SP-DATA(WS-TOTAL-SUSPENDU)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
                   ADD 1 TO WS-TOTAL-RECORDS
               END-READ
           END-PERFORM.

       PROCESS-FILE-2.
           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ FICHIER-ASSUR-PART2 INTO ASSUR-REC-2 AT END
                   MOVE '10' TO WS-FILE-STATUS
               NOT AT END
                   EVALUATE ASSUR-STATUT2
                       WHEN 'Actif'
                           ADD 1 TO WS-TOTAL-ACTIF
                           MOVE ASSUR-REC-2 TO AR-DATA(WS-TOTAL-ACTIF)
                       WHEN 'Resilie'
                           ADD 1 TO WS-TOTAL-RESILIE
                           MOVE ASSUR-REC-2 TO RS-DATA(WS-TOTAL-RESILIE)
                       WHEN 'Suspendu'
                           ADD 1 TO WS-TOTAL-SUSPENDU
                          MOVE ASSUR-REC-2 TO SP-DATA(WS-TOTAL-SUSPENDU)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
                   ADD 1 TO WS-TOTAL-RECORDS
               END-READ
           END-PERFORM.


       WRITE-ACTIVE-RECORDS.
           PERFORM VARYING AR-IDX FROM 1 BY 1 
                   UNTIL AR-IDX > WS-TOTAL-ACTIF
               MOVE AR-DATA(AR-IDX OF ACTIVE-RECORDS) TO 
               WS-RAPPORT-DATA
               MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
               WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
               WRITE RAPPORT-REC FROM WS-BLANK-LINE
           END-PERFORM.

       WRITE-RESILIE-RECORDS.
           PERFORM VARYING RS-IDX FROM 1 BY 1 
                   UNTIL RS-IDX > WS-TOTAL-RESILIE
               MOVE RS-DATA(RS-IDX OF RESILIE-RECORDS) TO 
               WS-RAPPORT-DATA
               MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
               WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
               WRITE RAPPORT-REC FROM WS-BLANK-LINE
           END-PERFORM.

       WRITE-SUSPENDU-RECORDS.
           PERFORM VARYING SP-IDX FROM 1 BY 1 
                   UNTIL SP-IDX > WS-TOTAL-SUSPENDU
               MOVE SP-DATA(SP-IDX OF SUSPENDU-RECORDS) TO 
               WS-RAPPORT-DATA
               MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
               WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
               WRITE RAPPORT-REC FROM WS-BLANK-LINE
           END-PERFORM.
