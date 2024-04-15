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

      *    Section ENVIRONMENT DIVISION: définition des fichiers 
      *    utilisés par le programme.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

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

      *    Section DATA DIVISION: définition des structures de données.
       DATA DIVISION.
       FILE SECTION.

      *    Définition des enregistrements pour chaque fichier.

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

      *    Variables de contrôle et de calcul.
       01  WS-FILE-STATUS        PIC XX.
       01  WS-CURRENT-TIME       PIC X(20).
       01  WS-REC-COUNT          PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-ACTIF        PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-RESILIE      PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-SUSPENDU     PIC 9(4) VALUE ZERO.
       01  WS-TOTAL-RECORDS      PIC 9(6) VALUE ZERO.
       01  WS-FOOTER-INFO        PIC X(147).
       01  WS-BLANK-LINE         PIC X(147) VALUE SPACES.

      *    Variables pour les montants.
       01  WS-TOTAL-MONTANT         PIC 9(10)V99 VALUE ZERO.
       01  WS-TOTAL-MONTANT-ACTIF   PIC 9(10)V99 VALUE ZERO.
       01  WS-TOTAL-MONTANT-RESILIE PIC 9(10)V99 VALUE ZERO.
       01  WS-TOTAL-MONTANT-SUSPENDU PIC 9(10)V99 VALUE ZERO.

      *    Variables pour afficher les montants avec format.
       01  WS-FORMATTED-TOTAL-MONTANT          PIC ZZ,ZZZ,ZZZ,Z9.
       01  WS-FORMATTED-TOTAL-MONTANT-ACTIF    PIC ZZ,ZZZ,ZZZ,Z9.
       01  WS-FORMATTED-TOTAL-MONTANT-RESILIE  PIC ZZ,ZZZ,ZZZ,Z9.
       01  WS-FORMATTED-TOTAL-MONTANT-SUSPENDU PIC ZZ,ZZZ,ZZZ,Z9.

      *    Tableaux pour stocker les enregistrements.
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

      *    Ouverture des fichiers en entrée et en sortie.
           OPEN INPUT FICHIER-ASSUR-PART1 FICHIER-ASSUR-PART2
               OUTPUT FICHIER-RAPPORT.

      *    Écriture du titre du rapport.
           MOVE "Rapport Global des Assurances" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.

      *    Traitement du premier fichier d'assurance.
           MOVE ZERO TO WS-FILE-STATUS
           PERFORM PROCESS-FILE-1

      *    Traitement du deuxième fichier d'assurance.
           PERFORM PROCESS-FILE-2

      *    Écriture du résumé des assurances.
           STRING "Total Records: " WS-TOTAL-RECORDS 
                   DELIMITED BY SIZE
                   " Actifs: " WS-TOTAL-ACTIF 
                   " Résiliés: " WS-TOTAL-RESILIE 
                   " Suspendus: " WS-TOTAL-SUSPENDU 
                   DELIMITED BY SIZE
                   INTO WS-FOOTER-INFO
           MOVE WS-FOOTER-INFO TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.

      *    Écriture du résumé des montants.
           MOVE WS-TOTAL-MONTANT TO WS-FORMATTED-TOTAL-MONTANT
           MOVE WS-TOTAL-MONTANT-ACTIF TO 
                   WS-FORMATTED-TOTAL-MONTANT-ACTIF
           MOVE WS-TOTAL-MONTANT-RESILIE TO 
                   WS-FORMATTED-TOTAL-MONTANT-RESILIE
           MOVE WS-TOTAL-MONTANT-SUSPENDU TO 
                   WS-FORMATTED-TOTAL-MONTANT-SUSPENDU

           STRING "Total Montant: " WS-FORMATTED-TOTAL-MONTANT "€ "
               "Actifs: " WS-FORMATTED-TOTAL-MONTANT-ACTIF "€ "
               "Résiliés: " WS-FORMATTED-TOTAL-MONTANT-RESILIE "€ "
               "Suspendus: " WS-FORMATTED-TOTAL-MONTANT-SUSPENDU "€ "
               DELIMITED BY SIZE
               INTO WS-FOOTER-INFO
           MOVE WS-FOOTER-INFO TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA.
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.

      *    Section pour les enregistrements actifs.
           MOVE "Enregistrements Actifs:" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           PERFORM WRITE-ACTIVE-RECORDS

      *    Section pour les enregistrements résiliés.
           MOVE "Enregistrements Résiliés:" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.
           PERFORM WRITE-RESILIE-RECORDS

      *    Section pour les enregistrements suspendus.
           MOVE "Enregistrements Suspendus:" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.
           PERFORM WRITE-SUSPENDU-RECORDS

      *    Fermeture des fichiers.
           CLOSE FICHIER-ASSUR-PART1
           CLOSE FICHIER-ASSUR-PART2
           CLOSE FICHIER-RAPPORT.
           DISPLAY "FIN DE TRAITEMENT DES ENREGISTREMENTS."
           STOP RUN.

      *    Traitement du premier fichier d'assurance.
       PROCESS-FILE-1.
      *    Lecture du fichier d'assurance partie 1 jusqu'à la fin du 
      *    fichier.
      *    Met à jour le statut de fichier lorsqu'il atteint la fin.
           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ FICHIER-ASSUR-PART1 INTO ASSUR-REC
                   AT END
                       MOVE '10' TO WS-FILE-STATUS  
                   NOT AT END

      *    Évalue le statut de l'assurance.
                       EVALUATE ASSUR-STATUT  
                           WHEN 'Actif'

      *                 Incrémente le compteur d'assurances actives.
                               ADD 1 TO WS-TOTAL-ACTIF  
                        
      *                 Stocke les données de l'assurance active.
                               MOVE ASSUR-REC TO AR-DATA(WS-TOTAL-ACTIF) 

      *              Calcule le total du montant des assurances actives.
                               COMPUTE WS-TOTAL-MONTANT-ACTIF =  
                                       WS-TOTAL-MONTANT-ACTIF + 
                                       FUNCTION NUMVAL-C(ASSUR-MONTANT)

                           WHEN 'Resilie'

      *                 Incrémente le compteur d'assurances résiliées.
                               ADD 1 TO WS-TOTAL-RESILIE

      *                 Stocke les données de l'assurance résiliée.
                               MOVE ASSUR-REC TO 
                                    RS-DATA(WS-TOTAL-RESILIE)  

      *            Calcule le total du montant des assurances résiliées.
                               COMPUTE WS-TOTAL-MONTANT-RESILIE =  
                                       WS-TOTAL-MONTANT-RESILIE + 
                                       FUNCTION NUMVAL-C(ASSUR-MONTANT)

                           WHEN 'Suspendu'  
                        
      *                 Incrémente le compteur d'assurances suspendues.
                               ADD 1 TO WS-TOTAL-SUSPENDU

      *                 Stocke les données de l'assurance suspendue.
                               MOVE ASSUR-REC TO 
                                    SP-DATA(WS-TOTAL-SUSPENDU)

      *           Calcule le total du montant des assurances suspendues.
                               COMPUTE WS-TOTAL-MONTANT-SUSPENDU =  
                                       WS-TOTAL-MONTANT-SUSPENDU + 
                                       FUNCTION NUMVAL-C(ASSUR-MONTANT)
                                
                           WHEN OTHER 
                                CONTINUE

                       END-EVALUATE

      *         Incrémente le compteur total d'enregistrements.
                       ADD 1 TO WS-TOTAL-RECORDS
                       COMPUTE WS-TOTAL-MONTANT = WS-TOTAL-MONTANT + 
                           FUNCTION NUMVAL-C(ASSUR-MONTANT) 

               END-READ

           END-PERFORM.

      *    Traitement du deuxième fichier d'assurance.
           PROCESS-FILE-2.
           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ FICHIER-ASSUR-PART2 INTO ASSUR-REC-2

                   AT END
                       MOVE '10' TO WS-FILE-STATUS
                   
                   NOT AT END
                       
                       EVALUATE ASSUR-STATUT2
                           
                           WHEN 'Actif'
                               ADD 1 TO WS-TOTAL-ACTIF
                               MOVE ASSUR-REC-2 TO 
                                    AR-DATA(WS-TOTAL-ACTIF)
                               COMPUTE WS-TOTAL-MONTANT-ACTIF = 
                                       WS-TOTAL-MONTANT-ACTIF + 
                                       FUNCTION NUMVAL(ASSUR-MONTANT2)
                           
                           WHEN 'Résilié'
                               ADD 1 TO WS-TOTAL-RESILIE
                               MOVE ASSUR-REC-2 TO 
                                    RS-DATA(WS-TOTAL-RESILIE)
                               COMPUTE WS-TOTAL-MONTANT-RESILIE =  
                                       WS-TOTAL-MONTANT-RESILIE + 
                                       FUNCTION NUMVAL(ASSUR-MONTANT2)
                           
                           WHEN 'Suspendu'
                               ADD 1 TO WS-TOTAL-SUSPENDU
                               MOVE ASSUR-REC-2 TO 
                                    SP-DATA(WS-TOTAL-SUSPENDU)
                               COMPUTE WS-TOTAL-MONTANT-SUSPENDU = 
                                       WS-TOTAL-MONTANT-SUSPENDU + 
                                       FUNCTION NUMVAL(ASSUR-MONTANT2)

                           WHEN OTHER 
                               CONTINUE
                       
                       END-EVALUATE
                       
                       ADD 1 TO WS-TOTAL-RECORDS
               
               END-READ
           
           END-PERFORM.

      *    Écriture des enregistrements actifs dans le rapport.

       WRITE-ACTIVE-RECORDS.
      
      *    Parcourt les enregistrements actifs et les écrit dans le rapport.
           PERFORM VARYING AR-IDX FROM 1 BY 1 UNTIL 
                           AR-IDX > WS-TOTAL-ACTIF
      
      *        Copie les données de l'enregistrement actif dans la 
      *        variable du rapport.
               MOVE AR-DATA(AR-IDX) TO WS-RAPPORT-DATA
      
      *        Détermine la longueur des données du rapport.
               MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
      
      *        Écrit les données de l'enregistrement dans le rapport.
               WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           
           END-PERFORM.


      *    Écriture des enregistrements résiliés dans le rapport.
       WRITE-RESILIE-RECORDS.

           PERFORM VARYING RS-IDX FROM 1 BY 1 UNTIL 
                           RS-IDX > WS-TOTAL-RESILIE

               MOVE RS-DATA(RS-IDX) TO WS-RAPPORT-DATA
               MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
               WRITE RAPPORT-REC FROM WS-RAPPORT-DATA

           END-PERFORM.

      *    Écriture des enregistrements suspendus dans le rapport.
       WRITE-SUSPENDU-RECORDS.

           PERFORM VARYING SP-IDX FROM 1 BY 1 UNTIL 
                           SP-IDX > WS-TOTAL-SUSPENDU

               MOVE SP-DATA(SP-IDX) TO WS-RAPPORT-DATA
               MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
               WRITE RAPPORT-REC FROM WS-RAPPORT-DATA

           END-PERFORM.
