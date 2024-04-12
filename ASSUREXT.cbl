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
               05  FILLER              PIC X(1).
               05  ASSUR-MONTANT2      PIC X(9).
               05  FILLER              PIC X(1).
               05  ASSUR-DEVISE2       PIC X(3).

       FD  FICHIER-RAPPORT.
           01  RAPPORT-REC.
      *        Longueur du segment de données.
               05  WS-RAPPORT-LENGTH  PIC 9(3).
      *        Les données du rapport.
               05  WS-RAPPORT-DATA    PIC X(147).

       WORKING-STORAGE SECTION.
       
      *    Variable pour stocker le code de statut de l'opération
      *    de fichier.
       01  WS-FILE-STATUS        PIC XX.

      *    Date et heure actuelles.
       01  WS-CURRENT-TIME       PIC X(20).

      *    Compteur pour suivre le nombre d'enregistrements lus.
       01  WS-REC-COUNT          PIC 9(4) VALUE ZERO.

      *    Total des enregistrements actifs.
       01  WS-TOTAL-ACTIF        PIC 9(4) VALUE ZERO.

      *    Total des enregistrements inactifs.
       01  WS-TOTAL-INACTIF      PIC 9(4) VALUE ZERO.

      *    Total des enregistrements lus.
       01  WS-TOTAL-RECORDS      PIC 9(6) VALUE ZERO.

      *    Informations de pied de page du rapport.
       01  WS-FOOTER-INFO        PIC X(147).

      *    Ligne vide pour le rapport.
       01  WS-BLANK-LINE          PIC X(147) VALUE SPACES.

       PROCEDURE DIVISION.

      *    Ouverture des fichiers d'entrée et de sortie.
           OPEN INPUT FICHIER-ASSUR-PART1 FICHIER-ASSUR-PART2
                OUTPUT FICHIER-RAPPORT.
                
      *    Écriture du titre du rapport.
           MOVE "Rapport Global des Assurances" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.

      *    Écriture de la section des statuts et contrats.
           MOVE "Section: Statuts et Contrats" TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
           WRITE RAPPORT-REC FROM WS-BLANK-LINE
           WRITE RAPPORT-REC FROM WS-BLANK-LINE.

      *    Initialisation du code de statut de fichier.
           MOVE ZERO TO WS-FILE-STATUS
           
      *    Traitement du premier fichier d'assurance.
           PERFORM PROCESS-FILE-1
           CLOSE FICHIER-ASSUR-PART1

      *    Réinitialisation du code de statut de fichier.
           MOVE ZERO TO WS-FILE-STATUS
           
      *    Traitement du deuxième fichier d'assurance.
           PERFORM PROCESS-FILE-2
           CLOSE FICHIER-ASSUR-PART2

      *    Génération du résumé des enregistrements.
           STRING "Total Records: " WS-TOTAL-RECORDS 
                 DELIMITED BY SIZE
                " Total Actif: " WS-TOTAL-ACTIF 
                 DELIMITED BY SIZE
                " Total Inactif: " WS-TOTAL-INACTIF 
                 DELIMITED BY SIZE
                 INTO WS-FOOTER-INFO
           MOVE WS-FOOTER-INFO TO WS-RAPPORT-DATA
           MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
           WRITE RAPPORT-REC FROM WS-RAPPORT-DATA.

      *    Fermeture du fichier de rapport.
           CLOSE FICHIER-RAPPORT.
           
      *    Affichage d'un message de fin.
           DISPLAY "FIN DE TRAITEMENT DES ENREGISTREMENTS."
           
      *    Arrêt de l'exécution du programme.
           STOP RUN.

       PROCESS-FILE-1.
       
      *    Boucle jusqu'à ce que le code de statut de fichier soit '10',
      *    indiquant la fin du fichier.
           PERFORM UNTIL WS-FILE-STATUS = '10'
               
               READ FICHIER-ASSUR-PART1 INTO ASSUR-REC AT END
               
      *    Si fin de fichier, mettre à jour le code de statut.
                   MOVE '10' TO WS-FILE-STATUS
                   
               NOT AT END
              
      *    Évaluation du statut de l'assurance.
                   EVALUATE ASSUR-STATUT
                       
      *    Si l'assurance est active, incrémenter le total d'assurances
      *    actives.
                       WHEN 'Actif'
                           ADD 1 TO WS-TOTAL-ACTIF
                       
      *    Si l'assurance est inactive, incrémenter le total 
      *    d'assurances inactives.
                       WHEN OTHER
                           ADD 1 TO WS-TOTAL-INACTIF
                   END-EVALUATE
                   
      *    Incrémenter le total des enregistrements lus.
                   ADD 1 TO WS-TOTAL-RECORDS
                   
      *    Obtenir la date et l'heure actuelles.
                   MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME
                   
      *    Construction de la ligne de rapport.
                   STRING " ID: " ASSUR-ID
                          " Assurance souscrite: " ASSUR-NOM 
                          " Statut: " ASSUR-STATUT DELIMITED BY SIZE
                          INTO WS-RAPPORT-DATA
                   MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
                   
      *    Écriture de la ligne de rapport dans le fichier de rapport.
                   WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
                   WRITE RAPPORT-REC FROM WS-BLANK-LINE
               
      *    Lecture de l'enregistrement suivant dans le fichier.
               END-READ
               
      *    Fin de la boucle de traitement.
           END-PERFORM.

       PROCESS-FILE-2.
       
      *    Boucle jusqu'à ce que le code de statut de fichier soit '10',
      *    indiquant la fin du fichier.
           PERFORM UNTIL WS-FILE-STATUS = '10'
               
               READ FICHIER-ASSUR-PART2 INTO ASSUR-REC-2 AT END
               
      *    Si fin de fichier, mettre à jour le code de statut.
                   MOVE '10' TO WS-FILE-STATUS
                   
               NOT AT END
              
      *    Évaluation du statut de l'assurance.
                   EVALUATE ASSUR-STATUT2
                       
      *    Si l'assurance est active, incrémenter le total d'assurances 
      *    actives.
                       WHEN 'Actif'
                           ADD 1 TO WS-TOTAL-ACTIF
                       
      *    Si l'assurance est inactive, incrémenter le total 
      *    d'assurances inactives.
                       WHEN OTHER
                           ADD 1 TO WS-TOTAL-INACTIF
                   END-EVALUATE
                   
      *    Incrémenter le total des enregistrements lus.
                   ADD 1 TO WS-TOTAL-RECORDS
                   
      *    Obtenir la date et l'heure actuelles.
                   MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME
                   
      *    Construction de la ligne de rapport.
                   STRING " ID: " ASSUR-ID2
                          " Assurance souscrite: " ASSUR-NOM2 
                          " Statut: " ASSUR-STATUT2 DELIMITED BY SIZE
                          INTO WS-RAPPORT-DATA
                   MOVE LENGTH OF WS-RAPPORT-DATA TO WS-RAPPORT-LENGTH
                   
      *    Écriture de la ligne de rapport dans le fichier de rapport.
                   WRITE RAPPORT-REC FROM WS-RAPPORT-DATA
                   WRITE RAPPORT-REC FROM WS-BLANK-LINE
               
      *    Lecture de l'enregistrement suivant dans le fichier.
               END-READ
               
      *    Fin de la boucle de traitement.
           END-PERFORM.
