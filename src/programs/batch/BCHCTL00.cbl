       *================================================================*
      * Program Name: BCHCTL00
      * Description: Batch Control Processor
      * Version: 1.1
      * Date: 2024
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BCHCTL00.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BATCH-CONTROL-FILE
               ASSIGN TO BCHCTL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BCT-KEY
               FILE STATUS IS WS-BCT-STATUS.
           SELECT TRANSACTION-HISTORY ASSIGN TO TRANHIST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRAN-KEY
               FILE STATUS IS WS-TRAN-STATUS. *> Added for CHANNEL-CODE support

       DATA DIVISION.
       FILE SECTION.
       FD  BATCH-CONTROL-FILE.
           COPY BCHCTL.
       FD  TRANSACTION-HISTORY.
           COPY TRNREC. *> Ensures CHANNEL-CODE is available

       WORKING-STORAGE SECTION.
           COPY BCHCON.
           COPY ERRHAND.
           01  WS-FILE-STATUS.
               05  WS-BCT-STATUS         PIC X(2).
               05  WS-TRAN-STATUS        PIC X(2). *> Added for TRANSACTION-HISTORY
           01  WS-WORK-AREAS.
               05  WS-CURRENT-TIME       PIC X(26).
               05  WS-PREREQ-MET         PIC X(1).
                   88  PREREQS-SATISFIED    VALUE 'Y'.
                   88  PREREQS-PENDING      VALUE 'N'.
               05  WS-PROCESS-MODE       PIC X(1).
                   88  MODE-INITIALIZE      VALUE 'I'.
                   88  MODE-CHECK-PREREQ    VALUE 'C'.
                   88  MODE-UPDATE-STATUS   VALUE 'U'.
                   88  MODE-FINALIZE        VALUE 'F'.
           01  WS-CHANNEL-CODE           PIC X(04). *> Added for channel assignment

       LINKAGE SECTION.
       01  LS-CONTROL-REQUEST.
           05  LS-FUNCTION          PIC X(4).
               88  FUNC-INIT          VALUE 'INIT'.
               88  FUNC-CHEK          VALUE 'CHEK'.
               88  FUNC-UPDT          VALUE 'UPDT'.
               88  FUNC-TERM          VALUE 'TERM'.
           05  LS-JOB-NAME         PIC X(8).
           05  LS-PROCESS-DATE     PIC X(8).
           05  LS-SEQUENCE-NO      PIC 9(4).
           05  LS-RETURN-CODE      PIC S9(4) COMP.
       
       PROCEDURE DIVISION USING LS-CONTROL-REQUEST.
       0000-MAIN.
           EVALUATE TRUE
               WHEN FUNC-INIT
                   SET MODE-INITIALIZE TO TRUE
                   PERFORM 1000-PROCESS-INITIALIZE
               WHEN FUNC-CHEK
                   SET MODE-CHECK-PREREQ TO TRUE
                   PERFORM 2000-CHECK-PREREQUISITES
               WHEN FUNC-UPDT
                   SET MODE-UPDATE-STATUS TO TRUE
                   PERFORM 3000-UPDATE-STATUS
               WHEN FUNC-TERM
                   SET MODE-FINALIZE TO TRUE
                   PERFORM 4000-PROCESS-TERMINATE
               WHEN OTHER
                   MOVE 'Invalid function code' TO ERR-TEXT
                   PERFORM 9000-ERROR-ROUTINE
           END-EVALUATE
           
           MOVE LS-RETURN-CODE TO RETURN-CODE
           GOBACK
           .
           
       1000-PROCESS-INITIALIZE.
           PERFORM 1100-OPEN-FILES
           PERFORM 1200-READ-CONTROL-RECORD
           PERFORM 1300-VALIDATE-PROCESS
           PERFORM 1400-UPDATE-START-STATUS
           .
           
       1100-OPEN-FILES.
           OPEN INPUT TRANSACTION-HISTORY
           IF WS-TRAN-STATUS NOT = '00'
               MOVE 'ERROR OPENING TRANSACTION HISTORY'
                 TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
           . *> Added open for TRANSACTION-HISTORY to support CHANNEL-CODE

       *================================================================*
      * Detailed procedures to be implemented:
      * 1100-OPEN-FILES
      * 1200-READ-CONTROL-RECORD
      * 1300-VALIDATE-PROCESS
      * 1400-UPDATE-START-STATUS
      * 2200-CHECK-DEPENDENCIES
      * 3200-UPDATE-PROCESS-STATUS
      * 3300-WRITE-CONTROL-RECORD
      * 4100-UPDATE-COMPLETION
      * 4200-CLOSE-FILES
      *================================================================* 