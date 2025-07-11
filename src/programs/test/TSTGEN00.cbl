       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTGEN00.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2024-04-09.
      *****************************************************************
      * Test Data Generator                                           *
      *                                                               *
      * Generates test data for system testing:                      *
      * - Portfolio test data                                        *
      * - Transaction test scenarios                                 *
      * - Error condition data                                       *
      * - Performance test volumes                                   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CONS.
           
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-CONFIG ASSIGN TO TSTCFG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CFG-STATUS.

           SELECT PORTFOLIO-OUT ASSIGN TO PORTOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PORT-STATUS.

           SELECT TRANSACTION-OUT ASSIGN TO TRANOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.

           SELECT RANDOM-SEED ASSIGN TO RANDSEED
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RAND-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TEST-CONFIG
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CONFIG-RECORD.
           05  CFG-TEST-TYPE        PIC X(10).
           05  CFG-VOLUME           PIC 9(6).
           05  CFG-PARAMETERS       PIC X(64).

       FD  PORTFOLIO-OUT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PORTFOLIO-RECORD.
           COPY PORTFLIO REPLACING ==:PREFIX:== BY ==PORT==.

       FD  TRANSACTION-OUT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRANSACTION-RECORD.
           COPY TRNREC REPLACING ==:PREFIX:== BY ==TRAN==. *> Now includes CHANNEL-CODE

       * ... rest of the code unchanged, but in 2310-GEN-TRAN-DATA add: ...
       2310-GEN-TRAN-DATA.
           MOVE 'ATM ' TO TRAN-CHANNEL-CODE *> Assign test channel code
           *> Other fields as before
