       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTPOS00.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2024-04-09.
      *****************************************************************
      * Daily Position Report Generator                                 *
      *                                                               *
      * This program generates the daily position report including:    *
      * - Portfolio position summary                                   *
      * - Transaction activity                                         *
      * - Exception reporting                                          *
      * - Performance metrics                                          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POSITION-MASTER ASSIGN TO POSMSTRE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS POS-KEY
               FILE STATUS IS WS-POSITION-STATUS.

           SELECT TRANSACTION-HISTORY ASSIGN TO TRANHIST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TRAN-KEY
               FILE STATUS IS WS-TRAN-STATUS.

           SELECT REPORT-FILE ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
           COPY POSREC.
           COPY TRNREC. *> Both now include CHANNEL-CODE

       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD             PIC X(132).

       WORKING-STORAGE SECTION.
           COPY RTNCODE.
           COPY ERRHAND.

       01  WS-FILE-STATUS.
           05  WS-POSITION-STATUS    PIC XX.
           05  WS-TRAN-STATUS        PIC XX.
           05  WS-REPORT-STATUS      PIC XX.

       01  WS-REPORT-HEADERS.
           05  WS-HEADER1.
               10  FILLER            PIC X(132) VALUE ALL '*'.
           05  WS-HEADER2.
               10  FILLER            PIC X(40) VALUE SPACES.
               10  FILLER            PIC X(52) 
                   VALUE 'DAILY POSITION REPORT'.
               10  FILLER            PIC X(40) VALUE SPACES.
           05  WS-HEADER3.
               10  FILLER            PIC X(15) VALUE 'REPORT DATE:'.
               10  WS-REPORT-DATE    PIC X(10).
               10  FILLER            PIC X(107) VALUE SPACES.

       01  WS-POSITION-DETAIL.
           05  WS-POS-PORTFOLIO     PIC X(10).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-POS-DESCRIPTION   PIC X(30).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-POS-QUANTITY      PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-POS-VALUE         PIC $$$$,$$$,$$9.99.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-POS-CHANGE-PCT    PIC +ZZ9.99.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  WS-CHANNEL-CODE      PIC X(04). *> Added for channel reporting
           05  FILLER               PIC X(34) VALUE SPACES.

       * ... rest of the code unchanged, but in 2110-FORMAT-POSITION add: ...
       2110-FORMAT-POSITION.
           MOVE POS-PORTFOLIO-ID   TO WS-POS-PORTFOLIO
           MOVE POS-DESCRIPTION    TO WS-POS-DESCRIPTION
           MOVE POS-QUANTITY       TO WS-POS-QUANTITY
           MOVE POS-CURRENT-VALUE  TO WS-POS-VALUE
           COMPUTE WS-POS-CHANGE-PCT = 
               (POS-CURRENT-VALUE - POS-PREVIOUS-VALUE) /
                POS-PREVIOUS-VALUE * 100
           MOVE POS-CHANNEL-CODE   TO WS-CHANNEL-CODE *> Output channel code
           WRITE REPORT-RECORD FROM WS-POSITION-DETAIL.
