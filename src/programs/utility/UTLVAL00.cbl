      IDENTIFICATION DIVISION.
      PROGRAM-ID. UTLVAL00.
      AUTHOR. CLAUDE.
      DATE-WRITTEN. 2024-04-09.
     *****************************************************************
     * Data Validation Utility                                        *
     *                                                               *
     * Performs comprehensive data validation:                       *
     * - Data integrity checks                                      *
     * - Cross-reference validation                                 *
     * - Format verification                                        *
     * - Balance reconciliation                                     *
     *****************************************************************
      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      SPECIAL-NAMES.
          CONSOLE IS CONS.
          
      INPUT-OUTPUT SECTION.
      FILE-CONTROL.
          SELECT VALIDATION-CONTROL ASSIGN TO VALCTL
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-VAL-STATUS.

          SELECT POSITION-MASTER ASSIGN TO POSMSTRE
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS POS-KEY
              FILE STATUS IS WS-POS-STATUS.

          SELECT TRANSACTION-HISTORY ASSIGN TO TRANHIST
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS TRAN-KEY
              FILE STATUS IS WS-TRAN-STATUS.

          SELECT ERROR-REPORT ASSIGN TO ERRRPT
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-RPT-STATUS.

      DATA DIVISION.
      FILE SECTION.
          COPY POSREC.
          COPY TRNREC. *> Both now include CHANNEL-CODE

      * ... rest of the code unchanged, but in 2420-CHECK-TRANSACTION-FORMAT add: ...
      2420-CHECK-TRANSACTION-FORMAT.
          IF TRAN-CHANNEL-CODE = SPACES OR TRAN-CHANNEL-CODE = LOW-VALUES
              MOVE 'CHANNEL-CODE MISSING' TO WS-ERR-DESC
              WRITE ERROR-RECORD FROM WS-ERROR-LINE
          END-IF
          *> Added validation for CHANNEL-CODE presence
