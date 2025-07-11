      IDENTIFICATION DIVISION.
      PROGRAM-ID. TSTVAL00.
      AUTHOR. CLAUDE.
      DATE-WRITTEN. 2024-04-09.
     *****************************************************************
     * Test Validation Suite                                         *
     *                                                               *
     * Validates test results and system behavior:                   *
     * - Test case execution                                        *
     * - Result validation                                          *
     * - Error condition testing                                    *
     * - Performance benchmarking                                   *
     *****************************************************************
      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      SPECIAL-NAMES.
          CONSOLE IS CONS.
          
      INPUT-OUTPUT SECTION.
      FILE-CONTROL.
          SELECT TEST-CASES ASSIGN TO TESTCASE
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-TEST-STATUS.

          SELECT EXPECTED-RESULTS ASSIGN TO EXPECTED
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-EXP-STATUS.

          SELECT ACTUAL-RESULTS ASSIGN TO ACTUAL
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-ACT-STATUS.

          SELECT TEST-REPORT ASSIGN TO TESTRPT
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-RPT-STATUS.

      DATA DIVISION.
      FILE SECTION.
          COPY TRNREC. *> Now includes CHANNEL-CODE

      * ... rest of the code unchanged, but in 2600-VALIDATE-RESULTS add: ...
      2600-VALIDATE-RESULTS.
          IF TRAN-CHANNEL-CODE = SPACES OR TRAN-CHANNEL-CODE = LOW-VALUES
              MOVE 'CHANNEL-CODE MISSING IN TEST' TO WS-ERROR-MESSAGE
              PERFORM 9999-ERROR-HANDLER
          END-IF
          *> Added validation for CHANNEL-CODE in test results
