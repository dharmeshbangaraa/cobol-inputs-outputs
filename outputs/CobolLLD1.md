# Technical Document and Low-Level Design (LLD)

## 1. Introduction  
This document provides a comprehensive technical overview and Low-Level Design (LLD) for the COBOL legacy application modules impacted by the addition of a `CHANNEL-CODE` field to the transaction record. The purpose of this document is to describe the existing logic, detail the required changes, and provide clear guidance for implementing and reviewing the modifications. This change is critical for enabling channel identification in batch processing and reporting, and affects core transaction data structures, file layouts, batch processing, reporting, and associated copybooks.

---

## 2. Existing Logic and Flow  

### 2.1 Overview  
The COBOL application processes business-critical transaction data through a set of modular programs. The transaction record structure is defined in a central copybook (`TRNREC.cpy`) and is referenced by all programs that read, write, or validate transaction files. The main impacted modules include batch reporting, utility validation, and test data generation/validation. Each program follows a structured flow: initialization (file open), main processing (transaction handling), and termination (file close and reporting).

### 2.2 Detailed Logic  

#### Transaction Record Structure (`src/copybook/common/TRNREC.cpy`)
```cobol
01  TRANSACTION-RECORD.
    05  TRN-KEY.
        10  TRN-DATE           PIC X(08).
        10  TRN-TIME           PIC X(06).
        10  TRN-PORTFOLIO-ID   PIC X(08).
        10  TRN-SEQUENCE-NO    PIC X(06).
    05  TRN-DATA.
        10  TRN-INVESTMENT-ID  PIC X(10).
        10  TRN-TYPE           PIC X(02).
            88  TRN-TYPE-BUY     VALUE 'BU'.
            88  TRN-TYPE-SELL    VALUE 'SL'.
            88  TRN-TYPE-TRANS   VALUE 'TR'.
            88  TRN-TYPE-FEE     VALUE 'FE'.
        10  TRN-QUANTITY       PIC S9(11)V9(4) COMP-3.
        10  TRN-PRICE         PIC S9(11)V9(4) COMP-3.
        10  TRN-AMOUNT        PIC S9(13)V9(2) COMP-3.
        10  TRN-CURRENCY      PIC X(03).
        10  TRN-STATUS        PIC X(01).
            88  TRN-STATUS-PEND   VALUE 'P'.
            88  TRN-STATUS-DONE   VALUE 'D'.
            88  TRN-STATUS-FAIL   VALUE 'F'.
            88  TRN-STATUS-REV    VALUE 'R'.
    05  TRN-AUDIT.
        10  TRN-PROCESS-DATE  PIC X(26).
        10  TRN-PROCESS-USER  PIC X(08).
    05  TRN-FILLER           PIC X(50).
```

#### Program Flows

##### Batch Reporting (`src/programs/batch/RPTPOS00.cbl`)
- **Initialization:**  
  - Opens POSITION-MASTER, TRANSACTION-HISTORY, and REPORT-FILE.
  - Handles file status errors.
- **Processing:**  
  - Reads transactions and summarizes activity.
- **Termination:**  
  - Writes reports and closes files.

##### Utility Validation (`src/programs/utility/UTLVAL00.cbl`)
- **Initialization:**  
  - Opens VALIDATION-CONTROL, POSITION-MASTER, TRANSACTION-HISTORY, and ERROR-REPORT.
- **Processing:**  
  - Checks position and transaction file formats.
- **Termination:**  
  - Reports errors and closes files.

##### Test Data Generation (`src/programs/test/TSTGEN00.cbl`)
- **Initialization:**  
  - Opens TEST-CONFIG, PORTFOLIO-OUT, TRANSACTION-OUT, and RANDOM-SEED.
- **Processing:**  
  - Generates transaction data and writes records.
- **Termination:**  
  - Closes files.

##### Test Data Validation (`src/programs/test/TSTVAL00.cbl`)
- **Initialization:**  
  - Opens TEST-CASES, EXPECTED-RESULTS, ACTUAL-RESULTS, and TEST-REPORT.
- **Processing:**  
  - Executes tests based on type and validates results.
- **Termination:**  
  - Writes test details and closes files.

#### Flowchart:  
```
flowchart TD
    Start(["Start"])
    OpenFiles["Open
Files"]
    MainProcessing["Main
Processing"]
    WriteReports["Write
Reports/Output"]
    End(["End"])

    Start --> OpenFiles
    OpenFiles --> MainProcessing
    MainProcessing --> WriteReports
    WriteReports --> End
```

---

## 3. Proposed Changes  

### 3.1 User Story or Analysis Report Summary  
**User Story:**  
Add `CHANNEL-CODE` Field to Transaction File for Channel Identification in Batch Processing and Reporting.

- The `CHANNEL-CODE` field will be added to the transaction record structure.
- All programs that read/write transaction data or generate reports must be updated to handle the new field.

### 3.2 Proposed Code Changes Summary

#### 3.2.1 Impacted Sections and Files

| File Path                                    | Sections/Paragraphs                  |
|----------------------------------------------|--------------------------------------|
| src/copybook/common/TRNREC.cpy               | CHANNEL-CODE definition              |
| src/programs/batch/RPTPOS00.cbl              | 1100-OPEN-FILES, 2200-PROCESS-TRANSACTIONS |
| src/programs/utility/UTLVAL00.cbl            | 1100-OPEN-FILES, 2400-CHECK-FORMAT  |
| src/programs/test/TSTGEN00.cbl               | 2300-GEN-TRANSACTION, 2320-WRITE-TRAN-RECORD |
| src/programs/test/TSTVAL00.cbl               | 1100-OPEN-FILES, 2100-EXECUTE-TEST  |

- **Purpose of Changes:**  
  To enable identification of the transaction channel (e.g., ONLINE, BRANCH, ATM) for each transaction, supporting enhanced reporting, validation, and test coverage.
- **Impact:**  
  - All transaction file layouts and copybooks must be updated.
  - All programs that process transaction records must read, write, and validate the new field.
  - Test data generation and validation must include the new field.

### 3.3 Insertion Points  

- **Copybook (`TRNREC.cpy`):**  
  - Add `CHANNEL-CODE` field to the transaction record structure, ideally after `TRN-CURRENCY` or before `TRN-STATUS` for logical grouping.
- **Batch Reporting (`RPTPOS00.cbl`):**  
  - Update FD and record layouts to include `CHANNEL-CODE`.
  - Update report output to display or process the new field.
- **Utility Validation (`UTLVAL00.cbl`):**  
  - Update FD and validation logic to check for presence and format of `CHANNEL-CODE`.
- **Test Data Generation (`TSTGEN00.cbl`):**  
  - Update record generation logic to populate `CHANNEL-CODE` with valid test values.
- **Test Data Validation (`TSTVAL00.cbl`):**  
  - Update validation logic to check `CHANNEL-CODE` in test records.

### 3.4 Structured Diffs  

#### A. `src/copybook/common/TRNREC.cpy`

**Before:**
```cobol
    10  TRN-CURRENCY      PIC X(03).
    10  TRN-STATUS        PIC X(01).
```
**After:**
```cobol
    10  TRN-CURRENCY      PIC X(03).
    10  CHANNEL-CODE      PIC X(04).
    10  TRN-STATUS        PIC X(01).
```
*// Inserted CHANNEL-CODE (4 chars, adjust length as per requirements) after TRN-CURRENCY for logical grouping.*

---

#### B. All Impacted Programs (FD/Record Layouts)

**Before:**
```cobol
       FILE SECTION.
           COPY TRNREC.
```
**After:**
```cobol
       FILE SECTION.
           COPY TRNREC.
*// No change to the COPY statement, but the expanded copybook now includes CHANNEL-CODE.
```

---

#### C. Batch Reporting (`RPTPOS00.cbl`)

**Before (Report Output Example):**
```cobol
    MOVE TRN-TYPE TO RPT-TYPE
    MOVE TRN-AMOUNT TO RPT-AMOUNT
```
**After:**
```cobol
    MOVE TRN-TYPE TO RPT-TYPE
    MOVE TRN-AMOUNT TO RPT-AMOUNT
    MOVE CHANNEL-CODE TO RPT-CHANNEL-CODE
```
*// Add CHANNEL-CODE to report output fields and layouts.*

---

#### D. Utility Validation (`UTLVAL00.cbl`)

**Before (Format Check Example):**
```cobol
    IF TRN-TYPE NOT = 'BU' AND NOT = 'SL' AND NOT = 'TR' AND NOT = 'FE'
        MOVE 'INVALID TYPE' TO WS-ERROR-MESSAGE
        PERFORM 9999-ERROR-HANDLER
    END-IF
```
**After:**
```cobol
    IF TRN-TYPE NOT = 'BU' AND NOT = 'SL' AND NOT = 'TR' AND NOT = 'FE'
        MOVE 'INVALID TYPE' TO WS-ERROR-MESSAGE
        PERFORM 9999-ERROR-HANDLER
    END-IF

    IF CHANNEL-CODE NOT = 'ONLN' AND NOT = 'BRCH' AND NOT = 'ATM'
        MOVE 'INVALID CHANNEL CODE' TO WS-ERROR-MESSAGE
        PERFORM 9999-ERROR-HANDLER
    END-IF
```
*// Add validation for CHANNEL-CODE values.*

---

#### E. Test Data Generation (`TSTGEN00.cbl`)

**Before:**
```cobol
    MOVE 'BU' TO TRN-TYPE
    MOVE 1000 TO TRN-AMOUNT
```
**After:**
```cobol
    MOVE 'BU' TO TRN-TYPE
    MOVE 1000 TO TRN-AMOUNT
    MOVE 'ONLN' TO CHANNEL-CODE
```
*// Populate CHANNEL-CODE with a valid test value.*

---

#### F. Test Data Validation (`TSTVAL00.cbl`)

**Before:**
```cobol
    IF ACTUAL-TRN-TYPE NOT = EXPECTED-TRN-TYPE
        MOVE 'TYPE MISMATCH' TO WS-ERROR-MESSAGE
        PERFORM 9999-ERROR-HANDLER
    END-IF
```
**After:**
```cobol
    IF ACTUAL-TRN-TYPE NOT = EXPECTED-TRN-TYPE
        MOVE 'TYPE MISMATCH' TO WS-ERROR-MESSAGE
        PERFORM 9999-ERROR-HANDLER
    END-IF

    IF ACTUAL-CHANNEL-CODE NOT = EXPECTED-CHANNEL-CODE
        MOVE 'CHANNEL CODE MISMATCH' TO WS-ERROR-MESSAGE
        PERFORM 9999-ERROR-HANDLER
    END-IF
```
*// Add validation for CHANNEL-CODE in test results.*

---

## 4. Conclusion  
The addition of the `CHANNEL-CODE` field to the transaction record and all associated programs enhances the system's ability to track and report transaction channels, supporting improved analytics and operational oversight. All impacted modules have been identified and detailed changes provided to ensure seamless integration and continued data integrity. The changes adhere to COBOL best practices and are structured for ease of review and implementation.

---
