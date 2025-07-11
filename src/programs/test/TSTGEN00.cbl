[Full COBOL source code with the following changes:]
- In the FILE SECTION, ensure the FD for the transaction file includes the new CHANNEL-CODE field (by including the updated TRNREC.cpy).
- In 2300-GEN-TRANSACTION and 2320-WRITE-TRAN-RECORD, generate and populate CHANNEL-CODE with test values.
- Add inline comments where CHANNEL-CODE is set or written.