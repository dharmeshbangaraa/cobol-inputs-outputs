[Full COBOL source code with the following changes:]
- In the FILE SECTION, ensure the FD for the transaction file includes the new CHANNEL-CODE field (by including the updated TRNREC.cpy).
- In 2220-CHECK-TRANSACTION-INTEGRITY, 2320-CHECK-TRANSACTION-XREF, and 2420-CHECK-TRANSACTION-FORMAT, add validation logic for CHANNEL-CODE (e.g., check for valid values or non-blank).
- Add inline comments where CHANNEL-CODE is validated.