[Full COBOL source code with the following changes:]
- In the FILE SECTION, ensure the FD for the transaction file includes the new CHANNEL-CODE field (by including the updated TRNREC.cpy).
- In 2000-PROCESS and 2100-LOAD-HISTORY, update logic to read and process CHANNEL-CODE if transaction records are referenced.
- Add inline comments where CHANNEL-CODE is referenced.