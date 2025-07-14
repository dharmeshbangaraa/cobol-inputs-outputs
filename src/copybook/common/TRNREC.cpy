01  TRANSACTION-RECORD.
           05  TRN-KEY.
               10  TRN-DATE           PIC X(08).
               10  TRN-TIME           PIC X(06).
           05  TRN-ACCOUNT-NUMBER     PIC X(12).
           05  TRN-AMOUNT             PIC 9(09)V99.
           05  TRN-TYPE               PIC X(02).
           05  TRN-STATUS             PIC X(01).
           05  CHANNEL-CODE           PIC X(04).    *-- Change: Added CHANNEL-CODE for channel identification (user story #CHANNEL-CODE)