       IDENTIFICATION DIVISION.
       PROGRAM-ID. A7SPLIT.

      *-----------------------------------------------------------------
      * AUTHOR:  GRANT HEATH
      * PURPOSE: SPLIT VALID INSURANCE DATA INTO REPAIR/REPLACE DATA
      *          FILES AND GENERATE A SUMMARY COUNTS AND CONTROL TOTALS.
      *-----------------------------------------------------------------

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INSUR-VAL-IN  ASSIGN TO INSURVAL
                                ORGANIZATION IS SEQUENTIAL.
           SELECT REPAIR-OUT    ASSIGN TO REPAIR
                                ORGANIZATION IS SEQUENTIAL.
           SELECT REPLACE-OUT   ASSIGN TO OUTREPL
                                ORGANIZATION IS SEQUENTIAL.
           SELECT RPT-OUT       ASSIGN TO RPTOUT
                                ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
      * RECORD DATA LINE
      *-----------------------------------------------------------------
       FD  INSUR-VAL-IN.
       01 INSUR-VAL-REC.
          05 IN-POLICY-NUMBER   PIC X(10).
          05 IN-CUST-NAME       PIC X(20).
          05 IN-PROD-CODE       PIC X(03).
          05 IN-CLAIM-TYPE      PIC X(07).
          05 IN-AMOUNT          PIC 9(04)V99.
          05 IN-JUSTIFY         PIC X(30).
          05 FILLER             PIC X(04).

       FD  REPAIR-OUT.
       01 REPAIR-REC            PIC X(80).

       FD  REPLACE-OUT.
       01 REPLACE-REC           PIC X(80).

       FD  RPT-OUT.
       01 RPT-LINE              PIC X(132).

       WORKING-STORAGE SECTION.

      * FLAGS AND COUNTERS
      *-----------------------------------------------------------------
       01 WS-FLAGS.
          05 WS-EOF             PIC X             VALUE 'N'.
             88 EOF-YES                           VALUE 'Y'.

       01 WS-SUB                PIC 9(02).

      * GRAND TOTALS
      *-----------------------------------------------------------------
       01 WS-GRAND-TOTALS.
          05 WS-TOTAL-COUNT     PIC 9(05)         VALUE 0.
          05 WS-G-AMOUNT        PIC 9(07)V99      VALUE 0.

      * REPAIR TOTALS
      *-----------------------------------------------------------------
       01 WS-REPAIR-TOTALS.
          05 WS-REP-COUNT       PIC 9(05)         VALUE 0.
          05 WS-REP-AMOUNT      PIC 9(07)V99      VALUE 0.
          05 WS-REP-PROD-STATS OCCURS 5 TIMES.
             10 WS-REP-P-COUNT  PIC 9(05)         VALUE 0.
             10 WS-REP-P-AMT    PIC 9(07)V99      VALUE 0.

      * REPLACE TOTALS
      *-----------------------------------------------------------------
       01 WS-REPLACE-TOTALS.
          05 WS-RPL-COUNT       PIC 9(05)         VALUE 0.
          05 WS-RPL-AMOUNT      PIC 9(07)V99      VALUE 0.
          05 WS-RPL-PROD-STATS OCCURS 5 TIMES.
             10 WS-RPL-P-COUNT  PIC 9(05)         VALUE 0.
             10 WS-RPL-P-AMT    PIC 9(07)V99      VALUE 0.

      * REGION STATS (FOR REPLACE ONLY)
      *-----------------------------------------------------------------
       01 WS-REGION-STATS.
          05 WS-RETOTAL-COUNT OCCURS 4 TIMES
                                PIC 9(05)         VALUE 0.
          05 WS-REG-TOTAL       PIC 9(05)         VALUE 0.

      * PRODUCT VALUES
      *-----------------------------------------------------------------
       01 WS-PROD-VALUES.
          05 FILLER             PIC X(3)          VALUE 'FRG'.
          05 FILLER             PIC X(3)          VALUE 'STV'.
          05 FILLER             PIC X(3)          VALUE 'WAS'.
          05 FILLER             PIC X(3)          VALUE 'ACO'.
          05 FILLER             PIC X(3)          VALUE 'OTH'.
       01    REDEFINES WS-PROD-VALUES.
          05 WS-PROD-CODE-VAL   PIC X(3) OCCURS 5 TIMES.

      * REGION VALUES
      *-----------------------------------------------------------------
       01 WS-REG-VALUES.
          05 FILLER             PIC X(3)          VALUE 'ONT'.
          05 FILLER             PIC X(3)          VALUE 'QUE'.
          05 FILLER             PIC X(3)          VALUE 'MAN'.
          05 FILLER             PIC X(3)          VALUE 'ALB'.
       01    REDEFINES WS-REG-VALUES.
          05 WS-REG-CODE-VAL    PIC X(3) OCCURS 4 TIMES.

      * REPORT FORMATTING
      *---------------------------------------------------------------
       01 WS-HDR-1              PIC X(80)         VALUE
             '          CLAIM SPLIT SUMMARY AND CONTROL TOTALS'.

       01 WS-DASHES             PIC X(60)         VALUE ALL '-'.

       01 WS-DETAIL-LINE.
          05 DET-LABEL          PIC X(25).
          05 DET-COUNT          PIC ZZZZ9.
          05 FILLER             PIC X(05)         VALUE SPACES.
          05 DET-AMT-LABEL      PIC X(10).
          05 DET-AMT            PIC $Z,ZZZ,ZZ9.99.
          05 FILLER             PIC X(04)         VALUE SPACES.
          05 DET-PCT-LABEL      PIC X(05).
          05 DET-PCT            PIC ZZ9.9.
          05 DET-PCT-SYM        PIC X             VALUE '%'.

       PROCEDURE DIVISION.
      * Main
      *---------------------------------------------------------------
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-LOOP UNTIL EOF-YES
           PERFORM 3000-FINALIZE-REPORT
           PERFORM 4000-CLOSE
           GOBACK.

      * Initalize the script by opening files and writing headers
      *---------------------------------------------------------------
       1000-INITIALIZE.
           OPEN INPUT INSUR-VAL-IN
           OPEN OUTPUT REPAIR-OUT REPLACE-OUT RPT-OUT

           WRITE RPT-LINE FROM WS-HDR-1
           WRITE RPT-LINE FROM WS-DASHES

           PERFORM 1100-READ-INPUT.

      * Read a input
      *---------------------------------------------------------------
       1100-READ-INPUT.
           READ INSUR-VAL-IN
           AT END
              SET EOF-YES TO TRUE
           END-READ.

      * Sorta each record by REPAIR or REPLACE
      *-----------------------------------------------------------------
       2000-PROCESS-LOOP.
           ADD 1 TO WS-TOTAL-COUNT
           ADD IN-AMOUNT TO WS-G-AMOUNT

           EVALUATE IN-CLAIM-TYPE
           WHEN 'REPAIR '
                PERFORM 2100-PROCESS-REPAIR
           WHEN 'REPLACE'
                PERFORM 2200-PROCESS-REPLACE
           END-EVALUATE

           PERFORM 1100-READ-INPUT.

      *    Process a Repair record
      *    add to the repair counter and move
      *    the record to the repair data file
      *-----------------------------------------------------------------
       2100-PROCESS-REPAIR.
           WRITE REPAIR-REC FROM INSUR-VAL-REC
           ADD 1 TO WS-REP-COUNT
           ADD IN-AMOUNT TO WS-REP-AMOUNT

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
                   IF IN-PROD-CODE = WS-PROD-CODE-VAL(WS-SUB)
                      ADD 1 TO WS-REP-P-COUNT(WS-SUB)
                      ADD IN-AMOUNT TO WS-REP-P-AMT(WS-SUB)
                   END-IF
           END-PERFORM.

      *    Process a Replace record
      *    add to the replace counter and move
      *    the record to the replace data file
      *-----------------------------------------------------------------
       2200-PROCESS-REPLACE.
           WRITE REPLACE-REC FROM INSUR-VAL-REC
           ADD 1 TO WS-RPL-COUNT
           ADD IN-AMOUNT TO WS-RPL-AMOUNT
           ADD 1 TO WS-REG-TOTAL

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
                   IF IN-PROD-CODE = WS-PROD-CODE-VAL(WS-SUB)
                      ADD 1 TO WS-RPL-P-COUNT(WS-SUB)
                      ADD IN-AMOUNT TO WS-RPL-P-AMT(WS-SUB)
                   END-IF
           END-PERFORM

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 4
                   IF IN-POLICY-NUMBER(1:3) = WS-REG-CODE-VAL(WS-SUB)
                      ADD 1 TO WS-RETOTAL-COUNT(WS-SUB)
                   END-IF
           END-PERFORM.

      *    Summary Report
      *    REPAIR  Counters
      *    REPLACE Counters
      *    REPLACE Region Percentages
      *    Grand Totals
      *-----------------------------------------------------------------
       3000-FINALIZE-REPORT.
           *> 1. REPAIR SECTION
           MOVE SPACES TO WS-DETAIL-LINE
           MOVE 'REPAIR TOTALS:' TO DET-LABEL
           MOVE WS-REP-COUNT TO DET-COUNT
           MOVE 'AMOUNT:' TO DET-AMT-LABEL
           MOVE WS-REP-AMOUNT TO DET-AMT
           WRITE RPT-LINE FROM WS-DETAIL-LINE

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
                   MOVE SPACES TO WS-DETAIL-LINE
                   STRING '  ' WS-PROD-CODE-VAL(WS-SUB) ' COUNT:'
                      DELIMITED BY SIZE INTO DET-LABEL
                   MOVE WS-REP-P-COUNT(WS-SUB) TO DET-COUNT
                   MOVE WS-REP-P-AMT(WS-SUB) TO DET-AMT
                   WRITE RPT-LINE FROM WS-DETAIL-LINE
           END-PERFORM

           WRITE RPT-LINE FROM WS-DASHES

           *> 2. REPLACE SECTION
           MOVE SPACES TO WS-DETAIL-LINE
           MOVE 'REPLACE TOTALS:' TO DET-LABEL
           MOVE WS-RPL-COUNT TO DET-COUNT
           MOVE 'AMOUNT:' TO DET-AMT-LABEL
           MOVE WS-RPL-AMOUNT TO DET-AMT
           WRITE RPT-LINE FROM WS-DETAIL-LINE

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
                   MOVE SPACES TO WS-DETAIL-LINE
                   STRING '  ' WS-PROD-CODE-VAL(WS-SUB) ' COUNT:'
                      DELIMITED BY SIZE INTO DET-LABEL
                   MOVE WS-RPL-P-COUNT(WS-SUB) TO DET-COUNT
                   MOVE WS-RPL-P-AMT(WS-SUB) TO DET-AMT
                   WRITE RPT-LINE FROM WS-DETAIL-LINE
           END-PERFORM

           *> 3. REGION PERCENTAGES (REPLACE ONLY)
           WRITE RPT-LINE FROM WS-DASHES
           MOVE 'REPLACE BY REGION ANALYSIS:' TO RPT-LINE
           WRITE RPT-LINE

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 4
                   MOVE SPACES TO WS-DETAIL-LINE
                   STRING '  REGION ' WS-REG-CODE-VAL(WS-SUB)
                      DELIMITED BY SIZE INTO DET-LABEL
                   MOVE WS-RETOTAL-COUNT(WS-SUB) TO DET-COUNT
                   MOVE 'PCT:' TO DET-PCT-LABEL
                   MOVE '%' TO DET-PCT-SYM
                   IF WS-REG-TOTAL > 0
                      *> CALCULATE AND ROUND TO ENSURE PRECISION
                      *> MOVES TO EDITED FIELD
                      COMPUTE DET-PCT ROUNDED =
                         (WS-RETOTAL-COUNT(WS-SUB) / WS-REG-TOTAL) * 100
                   ELSE
                      MOVE 0 TO DET-PCT
                   END-IF
                   WRITE RPT-LINE FROM WS-DETAIL-LINE
           END-PERFORM

           *> 4. GRAND TOTAL
           WRITE RPT-LINE FROM WS-DASHES
           MOVE SPACES TO WS-DETAIL-LINE
           MOVE 'GRAND TOTAL ALL RECORDS:' TO DET-LABEL
           MOVE WS-TOTAL-COUNT TO DET-COUNT
           MOVE 'TOTAL VAL:' TO DET-AMT-LABEL
           MOVE WS-G-AMOUNT TO DET-AMT
           WRITE RPT-LINE FROM WS-DETAIL-LINE.

      * Close files
      *-----------------------------------------------------------------
       4000-CLOSE.
           CLOSE INSUR-VAL-IN
                 REPAIR-OUT
                 REPLACE-OUT
                 RPT-OUT.

       END PROGRAM A7SPLIT.
