       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELFLIGHTID.
       AUTHOR. SCW.
  
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 SWITCHES. 
          05 SW-VALID-INPUT           PIC X VALUE 'N'.           
             88 YS-VALID-INPUT              VALUE 'Y'.             
             88 NO-VALID-INPUT              VALUE 'N'. 
          05 SW-DEL-FLIGHTN           PIC X VALUE 'N'.           
             88 YS-DEL-FLIGHTN              VALUE 'Y'.             
             88 NO-DEL-FLIGHTN              VALUE 'N'.   

       01 WK-ERR-PARAGRAPH            PIC X(50) VALUE SPACES.       
       01 WK-ERR-DESC                 PIC X(80) VALUE SPACES.   
       01 WK-DIS-MSG                  PIC X(80) VALUE SPACES.
       01 W-FLIGHT-NUMBER             PIC X(50).
       01 W-SQL-TEXT                  PIC X(200).

       COPY BCESET1. 
       COPY BCEMSG01. 
       COPY DELCONFM1. 
       COPY ERRMSGM. 
       COPY ERRVALI. 

       COPY DFHAID.
       COPY DFHBMSCA.

       EXEC SQL 
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL 
          INCLUDE @TDEFFLI
       END-EXEC.

       PROCEDURE DIVISION.
       0000-MAIN.
      **********************************************
           PERFORM 1000-HOUSEKEEPING
           PERFORM 2000-PROCESS
           PERFORM 9999-END-PROCESS.

      **********************************************
       1000-HOUSEKEEPING.
      **********************************************
           INITIALIZE BCEMSGM1
                REPLACING ALPHABETIC DATA BY SPACES
                
           EXEC CICS SEND 
              MAP('BCEMSGM1')                             
              MAPSET('BCESET1')                           
              ERASE                                      
           END-EXEC 
 
           EXEC CICS RECEIVE 
              MAP('BCEMSGM1')                          
              MAPSET('BCESET1')                       
              INTO(BCEMSGM1)                          
           END-EXEC

           MOVE BCEFLIGHNI TO W-FLIGHT-NUMBER.
          
      **********************************************
       2000-PROCESS.
      **********************************************
           SET NO-VALID-INPUT TO TRUE
           PERFORM 2100-CONFIRM-DELETE UNTIL YS-VALID-INPUT
 
           IF YS-DEL-FLIGHTN THEN
              PERFORM 2200-DELETE-FLIGHTN
           END-IF.

      ********************************************** 
       2100-CONFIRM-DELETE. 
      ********************************************** 
           INITIALIZE DELCONFM1
           MOVE W-FLIGHT-NUMBER TO CONFFLIGHNO
           MOVE SPACES          TO CONFCONFIRI
 
           EXEC CICS SEND 
              MAP('UPDCONFM1')                             
              MAPSET('UPDCONF01')                           
              FROM (UPDCONFM1)                                      
           END-EXEC 
 
           EXEC CICS RECEIVE 
              MAP('UPDCONFM1')                          
              MAPSET('UPDCONF01')                       
              INTO(UPDCONFM1)                          
           END-EXEC
 
           IF CONFCONFIRI = 'Y' THEN
              SET YS-DEL-FLIGHTN            TO TRUE
              SET YS-VALID-INPUT            TO TRUE
           ELSE
              IF CONFCONFIRI = 'N' THEN
                 SET NO-DEL-FLIGHTN         TO TRUE
                 SET YS-VALID-INPUT         TO TRUE
 
                 STRING 'UPDATE OF ' CONFFLIGHNO 
                        ' WAS ABORTED'
                 INTO WK-DIS-MSG
    
                 PERFORM 7100-DISPLAY-RESULT
              ELSE 
                 SET NO-VALID-INPUT TO TRUE
 
                 STRING 'UNEXPECTED RESPONSE ' CONFCONFIRI 
                     '. ENTER Y (YES) OR N (NO)' 
                 INTO WK-DIS-MSG
     
                 PERFORM 7100-DISPLAY-RESULT
              END-IF
           END-IF.

      ********************************************** 
       2200-DELETE-FLIGHTN.
      ********************************************** 
           PERFORM 2210-PREPARE-SQL
           PERFORM 2220-EXECUTE-SQL.

      ********************************************** 
       2210-PREPARE-SQL.
      ********************************************** 
           MOVE SPACES TO W-SQL-TEXT

           STRING "DELETE FROM BEDEF.TDEFFLI "
                  "WHERE FLIGHT_NUMBER_ID = '" W-FLIGHT-NUMBER "'"
                INTO W-SQL-TEXT

           EXEC SQL
              PREPARE DELETE_SQL FROM :W-SQL-TEXT
           END-EXEC.

           IF SQLCODE NOT = 0
              MOVE '2210-PREPARE-SQL' TO WK-ERR-PARAGRAPH

              STRING 'WITH SQLCODE = ' SQLCODE
              INTO WK-ERR-DESC

              PERFORM 9950-ERROR-MESSAGE  
           END-IF.

      ********************************************** 
       2220-EXECUTE-SQL.       
      ********************************************** 
           EXEC SQL
               EXECUTE DELETE_SQL 
           END-EXEC.

           EVALUATE SQLCODE
              WHEN 0
                 STRING 'FLIGHT NUMBER ' W-FLIGHT-NUMBER 
                        ' DELETED SUCCESSFULLY.'
                 INTO WK-DIS-MSG
    
                 PERFORM 7100-DISPLAY-RESULT  
              WHEN 100
                 STRING 'FLIGHT NUMBER ' W-FLIGHT-NUMBER
                        ' NOT FOUND IN DATABASE' 
                 INTO WK-DIS-MSG 
 
                 PERFORM 7100-DISPLAY-RESULT 
              WHEN OTHER
                 MOVE '2220-EXECUTE-SQL' TO WK-ERR-PARAGRAPH
   
                 STRING 'WITH SQLCODE = ' SQLCODE
                 INTO WK-ERR-DESC
   
                 PERFORM 9950-ERROR-MESSAGE  
           END-EVALUATE.

      ********************************************** 
       7100-DISPLAY-RESULT.
      ********************************************** 
           INITIALIZE BCEMSGM1
 
           MOVE WK-DIS-MSG     TO RETMSG01O 
 
           EXEC CICS SEND
                MAP('BCEMSGM1')
                MAPSET('BCEMSG01')
                FROM (BCEMSGM1)
           END-EXEC.

      ********************************************** 
       9950-ERROR-MESSAGE.
      ********************************************** 
           INITIALIZE ERRMSGM
 
           STRING  'FATAL ERROR IN ' WK-ERR-PARAGRAPH
                   ' ' WK-ERR-DESC
           INTO ERRMSG01O
 
           EXEC CICS SEND
                MAP('ERRMSGM1')
                MAPSET('ERRMSG01')
                FROM (ERRMSGM1)
           END-EXEC
 
           PERFORM 9999-END-PROCESS.

      **********************************************  
       9999-END-PROCESS.      
      **********************************************                                            
           EXEC CICS RETURN                                          
           END-EXEC.
