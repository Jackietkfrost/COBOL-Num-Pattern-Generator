       IDENTIFICATION DIVISION.
       PROGRAM-ID. PATTCBL
      ******************************
      *AUTHOR:     JACKIE MARCANO  *
      ******************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAIN-MENU-INPUT.
           05  PATTERN-SELECTION   PIC 99.
               88  PATTERN1    VALUE   1.
               88  PATTERN2    VALUE   2.
               88  PATTERN3    VALUE   3.
               88  PATTERN4    VALUE   4.
               88  PATTERN5    VALUE   5.
               88  PATTERN6    VALUE   6.
               88  PATTERN7    VALUE   7.
               88  PATTERN8    VALUE   8.
               88  PATTERN9    VALUE   9.
               88  EXITPROG    VALUE   10.
           05  USR-SIZE            PIC 9.
        
       01 HALF-USR-SIZE    PIC 9.
       01  VARIATION1       PIC 9.
       01  DUMMY           PIC X.
       01  USR-CONT        PIC XXX.
       01  COUNTER1        PIC 99.
       01  COUNTER2        PIC 99.
       01  BIN-CHANGER     PIC 9.
       01  CTR2-OUT        PIC 9.
       
      ******************************************************************
       SCREEN SECTION.

       01 MAIN-MENU
           BLANK SCREEN.
           05 LINE 3  COLUMN 25 VALUE "************************".           
           05 LINE 4  COLUMN 25 VALUE "*   PATTERN GENERATOR  *".           
           05 LINE 5  COLUMN 25 VALUE "*         BY    	   	  *".           
           05 LINE 6  COLUMN 25 VALUE "* ALEK MARCANO MORALES *".           
           05 LINE 7  COLUMN 25 VALUE "************************".          
           05 LINE 9  COLUMN 23 VALUE "SELECT A PATTERN BETWEEN (1-9)".
           05 LINE 3  COLUMN 60 VALUE "PATTERNS:".
           05 LINE 4  COLUMN 60 VALUE "1.DESCENDING".
           05 LINE 5  COLUMN 60 VALUE "2.DOUBLES DESCENDING".
           05 LINE 6  COLUMN 60 VALUE "3.INVERTED SUB ASCENDING".
           05 LINE 7  COLUMN 60 VALUE "4.INVERTED DESCENDING".
           05 LINE 8  COLUMN 60 VALUE "5.ASCENDING".
           05 LINE 9  COLUMN 60 VALUE "6.COUNT DOWN COUNT UP".
           05 LINE 10 COLUMN 60 VALUE "7.PUSHED DESCENDING".
           05 LINE 11 COLUMN 60 VALUE "8.BINARY DESCENDING".
           05 LINE 12 COLUMN 60 VALUE "9.ARRAY LIST".     
           05 LINE 10 COLUMN 30    VALUE "TO EXIT ENTER 10".         
           05 LINE 11 COLUMN 35    PIC ZZ USING PATTERN-SELECTION.          
           05 LINE 12 COLUMN 25    VALUE "PRESS TAB TO CONTINUE".           
           05 LINE 14 COLUMN 22    VALUE
           "ENTER THE SIZE FOR PATTERN (1-9)".
           05 LINE 15 COLUMN 35    PIC Z USING USR-SIZE.
           
      ******************************************************************

       01 PATTERN-DISPLAY
           BLANK SCREEN.
           05 LINE 1 COLUMN 25 VALUE
           "WOULD YOU LIKE TO DO ANOTHER SIZE?".
           05 LINE 1 COLUMN 60 USING USR-CONT PIC XXX.           
           05 LINE 2 COLUMN 25 VALUE
           "IF ANSWER YES, PRESS TAB TO ENTER SIZE. (1-9)".
           05 LINE 3 COLUMN 25 USING USR-SIZE PIC 9.          
           05 LINE 4 COLUMN 1.

       01 ERROR-SCREEN
           BLANK SCREEN.
           05 LINE 5 COLUMN 10 VALUE
           "VALUE INCORRECT. PLEASE CHOOSE BETWEEN 1 TO 10.".
           05 LINE 6 COLUMN 10 USING DUMMY.
           
       PROCEDURE DIVISION.

       100-MAIN.
           PERFORM UNTIL PATTERN-SELECTION = 10
               MOVE ZERO TO PATTERN-SELECTION
               MOVE ZERO TO USR-SIZE
               MOVE SPACE TO USR-CONT
               DISPLAY MAIN-MENU
               ACCEPT MAIN-MENU
               EVALUATE TRUE   
                   WHEN PATTERN1
                       PERFORM 200-DOWN-ASCENDING-PATTERN
                   WHEN PATTERN2
                       PERFORM 300-DBL-ASCENDING-PATTERN
                   WHEN PATTERN3
                        PERFORM 400-RVRS-DESCENDING-PATTERN
                   WHEN PATTERN4
                        PERFORM 500-RVRS-ASCENDING-PATTERN
                   WHEN PATTERN5
                         PERFORM 600-DESCENDING-PATTERN
                   WHEN PATTERN6
                        PERFORM 700-ZIGZAG-PATTERN
                   WHEN PATTERN7
                        PERFORM 800-PUSHED-ASCENDING-PATTERN
                   WHEN PATTERN8
                        PERFORM 900-BINARY-ASCENDING-PATTERN
                   WHEN PATTERN9
                        PERFORM 1000-INDEX-LIST
                   WHEN EXITPROG
                        STOP RUN    
                   WHEN OTHER
                       DISPLAY ERROR-SCREEN
                       ACCEPT ERROR-SCREEN
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       200-DOWN-ASCENDING-PATTERN.
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM 1 BY 1
                 UNTIL COUNTER1 GREATER THAN  USR-SIZE
                   PERFORM VARYING COUNTER2 FROM 1 BY 1
                     UNTIL COUNTER2 GREATER THAN COUNTER1
                       MOVE COUNTER2 TO CTR2-OUT
                       DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                   END-PERFORM
                   DISPLAY SPACE
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.
       300-DBL-ASCENDING-PATTERN.
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM 1 BY 1
                 UNTIL COUNTER1 GREATER THAN  USR-SIZE
                   PERFORM VARYING COUNTER2 FROM 1 BY 1
                     UNTIL COUNTER2 GREATER THAN COUNTER1
                       MOVE COUNTER1 TO CTR2-OUT
                       DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                   END-PERFORM
                   DISPLAY SPACE
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.

       400-RVRS-DESCENDING-PATTERN.
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM USR-SIZE BY -1
                 UNTIL COUNTER1 LESS THAN 1
                   PERFORM VARYING COUNTER2 FROM COUNTER1 BY -1
                     UNTIL COUNTER2 LESS THAN 1
                       MOVE COUNTER2 TO CTR2-OUT
                       DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                   END-PERFORM
                   DISPLAY SPACE              
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.

       500-RVRS-ASCENDING-PATTERN.
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM USR-SIZE BY -1
                 UNTIL COUNTER1 LESS THAN  1
                   PERFORM VARYING COUNTER2 FROM USR-SIZE BY -1
                     UNTIL COUNTER2 LESS THAN COUNTER1
                       MOVE COUNTER2 TO CTR2-OUT
                          DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                   END-PERFORM
                   DISPLAY SPACE
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.

       600-DESCENDING-PATTERN.
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM USR-SIZE BY -1
                 UNTIL COUNTER1 LESS THAN 1
                   PERFORM VARYING COUNTER2 FROM 1 BY 1
                     UNTIL COUNTER2 GREATER THAN COUNTER1
                       MOVE COUNTER2 TO CTR2-OUT
                       DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                   END-PERFORM
                   DISPLAY SPACE               
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.

       700-ZIGZAG-PATTERN.
           COMPUTE HALF-USR-SIZE ROUNDED = USR-SIZE/2
           MOVE HALF-USR-SIZE TO VARIATION1
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM 1 BY 1
               UNTIL COUNTER1 GREATER THAN HALF-USR-SIZE
                   PERFORM VARYING COUNTER2 FROM 1 BY 1
                   UNTIL COUNTER2 GREATER THAN VARIATION1
                       MOVE COUNTER2 TO CTR2-OUT
                       DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                   END-PERFORM
                SUBTRACT 1 FROM VARIATION1
               DISPLAY SPACE

               IF COUNTER1 EQUALS HALF-USR-SIZE
                   MOVE 2 TO VARIATION1
                   PERFORM VARYING COUNTER1 FROM HALF-USR-SIZE BY 1
                     UNTIL COUNTER1 EQUAL TO USR-SIZE
                       PERFORM VARYING COUNTER2 FROM 1 BY 1
                         UNTIL COUNTER2 GREATER THAN VARIATION1
                           MOVE COUNTER2 TO CTR2-OUT
                           DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                       END-PERFORM
                           ADD 1 TO VARIATION1
                   DISPLAY SPACE
                   END-PERFORM
               END-IF
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.

       800-PUSHED-ASCENDING-PATTERN.
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM 1 BY 1
                 UNTIL COUNTER1 GREATER THAN  USR-SIZE
                   PERFORM VARYING COUNTER2 FROM COUNTER1 BY -1
                     UNTIL COUNTER2 LESS THAN 1
                       MOVE COUNTER2 TO CTR2-OUT
                       DISPLAY CTR2-OUT, " ", WITH NO ADVANCING
                   END-PERFORM
               DISPLAY SPACE        
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.

       900-BINARY-ASCENDING-PATTERN.
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM 1 BY 1
                 UNTIL COUNTER1 GREATER THAN  USR-SIZE
                   MOVE 1 TO BIN-CHANGER
                   PERFORM VARYING COUNTER2 FROM 1 BY 1
                     UNTIL COUNTER2 GREATER THAN COUNTER1
                       DISPLAY BIN-CHANGER, " ", WITH NO ADVANCING
                       IF BIN-CHANGER = 1
                           THEN MOVE 0 TO BIN-CHANGER
                       ELSE 
                           MOVE 1 TO BIN-CHANGER
                       END-IF
                       
                   END-PERFORM
                   DISPLAY SPACE              
               END-PERFORM

               ACCEPT PATTERN-DISPLAY
           END-PERFORM.

       1000-INDEX-LIST.    
           PERFORM UNTIL USR-CONT = "NO "
               MOVE SPACE TO USR-CONT
               DISPLAY PATTERN-DISPLAY

               PERFORM VARYING COUNTER1 FROM 0 BY 1
                 UNTIL COUNTER1 EQUAL TO  USR-SIZE
                   PERFORM VARYING COUNTER2 FROM 0 BY 1
                     UNTIL COUNTER2 EQUAL TO USR-SIZE
                       IF COUNTER1 EQUAL TO COUNTER2 THEN
                           MOVE COUNTER2 TO CTR2-OUT
                           DISPLAY CTR2-OUT WITH NO ADVANCING
                       ELSE
                           DISPLAY ZERO WITH NO ADVANCING
                       END-IF
                   END-PERFORM
                   DISPLAY SPACE               
               END-PERFORM

           ACCEPT PATTERN-DISPLAY
           END-PERFORM.
       END PROGRAM PATTCBL.
