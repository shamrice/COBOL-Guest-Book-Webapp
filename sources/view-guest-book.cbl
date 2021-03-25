       >>source format is fixed
      *>****************************************************************
      *> Author: Erik Eriksen
      *> Create Date: 2021-03-23
      *> Last Modified: 2021-03-25
      *> Purpose: Displays web page with current contents of guest book
      *>          table in database.
      *> Tectonics: ./build_and_deploy.sh
      *>*****************************************************************
       identification division.
       program-id. view-guest-book.

       environment division.

       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.

       data division.

       file section.

      *Must be all uppercase for esqloc precompiler.
       WORKING-STORAGE SECTION.
       EXEC SQL
           BEGIN DECLARE SECTION
       END-EXEC.
       01  HOSTVARS.
           05  BUFFER               PIC X(1024).

       01  ws-guest-book-entry.
           05  ws-guest-name        pic x(256).
           05  ws-guest-email       pic x(256).
           05  ws-guest-date        pic x(256).
           05  ws-guest-comment     pic x(1024).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC.

       01  newline                  constant as x'0a'.


       local-storage section.

       procedure division.

      * TODO : This should be read from a config not hard coded.
           STRING 'DRIVER={PostgreSQL Unicode};'
                'SERVER=localhost;'
                'PORT=5432;'
                'DATABASE=guestbookdb;'
                'UID=postgres;'
                'PWD=password;'
                'COMRESSED_PROTO=0;'
           INTO BUFFER.
           EXEC SQL
               CONNECT TO :BUFFER
           END-EXEC.

           display "Content-type: text/html" newline
           display
               "<!DOCTYPE html>"
               "<html><head><title>GnuCOBOL Sample Guest Book - "
               "View Guest Book</title>"
               "<style>"
               "  table"
               "  {background-color:#e0ffff; border-collapse:collapse;}"
               "  table, th, td"
               "  { border: 1px solid black; }"
               "</style>"
               "</head><body>"
               newline
               '<h2>View Guest Book | <a href="/sign-guest-book.html">'
               "Sign Guest Book</a></h2>"
               newline "<p><b>"
               'Written in GnuCOBOL by Erik Eriksen'
               newline "</b></p><hr />"
           end-display

           EXEC SQL
               DECLARE CUR_ALL CURSOR FOR
               SELECT
                   GUEST_NAME, GUEST_EMAIL, GUEST_COMMENT, CREATE_DT
               FROM GUEST_ENTRY
               ORDER BY CREATE_DT DESC;
           END-EXEC

           perform sqlstate-check

           EXEC SQL
               OPEN CUR_ALL ;
           END-EXEC
           perform sqlstate-check

           perform until sqlcode = 100
               EXEC SQL
                   FETCH CUR_ALL
                   INTO
                       :ws-guest-name,
                       :ws-guest-email,
                       :ws-guest-comment,
                       :ws-guest-date ;
               END-EXEC
               perform sqlstate-check
               if sqlcode not = 100 then
                   display
                       "<p><table>"
                       "<tr><td>Name:</td><td>"
                       ws-guest-name "</td></tr>"
                       "<tr><td>Email:</td><td>"
                       ws-guest-email "</td></tr>"
                       "<tr><td>Date:</td><td>"
                       ws-guest-date "</td></tr>"
                       "<tr><td>Comment:</td><td>"
                       ws-guest-comment "</td></tr>"
                       "</p></table><hr />"
                   end-display
               end-if
           end-perform

           EXEC SQL
               CONNECT RESET
           END-EXEC.
           perform sqlstate-check

           display
               '<a href="'
               'https://github.com/shamrice/COBOL-Guest-Book-Webapp">'
               "COBOL Source Code</a>"
               "</body></html>"
           end-display

           goback.


       sqlstate-check section.
           if sqlcode < 0
               display 'SQLSTATE='  sqlstate,
                   ', SQLCODE=' sqlcode
               if sqlerrml > 0
                   display 'SQL Error message:' sqlerrmc(1:sqlerrml)
               end-if
               move sqlcode to return-code
               stop run
           else if sqlcode > 0 and not = 100
               display 'SQLSTATE='  sqlstate,
                   ', SQLCODE=' sqlcode
               if sqlerrml > 0
                   display 'SQL Warning message:' sqlerrmc(1:sqlerrml)
               end-if
           end-if
           exit section.


       end program view-guest-book.
