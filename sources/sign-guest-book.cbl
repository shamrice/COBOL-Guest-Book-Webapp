       >>source format is fixed
      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-03-23
      * Last Modified: 2021-03-25
      * Purpose: Page that receives the POST event from the sign guest
      *          book form from the HTML page. Checks if simple math
      *          question is correct (to deter bots) and if comment field
      *          is populated. If so, entry is added to the database.
      *          HTML displayed to the user is status of guest book
      *          signing as well as links to sign again and view contents.
      * Tectonics: ./build_and_deploy.sh
      ******************************************************************
       identification division.
       program-id. sign-guest-book.

       environment division.

       configuration section.
       repository.
           function get-param-value
           function html-decode
           function all intrinsic.

       input-output section.
       file-control.

           select fd-web-input assign to KEYBOARD
           file status is ws-input-status.

       data division.

       file section.

       fd  fd-web-input.
       01  f-chunk-of-post     pic x(2046).

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
           05  ws-guest-comment     pic x(1024).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC.

       01  ws-http-request-method-header constant as "REQUEST_METHOD".

       01  ws-http-value-string pic x(2046).
           88  IS-POST          value 'POST'.

       01  ws-input-status          pic xx.
       01  ws-file-status           pic xx.
       01  newline                  pic x value x'0a'.

       01  ws-temp                  pic x(1024).

       local-storage section.

       procedure division.

           display "Content-type: text/html" newline

           display
               "<!DOCTYPE html>"
               "<html><head><title>GnuCOBOL Sample Guest Book - "
               "Signed Guest Book</title>"
               "<style>"
               "  table"
               "{ background-color:#e0ffff; border-collapse:collapse; }"
               "  table, th, td"
               "  { border: 1px solid black; }"
               "</style>"
               "</head><body>"
               newline
               '<h2><a href="/cgi-bin/view-guest-book.cgi">'
               'View Guest Book</a> | <a href="/sign-guest-book.html">'
               "Sign Guest Book</a></h2>"
               newline "<p><b>"
               'Written in GnuCOBOL by Erik Eriksen'
               newline "</b></p><hr />"
           end-display

           perform process-new-entry

           display
               '<a href="'
               'https://github.com/shamrice/COBOL-Guest-Book-Webapp">'
               "COBOL Source Code</a>"
               "</body></html>"
           end-display

           goback.


       process-new-entry.
           accept ws-http-value-string
               from environment ws-http-request-method-header
           end-accept

      * Remove potentially bad strings
           inspect ws-http-value-string converting "<>&" to spaces

           if IS-POST then

               open input fd-web-input
                   if ws-input-status < 10 then
                       read fd-web-input end-read
                       if ws-input-status > 9 then
                           move spaces to f-chunk-of-post
                       end-if
                   end-if
               close fd-web-input

               *> Remove bad strings in form post contents.
               inspect f-chunk-of-post converting "<>&" to spaces

               move function
                   get-param-value(f-chunk-of-post, "answer")
                   to ws-temp

               if trim(ws-temp) not = "20" then
                   display
                       '<h2 style="color:red;">Wrong answer. '
                       "Not saving entry. : " ws-temp "</h2>"
                   end-display
                   exit paragraph
               else
                   move function
                       get-param-value(f-chunk-of-post, "name")
                       to ws-guest-name
                   move function
                       get-param-value(f-chunk-of-post, "email")
                       to ws-guest-email
                   move function
                       get-param-value(f-chunk-of-post, "comment")
                       to ws-guest-comment

                   if function trim(ws-guest-name) = spaces then
                       move "Anonymous" to ws-guest-name
                   end-if

                   if function trim(ws-guest-comment) = spaces then
                       display
                           '<h2 style="color:red;">Comment field '
                           "cannot be blank. Please try again.</h2>"
                       end-display
                       exit paragraph
                   end-if

                   move function
                   html-decode(ws-guest-name) to ws-guest-name
                   move function
                   html-decode(ws-guest-email) to ws-guest-email
                   move function
                   html-decode(ws-guest-comment) to ws-guest-comment

                   perform insert-into-database

                   display
                       '<h2 style="text-align:center;">'
                       "Thank you for signing the guest book!</h2>"
                   end-display
               end-if
           end-if

           exit paragraph.


       insert-into-database.
      *> TODO : This should be read from a config file instead of hardcoded.
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

           EXEC SQL
               INSERT INTO GUEST_ENTRY(
                   GUEST_NAME, GUEST_EMAIL, GUEST_COMMENT)
               VALUES (
                   :ws-guest-name, :ws-guest-email, :ws-guest-comment);
           END-EXEC

           EXEC SQL
               CONNECT RESET
           END-EXEC.
           perform sqlstate-check
           exit paragraph.


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

       end program sign-guest-book.
