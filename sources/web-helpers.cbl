       >>source format is fixed
      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-03-23
      * Last Modified: 2021-03-25
      * Purpose: Returns the found value for a parameter in a key value
      *          map string like a query string or post values. Returns
      *          space when search parameter is not found.
      * Tectonics: ./build_and_deploy.sh
      ******************************************************************
       identification division.
       function-id. get-param-value.

       environment division.

       configuration section.

       data division.

       working-storage section.

       01  ws-max-keys                     constant as 100.

       local-storage section.

       01  ls-string-map                   occurs 0 to ws-max-keys times
                                           depending on ls-num-keys.
           05  ls-string-key               pic x(1024).
           05  ls-string-value             pic x(1024).

       01  ls-raw-key-value-strings        pic x(1024)
                                           occurs 0 to ws-max-keys times
                                           depending on ls-num-keys
                                           value spaces.

       01  ls-idx                          pic 9(5) comp-3.
       01  ls-starting-pointer             pic 9(5) comp-3.
       01  ls-num-keys                     pic 9(5) comp-3.

       linkage section.
       01  l-raw-map-string                pic x any length.
       01  l-param-search-key              pic x any length.
       01  l-found-value                   pic x(1024).

       procedure division
           using l-raw-map-string, l-param-search-key
           returning l-found-value.

           move 1 to ls-starting-pointer
           move space to l-found-value

           inspect function trim(l-raw-map-string)
           tallying ls-num-keys for all '='

           if ls-num-keys = 0 then
      *        Nothing to parse
               goback
           end-if

           perform varying ls-idx from 1 by 1 until ls-idx > ls-num-keys

               unstring l-raw-map-string delimited by space
                   into ls-raw-key-value-strings(ls-idx)
                   with pointer ls-starting-pointer
               end-unstring

           end-perform

           perform varying ls-idx from 1 by 1 until ls-idx > ls-num-keys

               if ls-raw-key-value-strings(ls-idx) not = spaces then
                   unstring ls-raw-key-value-strings(ls-idx)
                       delimited by '=' into
                       ls-string-key(ls-idx)
                       ls-string-value(ls-idx)
                   end-unstring

                   if function trim(ls-string-key(ls-idx)) =
                       function trim(l-param-search-key) then

      *                Value found, return with value.
                       move function trim(ls-string-value(ls-idx))
                       to l-found-value
                       goback

                   end-if
               end-if
           end-perform

           goback.

       end function get-param-value.




      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-03-24
      * Last Modified: 2021-03-24
      * Purpose: Converts html encoded characters with their original
      *          values. (Note: Not complete!)
      * Tectonics: ./build_and_deploy.sh
      ******************************************************************
       identification division.
       function-id. html-decode.

       environment division.

       configuration section.

       data division.

       working-storage section.

       01  ws-max-string-length           constant as 2048.

       local-storage section.

       linkage section.
       01  l-html-encoded-string          pic x any length.
       01  l-html-decoded-string          pic x(ws-max-string-length).


       procedure division
           using l-html-encoded-string
           returning l-html-decoded-string.

           move l-html-encoded-string to l-html-decoded-string

      *> NOTE: supported characters to convert are very limited.
           move function substitute(l-html-decoded-string, "+", space)
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%21", "!")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%27", "'")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%40", "@")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%7E", "~")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%23", "#")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%25", "%")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%5E", "^")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%26", "&")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%28", "(")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%29", ")")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%2B", "+")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%60", "`")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%3D", "=")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%5B", "[")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%5D", "]")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%5C", "\")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%7B", "{")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%7D", "}")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%7C", "|")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%3B", ";")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%3A", ":")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%22", '"')
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%2C", ",")
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%2F", "/")
           to l-html-decoded-string

      *>   dissallow < and > characters
           move function substitute(l-html-decoded-string, "%3C", space)
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%3E", space)
           to l-html-decoded-string

           move function substitute(l-html-decoded-string, "%3F", "?")
           to l-html-decoded-string

           move function
           substitute(l-html-decoded-string, "%0D%0A", "<br />")
           to l-html-decoded-string

           goback.

       end function html-decode.
