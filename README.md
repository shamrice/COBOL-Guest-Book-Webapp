# COBOL Guest Book WebApp
A simple website guest book demonstration written in GnuCOBOL

*Note:* This project was created for the sole purpose of demonstrating a way to use GnuCOBOL to
create a web application that interacts with a database. As with any internet
facing web site, there are security risks. This code makes almost no
attempt to mitigate these risks and care should be taken before using the source
code in this repo in a live environment.

## Requirements to build & deploy
The following are needed in order to build and deploy the project:
* GnuCOBOL - https://sourceforge.net/projects/gnucobol/
* esqlOC Precompiler - https://sourceforge.net/p/gnucobol/contrib/HEAD/tree/trunk/esql/
* unixODBC - http://www.unixodbc.org/
* PostgreSQL Database - https://www.postgresql.org/
* odbc-postgresql - Postgres ODBC driver
* Apache2 web server - https://httpd.apache.org/

If you are using Ubuntu, all of the above are available in the apt package manager
with the exception of esqlOC, which must be downloaded and built manually.

## How to build
Building the project can be done by running the "build_and_deploy.sh" shell
script itn he sources directory.

The following variables can be set:
* DEPLOY_CODE - set to true to copy compiled COBOL source files and HTML documents to local web server directories after compilation finishes.
* HTML_DEST_DIR - Location to copy HTML files to in file system. (Default: /var/www/html)
* CGI_BIN_DEST_DIR - Location to copy the compiled COBOL files to. (Default: /usr/lib/cgi-bin)


## Live Demo
Below is a link to the a live demo of this software running. No guarantee on any uptime for this test server. It may be taken down at any time.
[Live Demo Link](http://eriksguestbook.servehttp.com:9574/cgi-bin/view-guest-book.cgi)


## Screen shots

### View guest book entries:
![Screen shot of view guest book page](https://i.imgur.com/jELUInT.png)

### Sign guest book:
![Screen shot of sign guest book page](https://i.imgur.com/jVeQdD5.png)
