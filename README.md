# COBOL Guest Book WebApp
A simple website guest book demonstration written in GnuCOBOL

## How to build
Building the project can be done by running the "build_and_deploy.sh" shell
script itn he sources directory.

The following variables can be set:
* DEPLOY_CODE - set to true to copy compiled COBOL source files and HTML documents to local web server directories.
* HTML_DEST_DIR - Location to copy HTML files to in file system. (Default: /var/www/html)
* CGI_BIN_DEST_DIR - Location to copy the compiled COBOL files to. (Default: /usr/lib/cgi-bin)

## Screen shots

### View guest book entries:
![Screen shot of view guest book page](https://i.imgur.com/jELUInT.png)

### Sign guest book:
![Screen shot of sign guest book page](https://i.imgur.com/jVeQdD5.png)
