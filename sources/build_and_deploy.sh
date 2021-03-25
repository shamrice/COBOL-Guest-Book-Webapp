#!/bin/bash

DEPLOY_CODE=true

HTML_DEST_DIR=/var/www/html
CGI_BIN_DEST_DIR=/usr/lib/cgi-bin
BUILD_DATE=`date`

echo
echo "*****************************************************************"
echo "*                                                               *"
echo "* Building guest book code base                                 *"
echo "*                                                               *"
echo "*****************************************************************"
echo "Build date: " $BUILD_DATE
echo
mkdir -p -v ./generated_sources

echo
echo "Running esqlOC precompiler..."
echo "-----------------------------"

# ESQL: https://sourceforge.net/p/gnucobol/contrib/HEAD/tree/trunk/esql/
# Relies on unixodbc package & postgres odbc library being installed.
# Can only use fixed format!
esqlOC -static -o ./generated_sources/view-guest-book.cob view-guest-book.cbl
esqlOC -static -o ./generated_sources/sign-guest-book.cob sign-guest-book.cbl

echo
echo "Building source files..."
echo "-------------------------"
cobc -v -x -std=default -Wall -debug -static -locsql -o ./bin/view-guest-book.cgi ./generated_sources/view-guest-book.cob
echo
echo
cobc -v -x -std=default -Wall -debug -static -locsql -o ./bin/sign-guest-book.cgi ./generated_sources/sign-guest-book.cob web-helpers.cbl
echo

if [ $DEPLOY_CODE == 'true' ]
then
    echo "*****************************************************************"
    echo "*                                                               *"
    echo "* Deploying to local web server directories                     *"
    echo "*                                                               *"
    echo "*****************************************************************"
    sudo cp -v ./bin/*.cgi $CGI_BIN_DEST_DIR
    sudo cp -v ./html/*.html $HTML_DEST_DIR
else
    echo "Skipping deploy process..."
fi

echo
echo "Done."
echo
