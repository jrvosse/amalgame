#!/bin/bash
#
# Run amalgame cointainer by mounting the current working directory ($PWD) as the project directory.
# The default settings run:
#   - the processes in the countainer under the id and group of the current user
#   - map host port 3020 on container port 3020

VERSION=latest
APP=amalgame
REPO=jrvosse
HPORT=3020 # Port exposed on host computer
CPORT=3020 # Port used on docker container running ClioPatria/amalgame
SUDO=
$SUDO docker run \
	-v $PWD:/opt/project \
	-v /etc/group:/etc/group:ro \
	-v /etc/passwd:/etc/passwd:ro \
	-u $( id -u $USER ):$( id -g $USER ) \
	-p $HPORT:$CPORT -it  \
	$REPO/$APP:$VERSION $@
