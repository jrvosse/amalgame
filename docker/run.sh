#!/bin/bash
VERSION=latest
APP=amalgame
REPO=jrvosse
HPORT=3020 # Port on host computer
CPORT=3020 # Port on docker container running ClioPatria
RUN=${1-./run.pl}
sudo docker run \
	-v $PWD:/opt/project \
	-v /etc/group:/etc/group:ro \
	-v /etc/passwd:/etc/passwd:ro \
	-u $( id -u $USER ):$( id -g $USER ) \
	-p $HPORT:$CPORT -it  \
	$REPO/$APP:$VERSION $RUN
