#!/bin/bash
VERSION=latest
APP=amalgame
REPO=jrvosse
SUDO=
$SUDO docker build --tag $REPO/$APP:$VERSION  .
