#!/bin/bash
VERSION=arm64v8
APP=amalgame
REPO=jrvosse
SUDO=
$SUDO docker build --tag $REPO/$APP:$VERSION  .
