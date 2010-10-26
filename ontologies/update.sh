#!/bin/sh

curl -o opmv.ttl  -D opmv.headers  http://open-biomed.sourceforge.net/opmv/opmv.owl
curl -o opmvc.rdf -D opmvc.headers http://opmv.googlecode.com/svn/trunk/schema/types/common.rdf

