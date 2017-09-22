FROM jrvosse/cliopatria

WORKDIR $CLIOPATRIA_DIR
RUN ./configure --with-localhost --with-rdfpath
RUN ./run.pl cpack install amalgame

WORKDIR $PROJECT_DIR
