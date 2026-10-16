FROM jrvosse/cliopatria

WORKDIR $CLIOPATRIA_DIR
RUN ./configure --with-localhost --with-rdfpath
ENV AMALGAME_VERSION V1.1.0

RUN ./run.pl cpack install amalgame

WORKDIR $PROJECT_DIR
