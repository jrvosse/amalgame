FROM jrvosse/cliopatria

WORKDIR $CLIOPATRIA_DIR
RUN ./configure --with-localhost --with-rdfpath
ENV AMALGAME_VERSION V1.0.0-102-g93809ba
RUN ./run.pl cpack install amalgame

WORKDIR $PROJECT_DIR
