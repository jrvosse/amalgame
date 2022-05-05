FROM jrvosse/cliopatria

WORKDIR $CLIOPATRIA_DIR
RUN ./configure --with-localhost --with-rdfpath
ENV AMALGAME_VERSION V1.0.0-109-gde5f27a
RUN ./run.pl cpack install amalgame

WORKDIR $PROJECT_DIR
