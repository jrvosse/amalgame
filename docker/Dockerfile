FROM jrvosse/cliopatria

RUN apt-get update

WORKDIR $CLIOPATRIA_DIR
RUN ./configure --with-localhost
RUN ./run.pl cpack install amalgame

WORKDIR $PROJECT_DIR
