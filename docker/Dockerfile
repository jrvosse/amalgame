FROM jrvosse/cliopatria

RUN apt-get update

ENV CLIOPATRIA_DIR /opt/ClioPatria
ENV PROJECT_DIR /opt/project

WORKDIR $CLIOPATRIA_DIR
RUN /opt/ClioPatria/configure --with-localhost
RUN ./run.pl cpack install amalgame

WORKDIR $PROJECT_DIR

CMD /opt/ClioPatria/configure --with-localhost
