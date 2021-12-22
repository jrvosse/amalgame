IMAGE=jrvosse/amalgame
CONTAINER=amalgame
# ARCH=amd64
ARCH=arm64v8
PORT=3020
PUBLISH=--publish=${PORT}:3020
VOLUME=$(shell pwd)
MOUNT=-v ${VOLUME}:/opt/project # mount host volume to /opt/project so RDF-store and other settings are persistent
# RUN_AS_ME=-v /etc/group:/etc/group:ro -v /etc/passwd:/etc/passwd:ro -u $(shell id -u ${USER} ):$(shell id -g ${USER} )

DOPTS=${PUBLISH} ${MOUNT} ${RUN_AS_ME} --name=${CONTAINER}

all:
	@echo "Targets"
	@echo
	@echo "image            Build the ${IMAGE} image"
	@echo "daemon           Run the image (detached)"
	@echo "run              Run the image interactively"
	@echo "manifest         Build and push a multiarch manifest for ${IMAGE}"

image::
	docker build -t ${IMAGE}:${ARCH} .

daemon:
	docker run --detach ${DOPTS} ${IMAGE}

run:
	docker run -it ${DOPTS} ${IMAGE} run

stop:
	docker stop -t2 ${CONTAINER}
	docker rm       ${CONTAINER}

manifest:
	# docker manifest rm ${IMAGE}
	docker manifest create ${IMAGE}:latest ${IMAGE}:arm64v8 --amend ${IMAGE}:amd64
	docker manifest push --purge ${IMAGE}
