docker_build:
	docker build -t box .

docker_bash:
	docker run -it -P box bash

docker_notebook:
	docker run --name notebook -P box