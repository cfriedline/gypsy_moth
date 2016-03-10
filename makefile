docker_build:
	docker build -t box .

docker_bash:
	docker run -it box bash

docker_notebook:
	docker run --name notebook -P box jupyter notebook --no-browser --ip=0.0.0.0 --debug