# Use phusion/baseimage as base image. To make your builds
# reproducible, make sure you lock down to a specific version, not
# to `latest`! See
# https://github.com/phusion/baseimage-docker/blob/master/Changelog.md
# for a list of version numbers.
FROM phusion/baseimage:0.9.18

# Use baseimage-docker's init system.
CMD ["/sbin/my_init"]

RUN apt-get update -qq && \ 
DEBIAN_FRONTEND=noninteractive apt-get install -yq --no-install-recommends \ 
build-essential \ 
ca-certificates \ 
curl \ 
git \ 
wget \ 
language-pack-en \ 
libcurl4-openssl-dev \ 
libffi-dev \ 
libsqlite3-dev \ 
libzmq3-dev \ 
pandoc \ 
python-dev \ 
python3-dev \ 
sqlite3 \ 
texlive-fonts-recommended \ 
texlive-latex-base \ 
texlive-latex-extra \ 
zlib1g-dev 

RUN useradd -ms /bin/bash user
USER user
WORKDIR /home/user

RUN wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
RUN bash Miniconda3-latest-Linux-x86_64.sh -b
RUN echo 'export PATH=~/miniconda3/bin:$PATH' >> ~/.bashrc
ENV PATH /home/user/miniconda3/bin:$PATH
RUN conda install numpy scipy pandas matplotlib seaborn dill statsmodels scikit-learn jupyter notebook
RUN git clone https://github.com/cfriedline/gypsy_moth.git

# Clean up APT when done.
USER root
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

EXPOSE 8888

RUN mkdir /etc/service/notebook
ADD docker_notebook.sh /etc/service/notebook/run
