# dockerfile for dev environment
FROM ubuntu:20.04

USER root
RUN apt-get update 
RUN apt-get install -y curl 
RUN apt-get install -y sudo
RUN apt-get install -y git
RUN apt install -y tmux
RUN apt install -y vim

WORKDIR /root/

RUN curl -sSL https://get.haskellstack.org/ | sh


RUN git clone https://github.com/aryzach/dotfiles.git
RUN /root/dotfiles/setup.sh


ADD . .

# docker build -t ssh-img .
# docker run -p 2024:2024 -d ssh -it ssh-img /bin/bash
# docker start -i -a ssh
