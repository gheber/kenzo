# original Dockerfile from https://github.com/melhadad/cl-jupyter-docker

FROM python:3.6

ENV HOME /root

# -----------------------
# zeromq with libsodium for encrypted communication
# https://github.com/ogomezm/zeromq-container
# MAINTAINER Oscar Gómez

RUN apt-get update && apt-get install -y git build-essential libtool autoconf automake pkg-config unzip libkrb5-dev curl wget pandoc texlive-generic-recommended texlive-xetex
RUN cd /tmp && git clone git://github.com/jedisct1/libsodium.git && cd libsodium && git checkout e2a30a && ./autogen.sh && ./configure && make check && make install && ldconfig
RUN cd /tmp && git clone --depth 1 git://github.com/zeromq/libzmq.git && cd libzmq && ./autogen.sh && ./configure && make
# RUN cd /tmp/libzmq && make check

RUN cd /tmp/libzmq && make install && ldconfig
RUN rm /tmp/* -rf

# -----------------------
# sbcl with quicklist
RUN wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.0-x86-64-linux-binary.tar.bz2 -O /tmp/sbcl.tar.bz2 && \
    mkdir /tmp/sbcl && \
    tar jxvf /tmp/sbcl.tar.bz2 --strip-components=1 -C /tmp/sbcl/ && \
    cd /tmp/sbcl && \
    sh install.sh && \
    cd /tmp \
    rm -rf /tmp/sbcl/
	
WORKDIR /tmp/
RUN wget http://beta.quicklisp.org/quicklisp.lisp

RUN sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(setq  ql-util::*do-not-prompt* t)" --eval "(ql:add-to-init-file)"

RUN sbcl --non-interactive --eval '(ql:quickload "alexandria")' --eval '(ql:quickload "trivial-features")' --eval '(ql:quickload "babel")' --eval '(ql:quickload "kenzo")'

# -----------------------
# cl-jupyter
RUN pip3 install ipython
RUN pip3 install jupyter

WORKDIR /root
RUN cd /root && git clone https://github.com/fredokun/cl-jupyter.git && cd cl-jupyter && python3 ./install-cl-jupyter.py && sbcl --load ./cl-jupyter.lisp

EXPOSE 8888
CMD jupyter notebook --allow-root --no-browser --NotebookApp.token='' --ip '*' --port 8888
