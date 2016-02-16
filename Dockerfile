FROM sergiobuj/cl-jupyter
MAINTAINER gheber <gheber@hdfgroup.org>

RUN apt-get update
RUN apt-get -y install wget
RUN wget https://github.com/gheber/kenzo/archive/master.zip   
RUN unzip master.zip -d /root/quicklisp/local-projects
RUN mv /root/quicklisp/local-projects/kenzo-master /root/quicklisp/local-projects/kenzo 
RUN cp /root/quicklisp/local-projects/kenzo/examples/*.ipynb /notebooks/
RUN cp /root/quicklisp/local-projects/kenzo/examples/*.png /notebooks/
RUN chmod 444 /notebooks
RUN chmod 444 /notebooks/*

# Update Quicklisp and install Kenzo
RUN sbcl --eval "(ql:quickload :kenzo)"
