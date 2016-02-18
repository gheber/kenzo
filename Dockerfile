FROM sergiobuj/cl-jupyter
MAINTAINER gheber <gheber@hdfgroup.org>

RUN apt-get update
RUN apt-get -y install wget
RUN wget https://github.com/gheber/kenzo/archive/master.zip   
RUN unzip master.zip -d /root/quicklisp/local-projects && rm master.zip
RUN mv /root/quicklisp/local-projects/kenzo-master /root/quicklisp/local-projects/kenzo 
RUN cp /root/quicklisp/local-projects/kenzo/examples/*.ipynb /notebooks/
RUN cp /root/quicklisp/local-projects/kenzo/examples/*.png /notebooks/
RUN chmod -R 0444 /notebooks

# Install Kenzo
RUN sbcl --eval "(ql:quickload :kenzo)"
