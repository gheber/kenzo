FROM sergiobuj/cl-jupyter
MAINTAINER gheber <gheber@hdfgroup.org>

RUN apt-get update
RUN apt-get -y install wget
RUN wget https://github.com/gheber/kenzo/archive/master.zip   
RUN unzip master.zip -d /root/quicklisp/local-projects && rm master.zip
RUN mv /root/quicklisp/local-projects/kenzo-master /root/quicklisp/local-projects/kenzo 
RUN cp /root/quicklisp/local-projects/kenzo/examples/*.ipynb /notebooks/
RUN cp /root/quicklisp/local-projects/kenzo/examples/*.png /notebooks/

# Install Kenzo
RUN sbcl --eval "(ql:quickload :kenzo)"

# Add Tini. Tini operates as a process subreaper for jupyter. This prevents
# kernel crashes.
ENV TINI_VERSION v0.6.0
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /usr/bin/tini
RUN chmod +x /usr/bin/tini
ENTRYPOINT ["/usr/bin/tini", "--"]

EXPOSE 8888
CMD ["jupyter", "notebook", "--port=8888", "--no-browser", "--ip=0.0.0.0"]
