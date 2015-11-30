FROM sergiobuj/cl-jupyter
MAINTAINER gheber <gheber@hdfgroup.org>

RUN apt-get update

# Update Quicklisp and install Kenzo
RUN sbcl --eval "(ql:update-client)"
RUN sbcl --eval "(ql:update-all-dists)"
RUN sbcl --eval "(ql:quickload :kenzo)"

RUN cp /root/quicklisp/dists/quicklisp/software/kenzo-*-git/examples/*.ipynb /notebooks/
