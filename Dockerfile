FROM elilillyco-lilly-docker.jfrog.io/centos:7

RUN yum -y install https://centos7.iuscommunity.org/ius-release.rpm
RUN yum -y install python36u which python36u-devel python36u-pip gcc gcc-c++ poppler-utils poppler-cpp-devel R

RUN useradd run_user

RUN mkdir /pipeline && mkdir /workdir

COPY ./pipeline /pipeline

RUN pip3.6 install -r /pipeline/requirements.txt

RUN pip3.6 install -r /pipeline/requirements1.txt

RUN pip3.6 install -r /pipeline/requirements2.txt

RUN pip3.6 install -r /pipeline/requirements3.txt

RUN pip3.6 install -r /pipeline/requirements4.txt

RUN chown -R run_user:run_user /pipeline/* /workdir

USER run_user

ENTRYPOINT ls /pipeline > /pipeline/output.txt;exit 0
