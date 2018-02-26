FROM r-base

RUN apt-get update && apt-get install -y psmisc wget sudo openssl libssl-dev libcurl4-openssl-dev build-essential && rm -rf /var/lib/apt/lists/*

# Replace 1000 with your user / group id
RUN export uid=1000 gid=1000 && \
    mkdir -p /home/vagrant && \
    echo "vagrant:x:${uid}:${gid}:vagrant,,,:/home/vagrant:/bin/bash" >> /etc/passwd && \
    echo "vagrant:x:${uid}:" >> /etc/group && \
    adduser vagrant video && \
    chown ${uid}:${gid} -R /home/vagrant && \
    mkdir /data && chown ${uid}:${gid} /data

#RUN R -e 'install.packages(c("httr", "curl", "jsonlite"), repos = "http://cran.us.r-project.org")'
#RUN R -e 'install.packages(c("gpuR"), repos = "http://cran.us.r-project.org")'

RUN R -e 'install.packages(c("Rserve"), repos = "http://cran.us.r-project.org")'

USER vagrant
ENV HOME /home/vagrant
WORKDIR $HOME
#ENTRYPOINT ["R", "CMD", "Rserve"]

EXPOSE 6311
ENTRYPOINT R -e "Rserve::run.Rserve(remote=TRUE)"
