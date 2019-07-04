FROM rocker/binder:3.6.0
LABEL maintainer='Mathew Ling'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}
RUN wget https://github.com/Lingtax/PMT-flu-2018/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-07-04/')); devtools::install_deps()"
