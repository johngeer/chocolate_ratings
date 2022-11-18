FROM rocker/tidyverse:4.2.2

RUN apt-get update && apt-get install -y fish ranger

RUN install2.r xgboost

USER rstudio
WORKDIR /home/rstudio

CMD /usr/bin/fish
