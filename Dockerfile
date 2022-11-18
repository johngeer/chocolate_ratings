FROM rocker/tidyverse:4.2.2

RUN apt-get update && apt-get install -y fish ranger

# RUN useradd -ms /bin/bash rocker
USER rstudio
WORKDIR /home/rstudio

CMD /usr/bin/fish
