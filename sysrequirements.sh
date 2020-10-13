#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
Rscript -e 'install.packages("remotes")'
Rscript -e 'remotes::install_cran(c("tidyverse", "rlang", "here", "lubridate", "janitor", "haven"))'
