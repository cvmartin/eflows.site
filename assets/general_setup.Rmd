---
title: "general setup"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# Objective: to setup a droplet to work with R, from the ground up

## Up and running

1. Do a droplet, connect with Putty

2. Add a user with sudo privileges and not needing a password

```
adduser ubuntu
# you will be asked for new passwords etc.

sudo visudo
# at the very end of the file
ubuntu ALL=(ALL) NOPASSWD: ALL
```

3. Install R

```
sudo apt-get update
sudo apt-get install r-base
```

4. Install Rstudio server

follow: `https://www.rstudio.com/products/rstudio/download-server/`

5. Install Rstudio shiny
Previously, install shiny and rmarkdown

```{bash}
sudo su - \-c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
sudo su - \-c "R -e \"install.packages('rmarkdown', repos='https://cran.rstudio.com/')\""
```

follow: `https://www.rstudio.com/products/shiny/download-server/`

6. Install some package dependencies

```{bash}
sudo apt-get install -y libsasl2-dev
sudo apt-get install -y libcurl4-openssl-dev
sudo apt-get install -y libssl-dev
sudo apt-get install -y libxml2-dev
```



7. Install truckload of packages

```{bash}
sudo su - \-c "R -e \"install.packages('caTools', repos='https://cran.rstudio.com/')\""
sudo su - \-c "R -e \"install.packages('bitops', repos='https://cran.rstudio.com/')\""
sudo su - \-c "R -e \"install.packages('devtools', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tidyverse', repos='http://cran.rstudio.com/')\""

sudo su - \-c "R -e \"install.packages('jsonlite', repos='https://cran.rstudio.com/')\""
sudo su - \-c "R -e \"install.packages('mongolite', repos='https://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('xts', repos='http://cran.rstudio.com/')\""
sudo su - \-c "R -e \"install.packages('dygraphs', repos='https://cran.rstudio.com/')\""

sudo su - \-c "R -e \"install.packages('httr', repos='https://cran.rstudio.com/')\""
sudo su - \-c "R -e \"install.packages('plumber', repos='https://cran.rstudio.com/')\""

sudo su - \-c "R -e \"install.packages('shinyWidgets', repos='https://cran.rstudio.com/')\""
sudo su - \-c "R -e \"install.packages('shinydashboard', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinyjs', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('Rcpp', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('RcppArmadillo', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('RcppEigen', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('R6', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"devtools::install_github('cvmartin/eflows')\""
sudo su - -c "R -e \"devtools::install_github('cvmartin/eflows.viz')\""


```

