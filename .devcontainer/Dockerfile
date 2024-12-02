FROM rocker/r-ver:4.4.2

# Install radian dependencies
RUN apt-get update -qq && apt-get install -y \
  git \
  pipx \
  python3-dev \
  wget && \
  pipx ensurepath && \
  pipx install radian

# Install R package dependencies
RUN apt-get install -y \
  libcurl4-openssl-dev \
  libfreetype6-dev \
  libfribidi-dev \
  libfontconfig1-dev \
  libgit2-dev \
  libglpk-dev \
  libharfbuzz-dev \ 
  libjpeg-dev \
  libpng-dev\
  libssl-dev \
  libtiff5-dev \
  libxml2-dev

WORKDIR /screentime_biomarkers

# Install tex
ENV CTAN_REPO="https://mirror.aarnet.edu.au/pub/CTAN/systems/texlive/tlnet"
ENV PATH="$PATH:/usr/local/texlive/bin/linux"
RUN /rocker_scripts/install_pandoc.sh
RUN /rocker_scripts/install_texlive.sh

# These are all the latex packages that sometimes aren't installed
RUN tlmgr install academicons booktabs colortbl enumitem environ euenc \
  fancyhdr fontawesome fontspec fp ifmtarg l3packages latex-amsmath-dev pgf \
  ragged2e setspace sourcesanspro tabu tcolorbox tipa trimspaces unicode-math \
  varwidth xifthen xunicode

# Install renv
ENV RENV_VERSION=1.0.7
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" && \
  R -e "remotes::install_github('rstudio/renv@v${RENV_VERSION}')" 

# # Setup renv
COPY ../renv.lock renv.lock
RUN R -e "renv::restore()"
# # Install dev requirements that are seperate from the project
# RUN R -e "renv::install()"