FROM fpco/stack-build:lts-14.27 as build

RUN mkdir /opt/build

# Copy source.
COPY ./ /opt/build

# Replace dependency location and install.
RUN cd /opt/build \
    && rm -rf .stack-work \
    && stack build --system-ghc

RUN mkdir -p /opt/birch-beer
ARG BINARY_PATH
WORKDIR /opt/birch-beer
RUN apt-get update && apt-get install -y --no-install-recommends --allow-unauthenticated \
        apt-transport-https \
        software-properties-common \
        ca-certificates \
        locales \
        libgmp-dev \
        build-essential \
        libblas-dev \
        liblapack-dev \
        libgsl-dev \
        libgtk2.0-dev \
        libcairo2-dev \
        libpango1.0-dev \
        graphviz \
        r-base \
        r-base-dev

# Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN R -e "install.packages(c('devtools', 'ggplot2', 'jsonlite'), repos='http://cran.us.r-project.org/')" \
    && R -e "devtools::install_version('cowplot', version='0.9.2', repos='http://cran.us.r-project.org/')" \
    && R -e "devtools::install_version('locfit', version='1.5-9.1', repos='http://cran.us.r-project.org/')" \
    && Rscript -e "source('http://bioconductor.org/biocLite.R')" -e "biocLite('edgeR')"

RUN cd /opt/build \
    && cp "$(stack exec -- which birch-beer)" /opt/birch-beer/ \
    && cd /opt/birch-beer

ENTRYPOINT ["/opt/birch-beer/birch-beer"]
