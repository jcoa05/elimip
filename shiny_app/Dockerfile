# =============================================================================
# ELIM-IP Dashboard Dockerfile
# =============================================================================
# Multi-stage build for optimized R Shiny application
# =============================================================================

FROM rocker/shiny:4.4.0

LABEL maintainer="Juan Carlos Ocampo <jcoa05@github>"
LABEL description="ELIM-IP: Exploring Levers to Increase Mosquito-borne Illness Prevention Dashboard"
LABEL version="2.0"

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'bslib', \
    'bsicons', \
    'leaflet', \
    'plotly', \
    'dplyr', \
    'tidyr', \
    'ggplot2', \
    'scales', \
    'scico', \
    'forcats', \
    'stringr', \
    'sf', \
    'DT', \
    'htmltools' \
), repos='https://cloud.r-project.org/', Ncpus=4)"

# Create app directory
RUN mkdir -p /srv/shiny-server/elimip

# Copy application files
COPY app.R /srv/shiny-server/elimip/
COPY R/ /srv/shiny-server/elimip/R/
COPY data/ /srv/shiny-server/elimip/data/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/elimip

# Configure shiny-server
RUN echo "run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    site_dir /srv/shiny-server/elimip;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}" > /etc/shiny-server/shiny-server.conf

# Expose port
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Run shiny-server
CMD ["/usr/bin/shiny-server"]
