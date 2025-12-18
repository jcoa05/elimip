# ELIM-IP Dashboard

**Exploring Levers to Increase Mosquito-borne Illness Prevention**

Interactive Shiny dashboard for visualizing Knowledge, Attitudes, and Practices (KAP) survey data and Discrete Choice Experiment (DCE) results on mosquito-borne disease prevention in metropolitan France.

![R](https://img.shields.io/badge/R-4.4.0-blue)
![Shiny](https://img.shields.io/badge/Shiny-1.8.0-green)
![License](https://img.shields.io/badge/License-MIT-yellow)

## Overview

This dashboard presents results from the largest quasi-representative survey on mosquito-borne disease knowledge, attitudes, practices, and preferences in metropolitan France. It supports evidence-based decision-making for public health interventions targeting *Aedes albopictus* mosquito control and arboviral disease prevention.

## Features

- **Interactive KAP visualization** across French regions and departments
- **Geographic mapping** with choropleth displays
- **Demographic breakdowns** by age, gender, education, and urbanization
- **DCE preference analysis** showing public preferences for mosquito control programs
- **Data export** functionality for further analysis
- **Responsive design** with expandable plots and professional aesthetics

## Quick Start

### Option 1: Docker (Recommended)

```bash
# Clone the repository
git clone https://github.com/jcoa05/elimip.git
cd elimip

# Build and run with Docker Compose
docker-compose up -d

# Access the dashboard
open http://localhost:3838
```

### Option 2: Local R Installation

```bash
# Clone the repository
git clone https://github.com/jcoa05/elimip.git
cd elimip

# Install dependencies (in R)
Rscript -e "install.packages(c('shiny', 'bslib', 'bsicons', 'leaflet', 'plotly', 'dplyr', 'tidyr', 'ggplot2', 'scales', 'scico', 'forcats', 'stringr', 'sf', 'DT'))"

# Prepare data (if not already done)
Rscript R/00_prepare_app_data.R

# Run the app
Rscript -e "shiny::runApp('app.R', port = 3838)"
```

## Project Structure

```
elimip/
├── app.R                      # Main Shiny application
├── Dockerfile                 # Docker container definition
├── docker-compose.yml         # Docker Compose configuration
├── .dockerignore              # Docker build exclusions
├── .gitignore                 # Git exclusions
├── README.md                  # This file
├── R/
│   └── 00_prepare_app_data.R  # Data preparation script
└── data/
    ├── raw/
    │   └── elimip-shiny.csv   # Raw survey data
    ├── geo/
    │   └── departements.geojson  # French department boundaries
    └── app_data.rds           # Prepared app data (generated)
```

## Data Preparation

Before running the app for the first time, you need to prepare the data:

```bash
# Ensure raw data is in place
ls data/raw/elimip-shiny.csv

# Run data preparation
Rscript R/00_prepare_app_data.R
```

This creates `data/app_data.rds` containing:
- Individual-level survey responses
- Department-level aggregations with inverse-variance weights
- Regional summaries
- DCE coefficient summaries
- Geographic boundaries

## Docker Commands

```bash
# Build the image
docker build -t elimip-dashboard .

# Run container
docker run -d -p 3838:3838 --name elimip elimip-dashboard

# View logs
docker logs -f elimip

# Stop container
docker stop elimip

# Remove container
docker rm elimip

# Using Docker Compose
docker-compose up -d      # Start
docker-compose down       # Stop
docker-compose logs -f    # View logs
docker-compose build      # Rebuild
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `SHINY_LOG_LEVEL` | `INFO` | Logging verbosity |

## Citation

If you use this dashboard or the underlying data, please cite:

> Ocampo, J.C., Mueller, J.E. & Dussault, J. Mosquito-borne disease knowledge, attitudes, and practices amongst the general population in metropolitan France. *Discov Public Health* 22, 545 (2025). https://doi.org/10.1186/s12982-025-00934-7

## Funding

This project was funded by an Institut Pasteur internal seed grant.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## Contact

For questions or collaboration inquiries, please open an issue on GitHub.
