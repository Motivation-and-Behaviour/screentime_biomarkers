
# Longitudinal associations between early childhood screen time and health outcomes in late childhood

Analysis for the screen time and biomarkers project.

<!-- TODO: Add OSF Link -->

## Setup

To get started with this project, follow these steps:

1. Install the renv package, if you haven't already:

    ```r
    install.packages("renv")
    ```

2. Clone the repository to your local machine
3. Install the required packages using `renv`:

    ```r
    renv::restore()
    ```

### Running the Pipeline

There are two ways to run the full pipeline:

1. In an R session, run the pipeline with `targets::tar_make()`.
2. In the terminal, run `Rscript make.R`.
