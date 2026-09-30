## ===========================================================================================#
# Pharmaceutical industry: replication orchestrator
# Runs the full pipeline, in order, from a fresh R session.
# Run from the project root:  source("_run-all.R")
# Outputs: figures-included/ and tables-included/
## ===========================================================================================#

rm(list = ls())

run_step <- function(script) {
  cat("\n>>> Running", script, "\n")
  source(script)
}

## ==================================  Setup  ================================================#

# helper functions, packages, plot theme
run_step("00-setup.R")

## ==================================  Data  =================================================#

# Compustat pharma sample, FDA drug data, patent data (KPSS), matching
# NOTE: 01 sources 02-data-cleaning.R internally (patent data), so 02 is not run separately
run_step("01-ingest-data.R")

# estimation samples (sample_top50, etc.)
run_step("03-sample-selection.R")

## ==================================  Figure 1  =============================================#

# NFC vs. pharma financials
run_step("04-figure1-financials.R")

## ==================================  Figure 2  =============================================#

# firm-year aggregates of drugs and patents
run_step("05-master-data.R")

# drugs and patents over time
run_step("06-figure2-drugs-patents.R")

## ==================================  Figure 3  =============================================#

# profitability vs. innovation scatterplots
run_step("07-figure3-scatterplots.R")

## ==================================  Table 1  ==============================================#

# descriptive statistics
run_step("08-table1-descriptive-table.R")

## ==================================  Table 2  ==============================================#

# baseline regressions
run_step("09-table2-baseline-regression.R")

## ==================================  Appendix  =============================================#

# Figure S1: supplementary scatterplots
run_step("10-appendix-figureS1.R")

# Table S1: supplementary regressions
run_step("11-appendix-tableS1.R")

# Table S2: supplementary regressions
run_step("12-appendix-tableS2.R")

# Table S3: supplementary regressions
run_step("13-appendix-tableS3.R")

cat("\n>>> Done.\n")
