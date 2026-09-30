# Profitability and Drug Discovery

Replication files for Işık and Orhangazi (2022), "Profitability and drug discovery," *Industrial and Corporate Change* ([doi:10.1093/icc/dtac011](https://doi.org/10.1093/icc/dtac011)). The code merges Compustat financials, PatentsView patents and FDA drug approvals for large publicly listed pharmaceutical firms, 1980-2018, and tests whether past profitability predicts R&D spending and new drug approvals.

Running the code reproduces Figures 1-3 and Tables 2-3 of the paper, and the supplementary Figure S1 and Tables S1-S3. Figures are saved as PDF in [figures-included/](figures-included/), and Table 2 as CSV in [tables-included/](tables-included/). Table 3 is printed to the console as LaTeX. The supplementary figure and tables are produced in the R session but not saved to file. Table 1 lists variable definitions and has no code.

## Requirements

R. The code was written under R 3.6.3. On the first run, any missing packages are installed from CRAN, so there is no need to install them manually. These will be the current CRAN versions, not the ones used for the paper.

## Data

Things you will need:

1. Compustat (not provided). Annual fundamentals from S&P Compustat, available through WRDS under a subscription, which we cannot share. Place two extracts in [raw-input-data/](raw-input-data/): `pharma_compustat.dta`, the firms with SIC codes 2834, 2835 and 2836, and `compustat_2_11_20.dta`, all firms, used for the comparison with nonfinancial corporations in Figure 1.
2. Patent data (not provided). These files are too large for GitHub. Place them in [raw-input-data/](raw-input-data/):
   - `clean_patent_data.csv`: patents granted by the USPTO, from PatentsView, with assignees matched to Compustat gvkeys using Kogan et al. (2017) and the Global Corporate Patent Dataset of Bena et al. (2017).
   - `patentview_nber.tsv`: the NBER technology category of each patent, from PatentsView.
   - `KPSS_2020_public.csv`: patent values and citations from Kogan, Papanikolaou, Seru and Stoffman (2017), 2020 public release.
3. FDA and lookup data (provided). [raw-input-data/FDA_drug_data/](raw-input-data/FDA_drug_data/) contains the Drugs@FDA files and our hand match of drug sponsors to Compustat gvkeys. [raw-input-data/](raw-input-data/) also has the SIC-to-gvkey crosswalk and the PatentsView NBER category and subcategory lookups.

## Running the code

From the project folder, in R:

```r
source("_run-all.R")
```

This will clear the R session and run all the scripts in order, printing the name of each script as it starts.

You can also run the scripts individually: run [00-setup.R](00-setup.R) first, then the numbered scripts in order, in the same R session. [01-ingest-data.R](01-ingest-data.R) runs [02-data-cleaning.R](02-data-cleaning.R) itself, so skip 02.

## What each script does

The script names follow the table numbering of the submitted manuscript. In the published paper, a table of variable definitions was added as Table 1, so the descriptive table became Table 2 and the regression table Table 3. The Output column uses the published numbering.

| Script | Content | Output |
|---|---|---|
| [00-setup.R](00-setup.R) | Packages, name-cleaning and helper functions, plot theme | |
| [01-ingest-data.R](01-ingest-data.R) | Compustat pharma sample; FDA approvals matched to firms and classified as ND1 or ND2; patent counts, citations and values; flags for drug producers, top-50 firms and firms present in all years | |
| [02-data-cleaning.R](02-data-cleaning.R) | Patents of SIC 2834-2836 firms, with NBER categories; run from 01 | |
| [03-sample-selection.R](03-sample-selection.R) | Estimation samples (top-50 drug producers, and those observed at least 10 and 20 years); profitability, moving averages, drug and patent totals | |
| [04-figure1-financials.R](04-figure1-financials.R) | Markup, profit rate, shareholder payments and R&D: pharma vs. other nonfinancial firms | Figure 1 |
| [05-master-data.R](05-master-data.R) | Yearly totals of drugs, patents and R&D for the top-50 firms | |
| [06-figure2-drugs-patents.R](06-figure2-drugs-patents.R) | Patents, drug approvals and R&D productivity over time | Figure 2 |
| [07-figure3-scatterplots.R](07-figure3-scatterplots.R) | Past profitability against R&D and new drugs | Figure 3 |
| [08-table1-descriptive-table.R](08-table1-descriptive-table.R) | Profitability, R&D and new drugs by decade | Table 2 |
| [09-table2-baseline-regression.R](09-table2-baseline-regression.R) | Baseline regressions of R&D and new drugs on past profitability | Table 3 |
| [10-appendix-figureS1.R](10-appendix-figureS1.R) | Supplementary scatterplots | Figure S1 |
| [11-appendix-tableS1.R](11-appendix-tableS1.R) | Supplementary regressions | Table S1 |
| [12-appendix-tableS2.R](12-appendix-tableS2.R) | Supplementary regressions | Table S2 |
| [13-appendix-tableS3.R](13-appendix-tableS3.R) | Supplementary regressions | Table S3 |

## Version history

1. Original code: [66959b0](https://github.com/enesn/2022-drug-discovery/commit/66959b0ba8fa6470b765127dd50583ee8814a5d6). This is the code as I wrote it for the submission to *Industrial and Corporate Change*.
2. Reproducibility revision (branch `cosmetic-for-better-reproduction`). I reworked the original code with an AI coding assistant to make it easier to reproduce. The changes number the scripts in running order, rename the data and output folders, add a one-command pipeline and install missing packages automatically.
