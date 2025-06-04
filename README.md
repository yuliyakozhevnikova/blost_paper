# BLOST: Bayesian Longitudinally Ordinal Sequential Trial Design for Evaluating Respiratory Disease Treatments

## **Overview**
The folder contains files to run the computing codes of the BLOST paper.

## **Description of files**
- **`scen_new.csv`**: Contains 14 scenarios with varying `pS` and `pE`.
- **`codes_BLOST/` & `codes_BLOST_BMS/`**: Each contains 14 R scripts for different scenarios (e.g. "brms_code_1.R" - is the code for the first scenario, which is indicated in `scen_new.csv`). By default, cohort size is 30, but it can be changed to 10 (or any number) inside R script `cohortsize = 30`. Each R script simulates the data corresponding to the chosen scenario and uses it to compute posterios using Hamiltonian Monte Carlo.
- **`tables_and_graphs.R`**: Analyzes `_dat.rds` output files and generates tables/graphs.

## **Requirements**
- R with libraries:
  ```r
  library(gsDesign)
  library(brms)
  library(data.table)
  library(dplyr)
  ```
- SSH access to Compute Canada.

## **Running Simulations**
1. **Set Parameters:** Modify cohort size (default 30) if necessary in R scripts. The R scripts also contain additional lines at the beginning to be run via SSH.
2. **Submit Jobs:**
   ```bash
   for ii in {1..1000}; do sbatch jobname.sh $ii; done
   ```
3. **Example `jobname.sh`:**
   ```bash
   #!/bin/bash
   #SBATCH --time=01:00:00
   #SBATCH --mem-per-cpu=4G
   #SBATCH --cpus-per-task=1
   #SBATCH --job-name="job1"
   #SBATCH --error=job1_%j.err

   cd /home/.../repository_name
   Rscript brms_code_1.R $1
   ```

## **Analyzing Results**
Run `tables_and_graphs.R` to analyze `_dat.rds` files. The main functions inside the file:
- **`run_blost()`**: For BLOST outputs (the working directory needs to be specified where `_dat.rds` are located)
- **`run_blost_bms()`**: For BLOST_BMS outputs (the working directory needs to be specified where `_dat.rds` are located)
- **`run_freq()`**: Performs a frequentist analysis 

Each function computes one **rejection rate of H0** for the indicated scenario, cohort_size, alpha and spending_function (Pocock or O'Brien-Fleming). Full final tables/graphs presented in the paper are generated at the script's end.


