# Source this file to replicate the entire project.
# BEFORE RUNNING: Create the .Renviron per the instructions in README.md.

# R environment setup
renv::restore()

# Read and clean data
source("src/import-ipums-usa.R")
source("src/process-ipums-usa-person.R")
source("src/process-ipums-usa-household.R")

# Create figures and results
source("src/figures/fig01-hhsize-decades-line.R")
source("src/figures/fig02-household-members.R")
source("src/figures/fig03-multifam-decades-line.R")
source("src/figures/fig04-hoh-housemates-normalized-line.R")

# k-means ML
source("src/cluster-elbow-plot.R") # choose k from k=1 to k=15 (outputs fig05)

# k = 6 clustering
source("src/k6/cluster-k6-selection.R") # choose among k = 6 model outputs (outputs fig06)
source("src/k6/cluster-define-archetypes.R") # outputs tab01, tab02, tab03
source("src/k6/cluster-graph-archetypes.R") # outputs fig07, fig08, fig09, fig10

# k = 9 clustering
source("src/k9/cluster-k9-selection.R") # choose among k = 9 model outputs (outputs fig11)



