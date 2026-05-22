# 🏘️ Households Over the Years
How has household size and configuration changed in the last 125 years? We answer this question 
using publicly available American Community Survey and Decennial Census data, accessed 
via IPUMS, alongside the analytic tools available in the [demographR](https://github.com/lorae/demographr) package.

## 📊 Key Findings

The average American at the turn of the 20th century lived in a household with nearly 
6 individuals. With some exceptions, that value has since been on the decline, with the
rate of decrease in household size much slower in recent decades than during most of 
the 20th century. As of 2023, the average American lives in a household of 3.33 persons.
![](k-means-clustering/output/figures/fig01-hhsize-decades-line.jpeg)

## ⚡ Quick Start
For experienced users who just want to get the project running right away. If you
have trouble following these steps, please follow the **Detailed Start** guide below.

1. Navigate to the directory where you want the project to be saved and clone both required repos side by side

    ```bash
    cd your/path/to/parent/directory
    ```

    ```bash
    git clone https://github.com/lorae/households-over-the-years households-over-the-years
    git clone https://github.com/lorae/demographr demographr
    ```

2. Enter the main project

    ```bash
    cd households-over-the-years
    ```

3. Copy the environment file and edit it with your own [IPUMS API key](https://account.ipums.org/api_keys)

    ```bash
    cp example.Renviron .Renviron
    # (Windows PowerShell: Copy-Item example.Renviron .Renviron)
    # IMPORTANT: open .Renviron and replace "your_ipums_api_key" with your actual key
    ```

4. Restore dependencies and run the analysis

    Open `households-over-the-years.Rproj` in your preferred IDE, then in the R console:
    
    ```r
    renv::restore()
    source("k-means-clustering/run-all.R")    # 1900-2023 clustering
    source("five-decade-aggregates/run-all.R") # 1970-2020 crowding
    source("household-archetypes/run-all.R")   # single-mother cost burden 1970-2020
    source("bedroom-allocation/run-all.R")     # how characteristics map to bedrooms over time
    ```
    


## 📎 Detailed Start
Detailed instructions for how to fully install and run this project code on your computer.

###  Part A: Clone the repo and configure the R project

These steps will allow you to install the code on your computer that runs this project and set up the environment so that it mimics the environment on which the code was developed.

1. **Clone the repo**: Open a terminal on your computer. Navigate to the directory you would like to be the parent directory of the repo, then clone the repo.

    MacOS/Linux:
    
    ```bash
    cd your/path/to/parent/directory
    ```
    ```bash
    git clone https://github.com/lorae/households-over-the-years households-over-the-years
    ```
    
    Windows:
    
    ```cmd
    cd your\path\to\parent\directory
    ```
    ```cmd
    git clone https://github.com/lorae/households-over-the-years households-over-the-years
    ```

2. **Open the R project**: Navigate into the directory, now located at `your/path/to/parent/directory/households-over-the-years`.
Open `households-over-the-years.Rproj` using your preferred IDE for R. (We use R Studio.)

    Every subsequent time you work with the project code, you should always open the `households-over-the-years.Rproj` file
    at the beginning of your work session. This will avoid common issues with broken file paths or an incorrect working directory.

3. **Initialize R environment**: Install all the dependencies (packages) needed to make the code run on your computer.

    First, ensure you have the package manager, `renv`, installed. Run the following in your R console:
    
    ```r
    install.packages("renv") # Safe to run, even if you're not sure if you already have renv
    ```
    ```r
    library("renv")
    ```
    
    Then restore the project:
    
    ```r
    renv::restore()
    ```

4. **Clone the sibling repo, `demographr`**: This project makes use of a bundle of functions that are unit-tested
and generalized in a package called `demographr`. Clone this repo in the same parent directory where you cloned 
`immigrant-households`.

    🛑 Important: Do not clone this **inside** of the `immigrant-households` repo: instead, it should be a 
    sibling: it should contained in the same folder structure as `households-over-the-years`.

    MacOS/Linux:
    
    ```bash
    cd your/path/to/parent/directory
    ```
    ```bash
    git clone https://github.com/lorae/demographr demographr
    ```
    
    Windows:
    
    ```cmd
    cd your\path\to\parent\directory
    ```
    ```cmd
    git clone https://github.com/lorae/demographr demographr
    ```
    
###  Part B: Configure API Access

The [IPUMS Terms of Use](https://www.ipums.org/about/terms) precludes us from directly sharing the raw microdata extract, however,
the data used in this analysis is freely available after setting up an IPUMS USA account, and we provide an automated script that 
writes the API call and downloads the exact data used in this analysis. 

5. **Copy the file** `example.Renviron` to a new file named `.Renviron` in the project root directory. 
You can do this manually or use the following terminal commands:

    MacOS/Linux:
    
    ```bash
    cp example.Renviron .Renviron
    ```
    
    Windows (use PowerShell):
    
    ```ps1
    Copy-Item example.Renviron .Renviron
    ```
    
6. **Set up your IPUMS USA API key**: If you don't already have one, set up a free account on 
[IPUMS USA](https://uma.pop.umn.edu/usa/user/new). Use the new account to login to the 
[IPUMS API key](https://account.ipums.org/api_keys) webpage. Copy your API key from this webpage.

7. **Open `.Renviron`** (‼️**not** `example.Renviron`!) and replace `your_ipums_api_key` with your actual key.  Do not include quotation marks. 
R will automatically load `.Renviron` when you start a new session. This keeps your API key private and separate 
from the codebase.

    🛑 Important: `.Renviron` is listed in `.gitignore`, so it will not be tracked or uploaded to GitHub — but `example.Renviron` is tracked, so do not put your actual API key in the example file.

### Part C: Run the analysis scripts

The code for this project is organized into four sub-projects, each with its own `run-all.R`, `src/`, `output/`, and (where used) `throughput/`. All sub-projects share the top-level `data/five-decade-db/ipums.duckdb` and the `renv/` environment, and all load the sibling `demographr` package via `devtools::load_all("../demographr")`.

- **`k-means-clustering/`** — Unsupervised clustering of household archetypes using IPUMS USA microdata from 1900–2023. Currently on hold.
- **`five-decade-aggregates/`** — Descriptive analysis of household crowding, persons-per-bedroom, and subfamily structure from 1970 to 2020 (decennial census + ACS 5-year pools). Also contains the graph-based subfamily detection pipeline over CPS data.
- **`household-archetypes/`** — Narrowly-defined single-mother household analysis: bedroom distribution, cost burden distributions (renters + owners), mean rent burden by bedrooms over time, and a progression of weighted regressions estimating cost burden. Includes `single-mother-results.Rmd`, a knitted HTML write-up of the findings.
- **`bedroom-allocation/`** — How household and person characteristics translate into bedrooms, and how that relationship has shifted over time. Includes a full-population bedroom distribution by tenure (1970 vs 2020) and a planned series of per-decade regressions of `BEDROOMS` on household characteristics, with counterfactual decompositions across decades.

8. Run the analysis by sourcing the appropriate `run-all.R` in your R console:

    ```r
    # K-means clustering analysis (1900-2023)
    source("k-means-clustering/run-all.R")

    # Five-decade crowding analysis (1970-2020)
    source("five-decade-aggregates/run-all.R")

    # Single-mother cost burden analysis (1970-2020)
    source("household-archetypes/run-all.R")

    # Bedroom allocation across household characteristics over time
    source("bedroom-allocation/run-all.R")
    ```
    


## 📜 License
MIT License (see LICENSE file).

## 📚 Citation
This repository accompanies ongoing research on households and household size. 

For now, please cite as:  
*Households Over the Years: Replication Code and Analysis*. Maintained by Lorae Stojanovic and Peter Hepburn.  
GitHub. https://github.com/lorae/households-over-the-years
