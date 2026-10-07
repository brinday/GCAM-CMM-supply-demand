# Yarlagadda et al. "Critical mineral resource availability and lead times may constrain multi-decadal supplies amid growing demands"

## Summary
Future demand for critical minerals could grow significantly, but long lead times and resource availability could constrain supplies. Efforts to characterize future CMM availability have largely treated supplies as static or ignored feedbacks between supply and demand. Here, we embed supply curves for three CMMs (copper, lithium, and nickel) into a multi-sectoral model that resolves regional primary production, economy-wide demands, and prices. Through mid-century, copper and nickel production to meet global demands primarily draws on operating mines; lithium relies heavily on projects under development. Beyond 2040, production of all three minerals increasingly depends on resources not associated with existing projects. Lead times constrain production potential through 2040 and result in multi-fold copper and nickel price increases, leading to substantial shifts in energy technology deployment. However, new discoveries could alleviate these effects. Our findings underscore the importance of analyzing mineral supplies and demands in a dynamic, interconnected manner.

## Journal reference
To be added

## Code and Data
### GCAM Model Version and Input Files
Yarlagadda, B and A. Zagoruichyk. (2026). Input files and model version for GCAM-CMM-supply-demand. Zenodo.
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20559774.svg)](https://doi.org/10.5281/zenodo.20559774)

This study's model version is based on GCAM v8.2.

### Output data
Yarlagadda, B and A. Zagoruichyk. (2026). Output data from Yarlagadda et al. gcam-CMM-supply-demand-paper
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20571203.svg)](https://doi.org/10.5281/zenodo.20571203)

## Table of Contents

- [Part I: Running a Scenario](#part-i-running-a-scenario)
- [Part II: Generating the `.prj` File](#part-ii-generating-the-prj-file)
- [Part III: Generating Data and Figures](#part-iii-generating-data-and-figures)
- [Part IV: Reproducing Figures and Data from Pre-Computed Model Output](#part-iv-reproducing-figures-and-data-from-pre-computed-model-output)

## Part I: Running a Scenario

### 1. Install GCAM
If this is your first time running GCAM, install the **Release version of GCAM for Windows** by following the video walkthrough:
🔗 https://www.youtube.com/watch?v=2Tv-5rryhk8

### 2. Download and unzip the scenario package
Download `gcam_CMM_supply_demand.zip` file from Zenodo and extract its contents into a separate folder. 
Do not extract it into your GCAM Release folder.

Inside you will find:
- `run-gcam.bat` — the script that launches the model
- Several configuration files, each defining a different scenario type*
- Other files necessary to run GCAM

#### *The table below shows the mapping of scenarios to configuration files 

|Paper scenario name                                   |Configuration file name                             |GCAM scenario name                              |
|:-----------------------------------------------------|:---------------------------------------------------|:-----------------------------------------------|
|Reference                                             |configuration_Reference.xml                         |01272026_His_constrSupply_BR_noTC               |
|Unconstrained supply                                  |configuration_UnconstrSupply.xml                    |01272026_UnlimitSupply_BR                       |
|Unconstrained supply: Increased recycling             |configuration_UnconstrSupply_IncreasedRecycling.xml |01272026_UnlimitSupply_EnR                      |
|Unconstrained supply: High EV demand                  |configuration_UnconstrSupply_HighEVDemand.xml       |03192026_UnlimitSupply_highDemand               |
|New resources                                         |configuration_NewResources.xml                      |01272026_His_constrSupply_BR_SS_noTC            |
|New resources: High EV demand                         |configuration_NewRes_HighEVDemand.xml               |03192026_His_constrSupply_SS_highDemand         |
|New resources: Short lead times                       |configuration_NewRes_ShortLT.xml                    |01272026_His_constrSupply_BR_SS_shortLT_noTC    |
|New resources: Increased recycling                    |configuration_NewRes_IncreasedRecycling.xml         |01272026_His_constrSupply_EnR_SS_noTC           |
|New resources: Short lead times + Increased recycling |configuration_NewRes_ShortLT_IncreasedRecycling.xml |01272026_His_constrSupply_EnR_SS_shortLT_noTC   |
|New resources: Short lead times + High EV demand      |configuration_NewRes_ShortLT_HighEVDemand.xml       |03192026_His_constrSupply_SS_shortLT_highDemand |
|Short lead times                                      |configuration_ShortLT.xml                           |01272026_His_constrSupply_BR_shortLT_noTC       |
|Short lead times + High EV demand                     |configuration_ShortLT_HighEVDemand.xml              |03192026_His_constrSupply_shortLT_highDemand    |
|Increased recycling                                   |configuration_IncreasedRecycling.xml                |01272026_His_constrSupply_EnR_noTC              |
|Short lead times + Increased recycling                |configuration_ShortLT_IncreasedRecycling.xml        |01272026_His_constrSupply_EnR_shortLT_noTC      |
|High EV demand                                        |configuration_HighEVDemand.xml                      |03192026_His_constrSupply_highDemand            |
|Increased recycling + High EV demand                  |configuration_IncreasedRecycling_HighEVDemand.xml   |09022026_His_constrSupply_EnR_highDemand        |
|Static at 2021 levels                                 |configuration_Static_2021.xml                       |08312026_His_constrSupply_static_2021           |
|Static at 2075 levels                                 |configuration_Static_2075.xml                       |08312026_His_constrSupply_static_2075           |
|Shorter lead times                                    |configuration_ShorterLT.xml                         |09022026_His_constrSupply_even_shorterLT_BR     |
|25% lower extraction costs                            |configuration_LowerExtrCosts.xml                    |09092026_His_constrSupply_BR_75_costs           |

### 3. Choose your scenario and edit `run-gcam.bat`
Right-click `run-gcam.bat` → **Edit** (or open it in Notepad) and make two changes:

**a) Point to the configuration file you want to run**

Find the line that starts with:
```bat
gcam.exe -C configuration_xyz.xml
```
Replace `configuration_xyz.xml` with the name of the configuration file for the scenario you want to run, e.g.:
```bat
gcam.exe -C configuration_NewResources.xml
```

**b) Set the path to your Java installation**

Find the line that starts with:
```bat
SET JAVA_HOME=
```
Replace the path with the actual location of the Java folder installed alongside your GCAM release, e.g.:
```bat
SET JAVA_HOME=C:\Program Files\Java\jre1.8.0_461
```
> ⚠️ This is only an example path — the Java folder installed on your machine may live somewhere else. Locate it before editing.

**c) Save and close**
- `File → Save`
- Close Notepad

### 4. Run the model
Double-click `run-gcam.bat`. The model run starts immediately in a terminal window.

> ⚠️ **Do not close the terminal window** and **do not let your computer lose power/sleep** while the run is in progress. Interrupting the run will break it, and you will need to start over from Step 4.

### 5. Wait for completion (might take up to an hour or longer, depending on the scenario)
Wait until the terminal displays:
```
Starting output to XML Database.
Model run completed.
Model exiting successfully.
```
Once you see this message, it is safe to close the terminal. Your output database has been saved in the `output` folder.

## Part II: Generating the `.prj` File

Once your model run has completed and the output database has been saved, use the provided R script to generate a `.prj` file for querying results.

### 1. Install Rtools 4.4:
🔗 https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html

### 2. Download the R script package
Download and unzip the `Rgcam_Querying.R` script package. Open it through `Rgcam.Rproj`. Inside you will find `minerals_queries.R`.

### 3. Install required R packages

```r
install.packages("devtools", type = "binary")

install.packages("remotes")

remotes::install_github(
  "JGCRI/rgcam",
  dependencies = TRUE,
  build_vignettes = FALSE,
  upgrade = "never"
)
```

### 4. Set the path to your output database folder
Open `minerals_queries.R` and set `db_path` to the location of the `output` folder created in Part I:
```r
db_path <- "C:/Users/Documents/Git_Hub_Repos/mineral_supply_demand_adj/output"
```
> ⚠️ This is only an example path — make sure the path leads to the database inside your `output` directory, or the script will fail to find it.

### 5. Set the name of the database to process, e.g.:
```r
db_names <- c("db_03192026_UnlimitSupply_highDemand")
```

### 6. Name your `.prj` file (the naming will be important for Part III), e.g.:
```r
prj_name <- "03192026_UnlimitSupply_highDemand_prj"
```

### 7. Source the script
Run the entire script (in RStudio: **Source**, or `Ctrl+Shift+Enter`).

### 8. Locate the generated file
The `.prj` file will appear in the same folder as `minerals_queries.R`, named according to the `prj_name` you set in Step 4 — e.g. `03192026_UnlimitSupply_highDemand_prj`.

## Part III: Generating Data and Figures

Once you have generated your `.prj` file(s) in Part II, use the provided repository to produce data outputs and figures.

### 1. Clone/download the repository
Clone or download the repository from GitHub:
🔗 https://github.com/brinday/GCAM-CMM-supply-demand.git

### 2. Locate the figure-generation script
Open it through `GCAM-CMM-supply-demand.Rproj`. Inside the repository you will find `generate_figures.R`. There, you will see the following lines:
```r
# READ IN DATA ------------------------------------------------------------
prj_A <- loadProject("input/data/prj_01272026")
prj_B <- loadProject("input/data/prj_03022026")
prj_C <- loadProject("input/data/prj_03072026")
prj_D <- loadProject("input/data/prj_03192026")
prj_E <- loadProject("input/data/prj_04092026")
```

### 3. Rename the `.prj` file references
Update each `loadProject(...)` line so the file name matches the `prj_name` you set in **Step 6 of Part II**. For example, if your `.prj` file was named `03192026_UnlimitSupply_highDemand_prj`, the corresponding line should read:
```r
prj_D <- loadProject("input/data/03192026_UnlimitSupply_highDemand_prj")
```
> Note: You only need to update the lines corresponding to the scenarios you actually generated — remove or comment out any `prj_` lines you don't need.

### 4. Place the `.prj` files in the input folder
Move (or copy) the `.prj` file(s) you want results for into the `input/data` folder of this repository.
> ⚠️ The file names in `input/data` must exactly match the names referenced in Step 3, or the script will fail to find them.

### 5. Source the script
Run the entire `generate_figures.R` script (in RStudio: **Source**, or `Ctrl+Shift+Enter`).

### 6. Locate the generated data and figures
Once the script finishes running, the generated data outputs and figures will be saved in the repository's designated output folder.

## Part IV: Reproducing Figures and Data from Pre-Computed Model Output

If you don't want to run your own scenario and generate `.prj` files from scratch (Parts I-III), you can instead run the figures script directly using **pre-computed `.prj` files** provided in the project's Zenodo repository.

### 1. Clone/download the repository
Clone or download the repository from GitHub:
🔗 https://github.com/brinday/GCAM-CMM-supply-demand.git

### 2. Download pre-computed `.prj` files
Download the latest `.prj` files from the **"Output data"** section of the Zenodo repository, and place them in the `input/data` folder of the cloned repository.

### 3. Install Rtools
Install **Rtools 4.4**, required to build and run the packages below:
🔗 https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html

### 4. Install required R packages (one-time setup)
Run the commands below **only once**. After installation succeeds, comment these lines out so they aren't re-run every time you source the script.
```r
install.packages("devtools", type = "binary")
install.packages("remotes")

remotes::install_github(
  "JGCRI/rgcam",
  dependencies = TRUE,
  build_vignettes = FALSE,
  upgrade = "never"
)

remotes::install_github(
  "JGCRI/gcamdata",
  dependencies = TRUE,
  build_vignettes = FALSE,
  upgrade = "never"
)

remotes::install_github(
  "JGCRI/rmap",
  dependencies = TRUE,
  build_vignettes = FALSE,
  upgrade = "never"
)
```

### 5. Source the script
Open it through `GCAM-CMM-supply-demand.Rproj`. Run the entire `generate_figures.R` script (in RStudio: **Source**, or `Ctrl+Shift+Enter`).

### 6. Locate the generated data and figures
Once the script finishes running, the generated data outputs and figures will be saved in the repository's designated output folder.
