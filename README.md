# Yarlagadda et al. "Critical mineral resource availability and lead times may constrain multi-decadal supplies amid growing demands"

## Summary
Future demand for critical minerals could grow significantly, but long lead times and resource availability could constrain supplies. Efforts to characterize future CMM availability have largely treated supplies as static or ignored feedbacks between supply and demand. Here, we embed supply curves for three CMMs (copper, lithium, and nickel) into a multi-sectoral model that resolves regional primary production, economy-wide demands, and prices. Through mid-century, copper and nickel production to meet global demands primarily draws on operating mines; lithium relies heavily on projects under development. Beyond 2040, production of all three minerals increasingly depends on resources not associated with existing projects. Lead times constrain production potential through 2040 and result in multi-fold copper and nickel price increases, leading to substantial shifts in energy technology deployment. However, new discoveries could alleviate these effects. Our findings underscore the importance of analyzing mineral supplies and demands in a dynamic, interconnected manner.

## Journal reference
To be added

## Code and Data
### GCAM Model Version and Input Files
Yarlagadda, B. (2026). Input files and model version for GCAM-CMM-supply-demand. Zenodo.
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20559774.svg)](https://doi.org/10.5281/zenodo.20559774)

This study's model version is based on GCAM v8.2.

### Output data
Yarlagadda, B. (2025). Output data from Yarlagadda et al. gcam-CMM-supply-demand-paper
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20571203.svg)](https://doi.org/10.5281/zenodo.20571203)

### System requirements:
Software dependencies (and versions) for running GCAM, an open source model, are documented at: 
https://jgcri.github.io/gcam-doc/gcam-build.html

### Installation guide:
A compiled executable (for Linux systems) used to run this study's model version is provided in the "GCAM Model Version and Input Files" repository above. Alternatively, you can install and compile GCAM using the instructions documented at: https://jgcri.github.io/gcam-doc/user-guide.html. To install and compile GCAM, it typically takes 2-3 hours. 

### Demo:
To run the scenarios in this paper, use the exe/configuration_*.xml files (a total of 15 configuration files for each of the 15 scenarios in the paper) included in the "GCAM Model Version and Input Files" repository above. To run a single scenario, it typically takes 30 minutes.

Output data used to generate all results and figures in the paper have been provided in the output dataset. To run this script and generate all figures, it typically takes ~20 minutes.

To reproduce the figures shown in the paper:
1. clone the repository
2. download the latest prj files from the zenodo repository for "Output data" and add it to use the _\input\data folder
3. run script generate_figures.R
