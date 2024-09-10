[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12579204.svg)](https://doi.org/10.5281/zenodo.12579204)

# Repository for paper on Cultural Hitchhiking and post marital residence rules titled "Post-marital residence rules and transmission pathways in cultural hitchhiking"

## Multimodal Transmission

What happens when horizontal, vertical, oblique transmission interact with sexual biases, population growth and between population mating?  This repository allow to run the model used in the paper and redo the plot presented in the paper.

For quick exploration/coding, this repository can be loaded as a package using `devtools::load_all()`.


## General Structure of the Folder

- `R/`: Core R functions.
- `data/`: R data object used by the package
- `data-raw/`: scripts describing how the data have been generated
- `manuscript/: scripts and files specific for the paper (latex file, raw svg and scripts to recreate the figures).
  - `manuscript/script_figures/`: scripts to recreate the figures
- `inst/`: 
  - `inst/scripts/`: Various scripts used throughout the development of the model and while writing the paper.
  - `inst/extdata/`: Data generated for model exploration.
- `man/`: Documentation for R functions.
- `simulations/`: Simulation outputs (archves in this folder need to be downloaded from Zenodo, see `simulations/README.md`).
- `tests/`: Unit tests to ensure that functions generate the expected output.
- `vignettes/`: A few vignettes that explore specific, simple aspects of the model.

## Details 

### `R/`


- `R/communities.R`: tools to generate and handle communities
- `R/computerate.R`: minimal function to compute \code{lambda} give base rate and adaptive traits
- `R/modelOOstyle.R`: deprecated model base on a list allowing a more Object Oriented feeling
- `R/modelVector.R`: current implementation of the model
- `R/reproduction.R`: set of functions to handle reproduction
- `R/socialLearning.R`: set of functions to handle social learning
- `R/generatePopulations.R`: create population structure
- `R/socialLearning.R`: core functions defining social learning for all pathways and biases
- `R/marriage.R`: function dealing with the matching of single individuals
- `R/analysis_tools.R`: function to extract and analysis model outcome
- `R/ageDeath.R`: function generating age dependent death. 
- `R/tools.R`: set of functions used throughout the package
- `R/legacyFunction.R`: archives of functions not used anymore
- `R/VecToList.R`: deprecated tools to transform list style model to arrays
- `R/joyplot.R`: functoion to semi automatise the generation of 'joyplot' density plots.

### `inst/scripts/`


Various scripts to be cleaned and grouped ; some should be made unit tests, others should be made vignettes

- `inst/scripts/base_exp_newPW.R`: script to run basic experiment in paralel
- `inst/scripts/paral.R`: last implementation of the script used to run basic experiment in paralel
- `inst/scripts/explore2populations.R`
- `inst/scripts/exploreGrowthRate.R` : draft of script that explore the impact of different parameters on the overall growth of the population
- `inst/scripts/fullJoyPlot.R`: draft script to generate joyplot representing within pop distribution of $c_i$s (final implementations in R/joyplot)
- `inst/scripts/generateBoxplotsEndTraits.R`: draft script to generate boxplot of % of $c_i$ 
- `inst/scripts/generateGraphs.R`
- `inst/scripts/neutralTranmission.R`
- `inst/scripts/retrieveAndPlotGrowthRates.R`
- `inst/scripts/testModel.R`
- `inst/scripts/trajPerTypes.R`
 
### `inst/extdata/`

Small datasets mainly used for unit testing and vignettes:

 - `inst/extdata/commu_ex.RDS`:  4 communities of sizes (154,150,149,150) on a 2x2 grid
 - `inst/extdata/pool.ex.Rbin`: fake pool of neutral traits with different sex to test social learning mechanism. 3 pools of 23, 23 and 18 indviduals.
 - `inst/extdata/pop_ex.RDS`:populaiton of 602 individual with5 neutral traits population the commu_ex before
 - `inst/extdata/pop_ex.csv`: csv version of the previous dataset
 - `inst/extdata/pop_ex2.csv`: another exemple of a valide popution (N=612)
 - `inst/extdata/pop_ex3.csv`: another exemple of a valide popution (N=607)


### `tests/`

Test files to be sure everything stays in order (needs to be improved)


## Fundings

This work has been made possible by funding from the Synergy project *COREX: From Correlations to Explanations: Towards a New European Prehistory* under the European Union’s Horizon 2020 research and innovation programme (Grant Agreement No. 95138) and the ERC Starting Grant *Demography, Cultural change, and the Diffusion of Rice and Millet during the Jomon-Yayoi transition in prehistoric Japan* (Grant Agreement No. 801953).
