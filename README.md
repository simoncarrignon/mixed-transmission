Repository for PNAS special issue paper on Cultural Hitchhiking and post marital residence rules

# Multimodal Transmission

What happened when horizontal, vertical, oblique transmission interact with sexual biases, population growth and between population mating?

This repository can be loaded as a package using `devtools::load_all()`.

Quick note about the folders and files available:


- `R/`
- `inst/`
- `tests/`
- `inst/scripts/`
- `inst/scripts/`

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

### `inst/scripts/`


Various script to be cleaned and group 
- `inst/scripts/base_exp_newPW.R`
- `inst/scripts/countMigrants.R`
- `inst/scripts/explore2populations.R`
- `inst/scripts/exploreGrowthRate.R`
- `inst/scripts/extractCommunites.R`
- `inst/scripts/fullJoyPlot.R`
- `inst/scripts/generateBoxplotsEndTraits.R`
- `inst/scripts/generateGraphs.R`
- `inst/scripts/joyPlot.R`
- `inst/scripts/joyPlotPerType.R`
- `inst/scripts/neutralTranmission.R`
- `inst/scripts/retrieveAndPlotGrowthRates.R`
- `inst/scripts/testAdoption.R`
- `inst/scripts/testModel.R`
- `inst/scripts/trajPerTypes.R`
 
### `inst/extdata/`

Various dataset that can be use to run test/vignettes...

## `tests/`

Test files to be sure everything stays in order (needs to be improved)
