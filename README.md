Repository for PNAS special issue paper on Cultural Hitchhiking and post marital residence rules

# Multimodal Transmission

What happened when horizontal, vertical, oblique transmission interact with sexual biases, population growth and between population mating?

This repository can be loaded as a package using `devtools::load_all()`.

Quick note about the folder and R files available:


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
- `R/ageDeath.R`: function generating age dependent death functions. 
- `R/tools.R`: set of useful functions 
- `R/legacyFunction.R`: archives of functions not used anymore
- `R/VecToList.R`: deprecated tools to transform list style model to arrays
