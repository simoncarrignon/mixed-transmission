#'  generic population
#'
#' A generic exemple
#' Report ...
#'
#' @format ## `population`
#' A fake populaiton with valid structure (couple, families and ages)
#' 600 row (individuals)  and 13 columns
#' \describe{
#'   \item{id}{unique id for every individual}
#'   \item{cid}{unique id for each couple}
#'   \item{fid}{unique id for each family}
#'   \item{age}{age of individual}
#'   \item{sex}{Sex of individual}
#'   \item{partner}{id of partner}
#'   \item{community}{Community of each individual}
#'   \item{t1 ... t4}{Neutral traits fo each individual}
#' }
"population"

#'  generic smaller population
#'
#' A generic exemple
#' Report ...
#'
#' @format ## `smallpo`
#' A fake populaiton with valid structure (couple, families and ages)
#' 186 row (individuals)  and 13 columns
#' \describe{
#'   \item{id}{unique id for every individual}
#'   \item{cid}{unique id for each couple}
#'   \item{fid}{unique id for each family}
#'   \item{age}{age of individual}
#'   \item{sex}{Sex of individual}
#'   \item{partner}{id of partner}
#'   \item{community}{Community of each individual}
#'   \item{t1 ... t4}{Neutral traits fo each individual}
#' }
"smallpop"

#'  Population used for the paper
#'
#' This population has 10 communities ; 500 individual and mean of exactly 50 people per community. This happened by chance. so better keep it
#' Report ...
#'
#' @format ## `paperpopulation`
#' A fake populaiton with valid structure (couple, families and ages)
#' 500 row (individuals)  and 13 columns
#' \describe{
#'   \item{id}{unique id for every individual}
#'   \item{cid}{unique id for each couple}
#'   \item{fid}{unique id for each family}
#'   \item{age}{age of individual}
#'   \item{sex}{Sex of individual}
#'   \item{partner}{id of partner}
#'   \item{community}{Community of each individual}
#'   \item{t1 ... t4}{Neutral traits fo each individual}
#' }
"paperpopulation"



#'  mortality rate by ages
#'
#' a vector describing mortality rate by age
#' Report ...
#'
#' @format ## `mortality`
#' A vector for probability of death for different age catgory
#' 6 values
#' \describe{
#'   6 categories of ages that need to be complated by deathage
#' }

"mortality"
