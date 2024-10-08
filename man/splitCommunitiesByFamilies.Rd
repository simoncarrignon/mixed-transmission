% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/communities.R
\name{splitCommunitiesByFamilies}
\alias{splitCommunitiesByFamilies}
\title{Split Communities by Families}
\usage{
splitCommunitiesByFamilies(comid, population, newsize, newid)
}
\arguments{
\item{comid}{The ID of the community to be partitioned.}

\item{population}{A dataframe containing population data,
which includes columns for community IDs and family IDs.}

\item{newsize}{The target population size for the selected families.
The function selects families until this size is reached or exceeded.}

\item{newid}{The new community ID to be assigned to the selected families.}
}
\value{
The population dataframe with updated community IDs for the selected families.
}
\description{
This function selects families from a specified community within a population dataset
until the total population size of the selected families reaches or exceeds a specified limit.
It then assigns a new community ID to these families.
}
\examples{
# Assuming `population` is a dataframe with 'community' and 'fid' columns
#splitCommunitiesByFamilies(1, population, 2, 2)
}
