% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_cost.R
\name{model_cost}
\alias{model_cost}
\title{model_cost}
\usage{
model_cost(
  num_informal_workers,
  participation_rate,
  minimum_contribution_monthly_usd = 5.05,
  government_share = 0.5,
  give_subsidy_only_to_subgroup = FALSE,
  percent_Subgroup = 0.5
)
}
\arguments{
\item{num_informal_workers}{number of informal workers in the given year}

\item{participation_rate}{percentage of workers who are signed up to the social security program}

\item{minimum_contribution_monthly_usd}{total monthly minimum contribution (worker + government share)}

\item{government_share}{what percentage of the minimum_contribution_monthly_usd does the government shoulder?}

\item{give_subsidy_only_to_subgroup}{should the subsidy only be given to a specific subgroup of all informal workers?}

\item{percent_Subgroup}{percentage of informal workers belonging to the subgroup (out of all informal workers)}
}
\value{
total cost of the subsidy for the given year
}
\description{
model_cost
}
\examples{
model_cost(100000, 0.1)
}
