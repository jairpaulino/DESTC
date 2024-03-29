% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/destc.R
\name{getDESTC}
\alias{getDESTC}
\title{DESTC model forecast}
\usage{
getDESTC(
  train_df,
  test_df,
  w,
  alpha,
  running_mean = 1,
  ensemble_method,
  none_models_number,
  pos_models_number,
  neg_models_number
)
}
\arguments{
\item{train_df}{Data frame. A pool of single models to be used as training set.}

\item{test_df}{Data frame. A pool of single models to be used as test set.}

\item{w}{Int. The sliding windows lag size.}

\item{alpha}{Float. The significance level (alpha) of MK trend test.}

\item{running_mean}{Int (default = 1). If running_mean>1, a running mean function is applied.}

\item{ensemble_method}{Character. A ensemble method ("median", "mean").}

\item{none_models_number}{Int. The number of models to be combined to "none trend" instances.}

\item{pos_models_number}{Int. The number of models to be combined to "positive trend" instances.}

\item{neg_models_number}{Int. The number of models to be combined to "negative trend" instances.}
}
\value{
A numeric vector of DESTC forecasts.
}
\description{
Generates forecasts based on DESTC model
}
\examples{
train_pool_df = read.csv(paste0("data/", country, "/pool_complete_valid.csv"))
test_pool_df =  read.csv(paste0("data/", country, "/pool_complete_test.csv"))
test = getDESTC(train_df = train_pool_df, test_df = test_pool_df
                , w = 10, alpha = 0.1
                , ensemble_method = "median", running_mean = 1
                , none_models_number = params$none_number
                , pos_models_number = params$pos_number
                , neg_models_number = params$neg_number)
}
