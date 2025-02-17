% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixr.R
\name{plot_frequencies}
\alias{plot_frequencies}
\title{Returns a plot showing frequencies of cat (lemma or word for instance) in data}
\usage{
plot_frequencies(
  df_freq,
  cat,
  frequency,
  scale = NA,
  fill = NULL,
  fill_fixed = "grey50"
)
}
\arguments{
\item{df_freq}{a tibble with frequencies of cat}

\item{cat}{words or lemmas, for instance}

\item{frequency}{frequency of cat}

\item{scale}{type of transformation (if any) to apply to the x-axis (e.g. "log","sqrt",etc.)}
}
\value{
a plot
}
\description{
Returns a plot showing frequencies of cat (lemma or word for instance) in data
}
\examples{
mydf <- tibble::tibble(txt=janeaustenr::prideprejudice) \%>\%
    tidytext::unnest_tokens(word,txt)
df_freq <- tidy_frequencies(mydf, word, min_freq=500)
plot_frequencies(df_freq,
                 cat=word,
                 frequency=freq)
}
