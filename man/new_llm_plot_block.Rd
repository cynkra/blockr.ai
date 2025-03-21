% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_llm_plot_block.R
\name{new_llm_plot_block}
\alias{new_llm_plot_block}
\title{LLM plot block constructor}
\usage{
new_llm_plot_block(
  question = "",
  code = "",
  explanation = "",
  max_retries = 3,
  ...
)
}
\arguments{
\item{question}{Initial question (optional)}

\item{code}{Initial code (optional)}

\item{explanation}{Explanation for initial code (optional)}

\item{max_retries}{Maximum number of retries for code execution}

\item{...}{Forwarded to \code{\link[=new_block]{new_block()}}}
}
\description{
This block allows for plotting data using LLM-generated R code based on natural language questions
}
\examples{
\dontrun{
pkgload::load_all(); serve(new_llm_plot_block(), list(data = mtcars))
}
}
