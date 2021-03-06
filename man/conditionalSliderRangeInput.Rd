% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditionalSliderRangeInput.R
\name{conditionalSliderRangeInput}
\alias{conditionalSliderRangeInput}
\title{Display a Point or Range Slider Based on a Trigger Input}
\usage{
conditionalSliderRangeInput(
  id,
  label,
  value,
  value2,
  trigger,
  ns = NS(NULL),
  min = NA,
  max = NA,
  step = NA,
  prefix = NULL,
  suffix = NULL
)
}
\arguments{
\item{id}{The input element ID}

\item{label}{The input element label}

\item{value}{`[numeric(1)]` The point value for the single handle}

\item{value2}{`[numeric(2)]` The range of the double handles}

\item{trigger}{`[character(1)]` The trigger variable}

\item{ns}{The shiny session namespace}

\item{min}{The minimum slider value}

\item{max}{The maximum slider value}

\item{step}{The step size of the slider}

\item{prefix}{A prefix to prepend to the value labels}

\item{suffix}{A suffix to append to the value labels}
}
\value{
A UI element
}
\description{
This input will display a slider with either a single or a double handle,
depending on whether it is selected in a trigger variable. It always returns
both the point (single) and the range (double) values; use
`reactivePointRange()` to choose the one currently displayed in the UI.
}
\keyword{internal}
