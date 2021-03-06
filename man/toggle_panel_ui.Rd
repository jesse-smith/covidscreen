% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_toggle_panel.R
\name{toggle_panel_ui}
\alias{toggle_panel_ui}
\title{Create a toggle panel in the UI}
\usage{
toggle_panel_ui(
  id,
  ...,
  label = NULL,
  icon = NULL,
  tag_fn = NULL,
  init_visible = TRUE
)
}
\arguments{
\item{id}{The UI element ID (technically the ID of the title)}

\item{...}{Panel content}

\item{label}{The panel title}

\item{icon}{An icon to display to the right of the title}

\item{tag_fn}{A tag to wrap the label and icon}

\item{init_visible}{Should the panel initialize as visible or invisible?}
}
\value{
A UI element
}
\description{
A toggle panel is a panel with show/hide functionality by clicking on the
title and additional info by clicking on an info button. `toggle_panel_ui()`
creates the UI definition for the panel.
}
\keyword{internal}
