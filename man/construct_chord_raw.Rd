% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chords.R
\name{construct_chord_raw}
\alias{construct_chord_raw}
\title{Construct a chord by root tone and key distances}
\usage{
construct_chord_raw(
  root_tone,
  distances_rel,
  chord_type = NULL,
  label_suffix = NULL
)
}
\arguments{
\item{root_tone}{root tone of chord}

\item{distances_rel}{relative distances: major is e.g. \code{c(4, 3)}, minor \code{c(3, 4)} etc.}

\item{chord_type}{a chord type, e.g. major, minor}

\item{label_suffix}{a suffix, e.g. \code{"m"} for minor}
}
\description{
Construct a chord by root tone and key distances
}
