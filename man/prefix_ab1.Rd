% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ab1_prefix.R
\name{prefix_ab1}
\alias{prefix_ab1}
\title{Prefix ab1 files}
\usage{
prefix_ab1(well_file, ab1_folder, re_pattern = "([A-Z]+)(\\\\d+)\\\\.(.+)")
}
\arguments{
\item{well_file}{A string, containing path to Excel well_file.}

\item{ab1_folder}{A string, containing path to ab1_folder.}

\item{re_pattern}{A regular expression string, indicating well index pattern.}
}
\description{
Prefix Sanger sequencing result ab1 files with a excel file containing a
8 x 12 spreadsheet filled with corresponding sample ID.
}
