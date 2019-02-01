\name{ics2csv}
\alias{ics2csv}
\title{Convert ICS Files to CSV}
\usage{
ics2csv(ics_file_path)
}
\arguments{
\item{ics_file_path}{Path to the input file}
}
\description{
This will load in the ICS file using readLines, and convert it to a CSV file. Currently metadata is discarded. It will turn available date information into lubridate date objects.
}
