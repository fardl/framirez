% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{fars_read}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A valid .csv file with the information of interest.}
}
\value{
This function creates a table from the data frame object.
}
\description{
This is a function that loads the database using the function "read_csv"
 from the "readr" package.
}
\note{
If the filename given does not exists,
     the function stops and prints "' does not exist".
     This function also suppresses additional information related
     with the csv database characteristics and potential errors.
}
\examples{
fars_read("dataexample.csv")

}
