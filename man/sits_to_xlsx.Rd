% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_xlsx.R
\name{sits_to_xlsx}
\alias{sits_to_xlsx}
\alias{sits_to_xlsx.sits_accuracy}
\alias{sits_to_xlsx.list}
\title{Save accuracy assessments as Excel files}
\usage{
sits_to_xlsx(acc, file)

\method{sits_to_xlsx}{sits_accuracy}(acc, file)

\method{sits_to_xlsx}{list}(acc, file)
}
\arguments{
\item{acc}{Accuracy statistics (either an output of sits_accuracy
or a list of those)}

\item{file}{The file where the XLSX data is to be saved.}
}
\value{
No return value, called for side effects.
}
\description{
Saves confusion matrices as Excel spreadsheets. This function
takes the a list of accuracy assessments generated
by \code{\link[sits]{sits_accuracy}}
and saves them in an Excel spreadsheet.
}
\note{
Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.
}
\examples{
if (sits_run_examples()) {
    # A dataset containing a tibble with time series samples
    # for the Mato Grosso state in Brasil
    # create a list to store the results
    results <- list()

    # accuracy assessment lightTAE
    acc_ltae <- sits_kfold_validate(samples_modis_ndvi,
        folds = 5,
        multicores = 1,
        ml_method = sits_lighttae()
    )
    # use a name
    acc_ltae$name <- "LightTAE"

    # put the result in a list
    results[[length(results) + 1]] <- acc_ltae

    # save to xlsx file
    sits_to_xlsx(
        results,
        file = tempfile("accuracy_mato_grosso_dl_", fileext = ".xlsx")
    )
}
}
\author{
Gilberto Camara, \email{gilberto.camara@inpe.br}
}
