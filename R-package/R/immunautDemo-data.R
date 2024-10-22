#' Demo data set from immunaut package.
#' This data is used in this package examples. It consist of 4x4 feature matrix + additional dummy columns that can be used for testing.
#' 
#' @docType data
#'
#' @usage data(immunautDemo)
#'
#' @keywords datasets
#'
#' @examples
#' \dontrun{
#' 	data(immunautDemo)
#' 	## define settings variable
#' 	settings <- list()
#' 	settings$fileHeader <- generate_file_header(immunautDemo)
#' 	# ... and other settings
#' 	results <- immunaut(immunautDemo, settings)
#' }
#' 
"immunautDemo"
