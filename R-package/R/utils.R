#' Pre-process and Resample Dataset
#'
#' This function applies pre-processing transformations to the dataset, then resamples it.
#'
#' @param datasetData Dataframe to be pre-processed
#' @param preProcess Vector of pre-processing methods to apply
#' @param selectedOutcomeColumns Character vector of outcome columns
#' @param outcome_and_classes List of outcomes and their classes
#'
#' @return A list containing the pre-processing mapping and the processed dataset
#' @export
preProcessResample <- function(datasetData, preProcess, selectedOutcomeColumns, outcome_and_classes){
    # ==> 2 PREPROCCESING: Skewness and normalizing of the numeric predictors
    preProcessMapping <- NULL
    preProcessedData <- NULL
    if(length(preProcess) > 0 ){
        transformations <- paste(preProcess, sep=",", collapse = ",")
        message <- paste0("===> INFO: Pre-processing transformation(s) (",transformations,") \r\n")
        cat(message)

        impute_idx <- grepl("impute", tolower(preProcess), fixed = FALSE)

        methods_impute <- preProcess[impute_idx]
        methods_no_impute <- preProcess[!impute_idx]

        message <- paste0("===> INFO: Pre-processing methods_impute: ",length(methods_impute)," methods_no_impute ",length(methods_no_impute),"\r\n")
        cat(message)

        if(length(methods_impute) > 0){
            preProcess <- methods_impute
            preProcessedData <- preProcessData(datasetData, selectedOutcomeColumns, outcome_and_classes, preProcess)
            datasetData <- preProcessedData$processedMat
        }

        if(length(methods_no_impute) > 0){
            preProcess <- methods_no_impute
            preProcessedData <- preProcessData(datasetData, selectedOutcomeColumns, outcome_and_classes, preProcess)
        }

        if(!is.null(preProcessedData)){
            ## Final processed data-frame
            datasetData <- preProcessedData$processedMat 

            if("pca" %in% preProcess){
                preProcessMapping <- preProcessedData$preprocessParams$rotation
                ## res.var <- factoextra::get_pca_var(res.pca)
                ## res.var$coord          # Coordinates
                ## res.var$contrib        # Contributions to the PCs
                ## res.var$cos2           # Quality of representation 
                ## corrplot::corrplot(res.var$cos2, is.corr = FALSE)
            }else if("ica" %in% preProcess){
                ## TODO not implemented
                ## preProcessMapping <- preProcessedData$processedMat
            }
        }else{
            message <- paste0("===> INFO: Could not apply preprocessing transformations, continuing without preprocessing.. \r\n")
            cat(message)
        }
    }

    return(list(preProcessMapping = preProcessMapping, datasetData = datasetData))
}

#' Preprocess a Dataset 
#'
#' This function preprocesses a dataset using a given method such as 
#' centering, scaling, etc., and excludes certain classes if specified.
#'
#' @param data The dataset to be preprocessed.
#' @param outcome The outcome variable in the dataset (if any).
#' @param excludeClasses A character vector of column names to exclude from preprocessing.
#' @param methods A character vector specifying the preprocessing methods to apply.
#'
#' @importFrom caret preProcess
#' @importFrom dplyr filter arrange select %>%
#' 
#' @return A list containing the processed dataset and the preprocessing parameters applied.
#' @export
preProcessData <- function(data, outcome, excludeClasses, methods = c("center", "scale"))
{
    set.seed(1337)
    if(length(methods) == 0){
        methods <- c("center", "scale")
    }
    if(!is.null(excludeClasses)){
        whichToExclude <- sapply( names(data), function(y) any(sapply(excludeClasses, function(excludeClass)  return (y %in% excludeClass) )) )
        dataset <- data[!whichToExclude]
    }else{
        dataset <- data
    }

    ### Make sure that ordering is correct!
    value = c("medianImpute", "bagImpute", "knnImpute", "expoTrans", "YeoJohnson", "BoxCox", "center", "scale", "range", "ica", "spatialSign", "zv", "nzv", "conditionalX", "pca", "corr")
    processing_values <- data.frame(value, stringsAsFactors=FALSE)
    processing_values$order <- as.numeric(row.names(processing_values))

    methods_sorted <- processing_values %>% filter(value %in% methods) %>% arrange(order) %>% select(value)
    methods_sorted <- methods_sorted$value

    transformations <- paste(methods_sorted, sep=",", collapse = ",")
    message <- paste0("===> INFO: Pre-processing transformation sorted (",transformations,") \r\n")
    cat(message)

    if(length(colnames(dataset)) < 2){
        message <- paste0("===> INFO: Pre-processing less than 2 columns detected removing some preprocessing methods\r\n")
        cat(message)
        return(NULL)
    }

    # calculate the pre-process parameters from the dataset
    if(!is.null(outcome)){
        preprocessParams <- preProcess(dataset, method = methods_sorted, outcome = outcome, n.comp = 25, verbose = TRUE, cutoff = 0.5)    
    }else{
        preprocessParams <- preProcess(dataset, method = methods_sorted, n.comp = 25, verbose = TRUE)   
    }
    # transform the dataset using the parameters
    processedMat <- predict(preprocessParams, newdata=dataset)

    if(!is.null(excludeClasses)){
        # summarize the transformed dataset
        processedMat[excludeClasses] <- data[excludeClasses]
    }
    message <- paste0("===> INFO: Pre-processing done!\r\n")
    cat(message)
    
    return(list(processedMat = processedMat, preprocessParams = preprocessParams))
}

#' Cast All Strings to NA
#'
#' Removes all strings/words from specified columns in a dataset
#'
#' @param dataset Dataframe to be processed
#' @param excludeColumns Character vector of column names to exclude
#'
#' @return A dataframe with all strings in non-excluded columns cast to NA
#' @export
castAllStringsToNA <- function(dataset, excludeColumns = c()){

    # Check if there are any non-numeric values in dataset except in excludeColumns
    includedColumns <- setdiff(colnames(dataset), excludeColumns)

    # If there are any non-numeric values in dataset except in excludeColumns, cast them all to NA
    suppressWarnings({
        dataset[includedColumns] <- lapply(dataset[includedColumns], function(column) {
            as.numeric(column) # Will convert non-numeric values to NA with a warning
        })
    })

    # Return the modified dataset
    return(dataset)
}


#' Is Numeric
#' 
#' Determines whether a variable is a number or a numeric string
#' 
#' @param x Variable to be checked
#' 
#' @return Logical indicating whether x is numeric and non-NA
#' @export
isNumeric <- function(x) {
	is.numeric(x) & !is.na(x)
}

#' Check if Request Variable is Empty
#'
#' This function checks whether a given variable is empty. It returns TRUE if the variable is empty; otherwise it returns FALSE.
#'
#' @param variable The input variable to check.
#' @return A boolean value indicating whether the variable is empty.
#' @importFrom rlang is_empty
#' @export
is_var_empty <- function(variable){
    is_empty <- FALSE

    if(length(variable) == 0){
        is_empty <- TRUE
    }else if(!is.null(variable) & rlang::is_empty(variable)){
        is_empty <- TRUE
    }else if(is.null(variable)){
        is_empty <- TRUE
    }

    if(is_empty == FALSE && !is.vector(variable) && !is.data.frame(variable)){
        print(variable)
        if(variable == ""){
            is_empty <- TRUE
        }
    }

    return(is_empty)
}

#' Generate a File Header
#'
#' This function generates a fileHeader object from a given data frame 
#' which includes original names and remapped names of the data frame columns.
#'
#' @param dataset The input data frame.
#' 
#' @return A data frame containing original and remapped column names.
#' @export
generate_file_header <- function(dataset) {
  
  ## create a data frame with original file names
  fileHeader <- data.frame('original' = colnames(dataset))
  
  ## create new remapped file names
  remappedNames <- paste0('column', seq_along(colnames(dataset)) - 1) # Subtract 1 to start from column0
  
  ## add the remapped names to the fileHeader data frame
  fileHeader$remapped <- remappedNames
  
  return(fileHeader)
}
