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
#' @importFrom stats predict
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
    processedMat <- stats::predict(preprocessParams, newdata=dataset)

    if(!is.null(excludeClasses)){
        # summarize the transformed dataset
        processedMat[excludeClasses] <- data[excludeClasses]
    }
    message <- paste0("===> INFO: Pre-processing done!\r\n")
    cat(message)
    
    return(list(processedMat = processedMat, preprocessParams = preprocessParams))
}

#' @title castAllStringsToNA   
#' @description Removes all strings/words from specified columns in dataset
#' @param dataset dataframe
#' @param excludeColumns character
#' @return dataframe
castAllStringsToNA <- function(dataset, excludeColumns = c()) {
    # Validate inputs
    if (!is.data.frame(dataset))  {
        stop("=====> ERROR: The 'dataset' must be a dataframe.")
    }

    if (!is.character(excludeColumns)) {
        stop("=====> ERROR: castAllStringsToNA The 'excludeColumns' must be a character vector.")
    }
    
    # Identify columns to process
    includedColumns <- setdiff(names(dataset), excludeColumns)

    # Process each included column
    dataset[includedColumns] <- lapply(dataset[includedColumns], function(column) {
        if (is.factor(column)) {
            column <- as.character(column)
        }
        if (is.character(column)) {
            # Convert all non-numeric strings to NA
            suppressWarnings(as.numeric(column))
        } else {
            # Leave columns of other types unchanged
            column
        }
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

#' @title Check if request variable is Empty
#' @description Checks if the given variable is empty and optionally logs the variable name.
#' @param variable The variable to check.
#' @param variable_name Optional; the name of the variable to log.
#' @return boolean TRUE if the variable is considered empty, FALSE otherwise.
is_var_empty <- function(variable, variable_name = NULL){
    # Initialize is_empty as FALSE
    is_empty <- FALSE
    
    # Check for different conditions that qualify the variable as empty
    if(length(variable) == 0 || rlang::is_null(variable)){
        is_empty <- TRUE
    } else if (is.character(variable) && length(variable) == 1 && variable == "") {
        is_empty <- TRUE
    } else if (rlang::is_empty(variable)) {
        is_empty <- TRUE
    }

    # Optionally print variable name
    if (!is.null(variable_name)) {
        print(paste0("=====> INFO: Variable '", variable_name, "' is_empty: ", is_empty))
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


#' Find Optimal Resolution for Louvain Clustering
#'
#' This function finds the optimal resolution for Louvain clustering by iterating over a 
#' range of resolution values, balancing modularity and the number of clusters. The function 
#' aims to find a resolution that produces a reasonable number of clusters while maintaining 
#' high modularity.
#'
#' @param graph An igraph object representing the graph to be clustered.
#' @param start_resolution Numeric. The starting resolution for the Louvain algorithm. Default is 0.1.
#' @param end_resolution Numeric. The maximum resolution to test. Default is 10.
#' @param min_modularity Numeric. The minimum acceptable modularity for valid clusterings. Default is 0.3.
#' @param target_clusters_range Numeric vector of length 2. The range of acceptable cluster numbers (inclusive). Default is c(3, 6).
#'
#' @return A list containing:
#'   \item{optimal_resolution}{The resolution that balances modularity and number of clusters.}
#'   \item{best_modularity}{The modularity at the optimal resolution.}
#'   \item{best_clusters}{The number of clusters at the optimal resolution.}
#'
#' @details
#' The function iterates through different resolutions, performing Louvain clustering at each step, 
#' and records the number of clusters and modularity. It then selects the resolution that provides 
#' a good balance between a reasonable number of clusters and high modularity. 
#' The user can set the desired range for the number of clusters.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   g <- make_ring(10)  # igraph object representing the graph to be clustered
#'   result <- find_optimal_resolution(g, start_resolution = 0.1, 
#'                                    end_resolution = 2, 
#'                                    min_modularity = 0.4)
#'   print(result)
#' }
#'
#' @export
find_optimal_resolution <- function(graph, start_resolution = 0.1, end_resolution = 10, min_modularity = 0.3, target_clusters_range = c(3, 6)) {
    optimal_resolution <- NA
    best_modularity <- -1  # Initialize best modularity to a very low value
    best_clusters <- NA  # Track the best number of clusters
    res <- start_resolution
    
    # Iterate over resolutions from start_resolution to end_resolution
    while (res <= end_resolution) {
        lc <- igraph::cluster_louvain(graph, resolution = res)  # Perform Louvain clustering
        modularity_value <- igraph::modularity(lc)  # Calculate modularity
        num_clusters <- length(unique(igraph::membership(lc)))  # Get the number of clusters
        print(paste0("====> Clusters detected: ", num_clusters, " Resolution: ", res, " with modularity: ", modularity_value))
        
        # Check if modularity is above threshold and the number of clusters is within the target range
        if (modularity_value >= min_modularity && num_clusters >= target_clusters_range[1] && num_clusters <= target_clusters_range[2]) {
            # Update best resolution if this configuration provides a good balance
            if (modularity_value > best_modularity) {
                best_modularity <- modularity_value
                best_clusters <- num_clusters
                optimal_resolution <- res
            }
        }
        
        # Increment resolution by 0.1 for the next iteration
        res <- res + 0.1
    }
    
    return(list(optimal_resolution = optimal_resolution, best_modularity = best_modularity, best_clusters = best_clusters))
}
