#' Pre-process and Resample Dataset
#'
#' This function applies pre-processing transformations to the dataset, then resamples it.
#' Pre-processing parameters are estimated strictly from the training dataset (`datasetData`),
#' and optionally applied to a separate test dataset (`testData`) to prevent data leakage.
#'
#' @param datasetData Dataframe to be pre-processed (e.g. training data).
#' @param preProcess Vector of pre-processing methods to apply.
#' @param selectedOutcomeColumns Character vector of outcome columns.
#' @param outcome_and_classes List of outcomes and their classes.
#' @param settings A named list containing settings for the analysis. If NULL, defaults will be used. The settings list may contain:
#'        - `seed`: An integer seed value for reproducibility.
#' @param testData Optional dataframe of test data to be pre-processed using the parameters estimated from `datasetData`. Default is NULL.
#'
#' @return A list containing:
#' - `preProcessMapping`: The pre-processing mapping (e.g. PCA rotation matrix if applicable).
#' - `datasetData`: The pre-processed training dataset.
#' - `testData`: The pre-processed test dataset (if `testData` was provided, otherwise NULL).
#' - `preprocessParams`: A list containing the fitted preprocessing parameter objects (`impute` and `transform`).
#' @keywords internal
preProcessResample <- function(datasetData, preProcess, selectedOutcomeColumns, outcome_and_classes, settings, testData = NULL){
    # ==> 2 PREPROCCESING: Skewness and normalizing of the numeric predictors
    preProcessMapping <- NULL
    preProcessedData_imp <- NULL
    preProcessedData_no_imp <- NULL
    preprocessParams_impute <- NULL
    preprocessParams_no_impute <- NULL

    if(length(preProcess) > 0 ){
        transformations <- paste(preProcess, sep=",", collapse = ",")
        message(paste0("===> INFO: Pre-processing transformation(s) (",transformations,") \r\n"))

        impute_idx <- grepl("impute", tolower(preProcess), fixed = FALSE)

        methods_impute <- preProcess[impute_idx]
        methods_no_impute <- preProcess[!impute_idx]

        message(paste0("===> INFO: Pre-processing methods_impute: ",length(methods_impute)," methods_no_impute ",length(methods_no_impute),"\r\n"))

        if(length(methods_impute) > 0){
            preProcessedData_imp <- preProcessData(datasetData, selectedOutcomeColumns, outcome_and_classes, methods_impute, settings, testData = testData)
            if(!is.null(preProcessedData_imp)){
                datasetData <- preProcessedData_imp$processedMat
                if (!is.null(testData)) {
                    testData <- preProcessedData_imp$processedTestMat
                }
                preprocessParams_impute <- preProcessedData_imp$preprocessParams
            }
        }

        if(length(methods_no_impute) > 0){
            preProcessedData_no_imp <- preProcessData(datasetData, selectedOutcomeColumns, outcome_and_classes, methods_no_impute, settings, testData = testData)
            if(!is.null(preProcessedData_no_imp)){
                datasetData <- preProcessedData_no_imp$processedMat
                if (!is.null(testData)) {
                    testData <- preProcessedData_no_imp$processedTestMat
                }
                preprocessParams_no_impute <- preProcessedData_no_imp$preprocessParams

                if("pca" %in% methods_no_impute && !is.null(preProcessedData_no_imp$preprocessParams)){
                    preProcessMapping <- preProcessedData_no_imp$preprocessParams$rotation
                }
            }
        }

        if(is.null(preProcessedData_imp) && is.null(preProcessedData_no_imp)){
            message(paste0("===> INFO: Could not apply preprocessing transformations, continuing without preprocessing.. \r\n"))
        }
    }

    return(list(
        preProcessMapping = preProcessMapping,
        datasetData = datasetData,
        testData = testData,
        preprocessParams = list(
            impute = preprocessParams_impute,
            transform = preprocessParams_no_impute
        )
    ))
}

#' Preprocess a Dataset Using Specified Methods
#'
#' This function preprocesses a dataset by applying a variety of transformation methods, 
#' such as centering, scaling, or imputation. Users can also specify columns to exclude 
#' from preprocessing. Preprocessing parameters are estimated strictly from the training 
#' dataset (`data`) and optionally applied to a test dataset (`testData`) to prevent data leakage.
#'
#' @param data A data frame or matrix representing the dataset to be preprocessed (e.g. training set).
#' @param outcome A character string representing the outcome variable, if any, 
#'        for outcome-based transformations.
#' @param excludeClasses A character vector specifying the column names to exclude from 
#'        preprocessing. Default is `NULL`, meaning all columns are included in the preprocessing.
#' @param methods A character vector specifying the preprocessing methods to apply. 
#'        Default methods are `c("center", "scale")`. Available methods include:
#'        - `"medianImpute"`: Impute missing values with the median.
#'        - `"bagImpute"`: Impute missing values using bootstrap aggregation.
#'        - `"knnImpute"`: Impute missing values using k-nearest neighbors.
#'        - `"center"`: Subtract the mean from each feature.
#'        - `"scale"`: Divide features by their standard deviation.
#'        - `"pca"`: Principal Component Analysis for dimensionality reduction.
#'        - Other methods such as `"BoxCox"`, `"YeoJohnson"`, `"range"`, `"zv"`, `"nzv"`, `"corr"`.
#' @param settings A named list containing settings for the analysis. If NULL, defaults will be used. The settings list may contain:
#'        - `seed`: An integer seed value for reproducibility.
#' @param testData Optional data frame representing the test set to be transformed using the parameters fitted on `data`. Default is `NULL`.
#'
#' @importFrom caret preProcess
#' @importFrom dplyr filter arrange select %>%
#' @importFrom stats predict
#'
#' @return A list containing:
#' - `processedMat`: The preprocessed training dataset.
#' - `processedTestMat`: The preprocessed test dataset (if `testData` was provided, otherwise NULL).
#' - `preprocessParams`: The preprocessing parameters that were applied to the dataset.
#'
#' @details
#' The function applies various transformations to the dataset as specified by the user. It ensures 
#' that methods are applied in the correct order to maintain data integrity and consistency. If fewer 
#' than two columns remain after excluding specified columns, the function halts and returns `NULL`. 
#' The function also handles categorical columns by skipping their transformation. Users can also 
#' specify outcome variables for specialized preprocessing.
#'
#' @keywords internal
preProcessData <- function(data, outcome, excludeClasses, methods = c("center", "scale"), settings, testData = NULL)
{
    set.seed(settings$seed)
    if(length(methods) == 0){
        methods <- c("center", "scale")
    }

    # Identify columns to exclude from preprocessing (e.g., outcome or grouping variables)
    whichToExclude <- character(0)
    if(!is.null(excludeClasses)){
        whichToExclude <- intersect(colnames(data), unlist(excludeClasses))
        dataset <- data[, !colnames(data) %in% whichToExclude, drop = FALSE]
        if (!is.null(testData)) {
            test_dataset <- testData[, !colnames(testData) %in% whichToExclude, drop = FALSE]
        }
    }else{
        dataset <- data
        if (!is.null(testData)) {
            test_dataset <- testData
        }
    }

    ### Make sure that ordering is correct!
    value = c("medianImpute", "bagImpute", "knnImpute", "expoTrans", "YeoJohnson", "BoxCox", "center", "scale", "range", "ica", "spatialSign", "zv", "nzv", "conditionalX", "pca", "corr")
    processing_values <- data.frame(value, stringsAsFactors=FALSE)
    processing_values$order <- as.numeric(row.names(processing_values))

    methods_sorted <- processing_values %>% filter(value %in% methods) %>% arrange(order) %>% select(value)
    methods_sorted <- methods_sorted$value
    if(length(methods_sorted) == 0){
        methods_sorted <- methods
    }

    transformations <- paste(methods_sorted, sep=",", collapse = ",")

    message(paste0("===> INFO: Pre-processing transformation sorted (",transformations,")"))

    if(ncol(dataset) < 1){
        message("===> INFO: Pre-processing 0 columns detected, returning NULL")
        return(NULL)
    }

    if(ncol(dataset) < 2){
        message("===> INFO: Pre-processing less than 2 columns detected; removing multi-column methods")
        methods_sorted <- setdiff(methods_sorted, c("pca", "corr", "ica"))
        if(length(methods_sorted) == 0){
            return(NULL)
        }
    }

    # calculate the pre-process parameters strictly from the training dataset
    outcome_vec <- NULL
    if (!is.null(outcome)) {
        if (is.character(outcome) && length(outcome) == 1 && !is.na(outcome)) {
            if (outcome %in% colnames(data)) {
                outcome_vec <- data[[outcome]]
            } else if (outcome %in% colnames(dataset)) {
                outcome_vec <- dataset[[outcome]]
            }
        } else if ((is.numeric(outcome) || is.factor(outcome)) && length(outcome) == nrow(dataset)) {
            outcome_vec <- outcome
        }
    }

    if (!is.null(outcome_vec)) {
        preprocessParams <- caret::preProcess(dataset, method = methods_sorted, outcome = outcome_vec, n.comp = 25, verbose = FALSE, cutoff = 0.5)    
    } else {
        preprocessParams <- caret::preProcess(dataset, method = methods_sorted, n.comp = 25, verbose = FALSE)   
    }

    # transform the training dataset using the parameters
    processedMat <- stats::predict(preprocessParams, newdata=dataset)

    # restore excluded columns to training dataset
    if(length(whichToExclude) > 0){
        for (col in whichToExclude) {
            processedMat[[col]] <- data[[col]]
        }
    }
    if (all(colnames(data) %in% colnames(processedMat))) {
        processedMat <- processedMat[, colnames(data), drop = FALSE]
    } else {
        kept_cols <- colnames(data)[colnames(data) %in% colnames(processedMat)]
        extra_cols <- setdiff(colnames(processedMat), colnames(data))
        processedMat <- processedMat[, c(kept_cols, extra_cols), drop = FALSE]
    }

    # transform the test dataset if provided, using the exact parameters fitted on training data
    processedTestMat <- NULL
    if (!is.null(testData)) {
        processedTestMat <- stats::predict(preprocessParams, newdata=test_dataset)
        if (length(whichToExclude) > 0) {
            for (col in whichToExclude) {
                processedTestMat[[col]] <- testData[[col]]
            }
        }
        if (all(colnames(testData) %in% colnames(processedTestMat))) {
            processedTestMat <- processedTestMat[, colnames(testData), drop = FALSE]
        } else {
            kept_cols_t <- colnames(testData)[colnames(testData) %in% colnames(processedTestMat)]
            extra_cols_t <- setdiff(colnames(processedTestMat), colnames(testData))
            processedTestMat <- processedTestMat[, c(kept_cols_t, extra_cols_t), drop = FALSE]
        }
    }

    message(paste0("===> INFO: Pre-processing done!"))
    
    return(list(processedMat = processedMat, processedTestMat = processedTestMat, preprocessParams = preprocessParams))
}

#' @title Cast All Strings to NA
#' 
#' @description
#' This function processes the columns of a given dataset, converting all non-numeric string values 
#' (including factor columns converted to character) to `NA`. It excludes specified columns from 
#' this transformation. Columns that are numeric or of other types are left unchanged.
#' 
#' @param dataset A data frame containing the dataset to be processed.
#' @param excludeColumns A character vector specifying the names of columns to be excluded from processing. 
#' These columns will not have any values converted to `NA`.
#' 
#' @return A data frame where non-numeric strings in the included columns are replaced with `NA`, and all other columns remain unchanged.
#' 
#' @details
#' The function iterates through the specified columns (excluding those listed in `excludeColumns`), 
#' converts factors to character, and then attempts to convert character values to numeric. 
#' Any non-numeric strings will be converted to `NA`. This is useful for cleaning datasets that may contain
#' mixed data types.
#' 
#' @keywords internal
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
#' @keywords internal
isNumeric <- function(x) {
	is.numeric(x) & !is.na(x)
}

#' @title Check if request variable is Empty
#' @description Checks if the given variable is empty and optionally logs the variable name.
#' @param variable The variable to check.
#' @return boolean TRUE if the variable is considered empty, FALSE otherwise.
#' @keywords internal
is_var_empty <- function(variable){
    if (length(variable) == 0) return(TRUE)
    if (is.null(variable)) return(TRUE)
    if (rlang::is_empty(variable)) return(TRUE)
    if (is.character(variable) && length(variable) == 1 && (is.na(variable) || variable == "")) return(TRUE)
    return(FALSE)
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
#' This function iterates over a range of resolution values to find the optimal resolution for 
#' Louvain clustering, balancing the number of clusters and modularity. It aims to identify a 
#' resolution that results in a reasonable number of clusters while maintaining a high modularity score.
#'
#' @param graph An \code{igraph} object representing the graph to be clustered.
#' @param start_resolution Numeric. The starting resolution for the Louvain algorithm. Default is 0.1.
#' @param end_resolution Numeric. The maximum resolution to test. Default is 10.
#' @param resolution_increment Numeric. The increment to adjust the resolution at each step. Default is 0.1.
#' @param min_modularity Numeric. The minimum acceptable modularity for valid clusterings. Default is 0.3.
#' @param target_clusters_range Numeric vector of length 2. Specifies the acceptable range for the number of clusters (inclusive). Default is \code{c(3, 6)}.
#'
#' @return A list containing:
#' \item{selected}{A list with the optimal resolution, best modularity, and number of clusters.}
#' \item{frequent_clusters_results}{A data frame containing results for resolutions that yielded the most frequent number of clusters.}
#' \item{all_results}{A data frame with the resolution, number of clusters, and modularity for all tested resolutions.}
#'
#' @details
#' The function performs Louvain clustering at different resolutions, starting from \code{start_resolution} and 
#' ending at \code{end_resolution}, incrementing by \code{resolution_increment} at each step. At each resolution, 
#' the function calculates the number of clusters and modularity. The results are filtered to select those 
#' where modularity exceeds \code{min_modularity} and the number of clusters falls within the specified range 
#' \code{target_clusters_range}. The optimal resolution is chosen based on the most frequent number of clusters and 
#' the median resolution that satisfies these criteria.
#'
#' @importFrom igraph cluster_louvain modularity membership
#' @keywords internal
find_optimal_resolution <- function(graph, 
    start_resolution = 0.1, 
    end_resolution = 10, 
    resolution_increment = 0.1, 
    min_modularity = 0.3, 
    target_clusters_range = c(3, 6)) {
    results <- data.frame(
        resolution = numeric(),
        num_clusters = integer(),
        modularity = numeric(),
        stringsAsFactors = FALSE
    )
    
    res <- start_resolution
    
    min_clust <- min(target_clusters_range)
    max_clust <- max(target_clusters_range)

    # Iterate over resolutions from start_resolution to end_resolution
    while (res <= end_resolution) {
        lc <- igraph::cluster_louvain(graph, resolution = res)  # Perform Louvain clustering
        modularity_value <- igraph::modularity(lc)  # Calculate modularity
        num_clusters <- length(unique(igraph::membership(lc)))  # Get the number of clusters

        # Skip clusterings that are not within the target_clusters_range
        if (num_clusters < min_clust || num_clusters > max_clust) {
            res <- res + resolution_increment
            next
        }
        # Collect the results into a dataframe
        results <- rbind(results, data.frame(resolution = res, num_clusters = num_clusters, modularity = modularity_value))
        
        # Increment resolution by 0.1 for the next iteration
        res <- res + resolution_increment
    }
    
    # Filter results for modularity above threshold and number of clusters within the target range
    valid_results <- results[
        results$modularity >= min_modularity &
        results$num_clusters >= min_clust &
        results$num_clusters <= max_clust,
    ]
    
    if (nrow(valid_results) == 0) {
        message("===> INFO: No valid resolutions found")
        return(NULL)
    }
    
    # Find the most frequent number of clusters
    most_frequent_clusters <- as.numeric(names(sort(table(valid_results$num_clusters), decreasing = TRUE)[1]))
    
    # Subset the results where the number of clusters matches the most frequent one
    frequent_clusters_results <- valid_results[valid_results$num_clusters == most_frequent_clusters, ]
    
    # Find the median resolution from the frequent clusters subset
    median_resolution <- median(frequent_clusters_results$resolution)
    
    # Get the row with the median resolution
    best_row <- frequent_clusters_results[which.min(abs(frequent_clusters_results$resolution - median_resolution)), ]
    
    # Output the selected clustering result
    message(paste0("===> INFO: Selected resolution: ", best_row$resolution, 
                   " Modularity: ", best_row$modularity, 
                   " Clusters: ", best_row$num_clusters))

    return(list(
            selected = list(optimal_resolution = best_row$resolution, 
                best_modularity = best_row$modularity, 
                best_clusters = best_row$num_clusters),
            frequent_clusters_results = frequent_clusters_results,
            all_results = results
        ))
}


#' Generate a Demo Dataset with Specified Number of Clusters and Overlap
#'
#' This function generates a demo dataset with a specified number of subjects, features, 
#' and desired number of clusters, ensuring that the generated clusters are not too far apart 
#' and have some degree of overlap to simulate real-world data. 
#' The generated dataset includes demographic information (`outcome`, `age`, and `gender`), 
#' as well as numeric features with a specified probability of missing values.
#'
#' @param n_subjects Integer. The number of subjects (rows) to generate. Defaults to 1000.
#' @param n_features Integer. The number of features (columns) to generate. Defaults to 200.
#' @param missing_prob Numeric. The probability of introducing missing values (NA) in the feature columns. Defaults to 0.1.
#' @param desired_number_clusters Integer. The approximate number of clusters to generate in the feature space. Defaults to 3.
#' @param cluster_overlap_sd Numeric. The standard deviation to control cluster overlap. Defaults to 15 for more overlap.
#'
#' @return A data frame containing the generated demo dataset, with columns:
#' - `outcome`: A categorical variable with values "low" or "high".
#' - `age`: A numeric variable representing the age of the subject (range 18-90).
#' - `gender`: A categorical variable with values "male" or "female".
#' - `Feature X`: Numeric feature columns with random values and some missing data.
#'
#' @details
#' The function generates `n_features` numeric columns based on Gaussian clusters 
#' with some overlap between clusters to simulate more realistic data. Missing values are 
#' introduced in each feature column based on the `missing_prob`.
#'
#' @examples
#' \donttest{
#' # Generate a demo dataset with 1000 subjects, 200 features, and 3 clusters
#' demo_data <- generate_demo_data(n_subjects = 1000, n_features = 200, 
#'                                 desired_number_clusters = 3, 
#'                                 cluster_overlap_sd = 15, missing_prob = 0.1)
#' 
#' # View the first few rows of the dataset
#' head(demo_data)
#' }
#'
#' @export
generate_demo_data <- function(n_subjects = 1000, n_features = 200, missing_prob = 0.1, 
                               desired_number_clusters = 3, cluster_overlap_sd = 15) {
  
  # Define potential values for categorical variables
  outcomes <- c("low", "high")
  genders <- c("male", "female")
  
  # Generate demographic columns
  outcome <- sample(outcomes, n_subjects, replace = TRUE)
  age <- sample(18:90, n_subjects, replace = TRUE)
  gender <- sample(genders, n_subjects, replace = TRUE)
  
  # Generate cluster assignments
  cluster_labels <- sample(seq_len(desired_number_clusters), n_subjects, replace = TRUE)
  
  # Generate feature columns with Gaussian Mixture Model for each cluster
  feature_data <- replicate(n_features, {
    feature_column <- numeric(n_subjects)
    
    for (cluster in seq_len(desired_number_clusters)) {
      cluster_size <- sum(cluster_labels == cluster)
      mean_val <- stats::runif(1, min = -20, max = 20)  # Mean closer to each other for more overlap
      sd_val <- cluster_overlap_sd                # Standard deviation to control overlap
      feature_column[cluster_labels == cluster] <- stats::rnorm(cluster_size, mean = mean_val, sd = sd_val)
    }
    
    # Introduce missing values
    feature_column[sample(seq_len(n_subjects), size = floor(missing_prob * n_subjects))] <- NA
    return(feature_column)
  })
  
  # Name the features
  feature_names <- paste0("Feature ", seq_len(n_features))
  feature_data <- as.data.frame(feature_data)
  colnames(feature_data) <- feature_names
  
  # Combine all into a data frame
  demo_data <- data.frame(outcome = outcome, age = age, gender = gender, feature_data)
  
  return(demo_data)
}


#' Remove Outliers Based on Cluster Information
#'
#' The `remove_outliers` function removes rows from a dataset based on the presence 
#' of outliers marked by a specific cluster ID (typically 100) in the `pandora_cluster` column.
#' This function is meant to be used internally during downstream dataset analysis 
#' to filter out data points that have been identified as outliers during clustering.
#'
#' @param dataset A data frame that includes clustering results, particularly a `pandora_cluster` column.
#' @param settings A list of settings. Must contain the logical value `datasetAnalysisRemoveOutliersDownstream`. 
#' If `datasetAnalysisRemoveOutliersDownstream` is TRUE, outliers (rows where `pandora_cluster == 100`) 
#' will be removed from the dataset.
#'
#' @return A filtered data frame with outliers removed if applicable.
#'
#' @keywords internal
remove_outliers <- function(dataset, settings) {
    if (isTRUE(settings$datasetAnalysisRemoveOutliersDownstream)) {
        message("===> INFO: Trying to remove outliers from dataset")
        if ("pandora_cluster" %in% names(dataset)) {
            outlier_rows <- which(dataset$pandora_cluster == 100 | dataset$pandora_cluster == "100")
            if (length(outlier_rows) > 0) {
                dataset <- dataset[-outlier_rows, , drop = FALSE]
                message("===> INFO: Rows with pandora_cluster == 100 have been removed.")
            } else {
                message("===> INFO: Cluster 100 does not exist in pandora_cluster.")
            }
        } else {
            message("===> INFO: No outliers detected")
        }
    }
    return(dataset)
}

#' Plot Clustered t-SNE Results
#'
#' This function generates a t-SNE plot with cluster assignments using consistent color mappings. 
#' It includes options for plotting points based on their t-SNE coordinates and adding cluster 
#' labels at the cluster centroids.
#'
#' @param info.norm A data frame containing t-SNE coordinates (`tsne1`, `tsne2`) and cluster assignments (`pandora_cluster`) for each point.
#' @param cluster_data A data frame containing the cluster centroids and labels, with columns `tsne1`, `tsne2`, `label`, and `pandora_cluster`.
#' @param settings A list of settings for the plot, including:
#'   - `theme`: The ggplot2 theme to use (e.g., `"theme_classic"`).
#'   - `colorPalette`: The color palette to use for clusters (e.g., `"RdPu"`).
#'   - `pointSize`: The size of points in the plot.
#'   - `fontSize`: The font size used in the plot.
#'   - `legendPosition`: The position of the legend (e.g., `"right"`).
#'   - `plot_size`: The size of the plot.
#'   - `aspect_ratio`: The aspect ratio of the plot.
#'
#' @return ggplot2 object representing the clustered t-SNE plot.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_label labs theme theme_classic scale_color_manual element_text element_rect unit
#' @importFrom grDevices svg dev.off colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot <- plot_clustered_tsne(info.norm, cluster_data, settings)
#' print(plot)
#' }
#' @export
plot_clustered_tsne <- function(info.norm, cluster_data, settings){
    font_size <- if (!is.null(settings$fontSize) && is.numeric(settings$fontSize)) settings$fontSize else 12
    point_size <- if (!is.null(settings$pointSize) && is.numeric(settings$pointSize)) settings$pointSize else 2
    legend_pos <- if (!is.null(settings$legendPosition)) settings$legendPosition else "right"
    palette_name <- if (!is.null(settings$colorPalette)) settings$colorPalette else "RdPu"

    # Theme selection (applied directly to ggplot, avoiding global theme_set side effects)
    theme_name <- if (!is.null(settings$theme) && exists(settings$theme, envir = asNamespace("ggplot2"))) {
        settings$theme
    } else {
        if (!is.null(settings$theme)) {
            message(paste0("Invalid ggplot2 theme: ", settings$theme, ". Using 'theme_classic' instead."))
        }
        "theme_classic"
    }
    base_theme <- get(theme_name, envir = asNamespace("ggplot2"))(base_size = font_size)

    # Preserve cluster assignments as character/factor without coercing strings to NA via as.numeric
    info.norm$pandora_cluster <- as.character(info.norm$pandora_cluster)
    cluster_data$pandora_cluster <- as.character(cluster_data$pandora_cluster)

    # Convert 'cluster' to a factor with consistent levels in both data frames
    unique_clusters <- unique(c(info.norm$pandora_cluster, cluster_data$pandora_cluster))
    unique_clusters <- unique_clusters[!is.na(unique_clusters)]

    suppressWarnings({
        num_check <- as.numeric(unique_clusters)
        if (!any(is.na(num_check))) {
            unique_clusters <- unique_clusters[order(num_check)]
        } else {
            unique_clusters <- sort(unique_clusters)
        }
    })

    info.norm$pandora_cluster <- factor(info.norm$pandora_cluster, levels = unique_clusters)
    cluster_data$pandora_cluster <- factor(cluster_data$pandora_cluster, levels = unique_clusters)

    n_colors <- max(1, length(unique_clusters))
    n_brewer <- min(8, max(3, n_colors))
    colorsTemp <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(n_brewer, palette_name)
    )(n_colors)

    # Create the plot with consistent color mapping
    plotData <- ggplot(info.norm, aes(x = tsne1, y = tsne2)) + 
                    geom_point(aes(color = pandora_cluster), size = point_size, alpha = 0.7) +
                    scale_color_manual(values = colorsTemp) +
                    labs(x = "t-SNE dimension 1", y = "t-SNE dimension 2", color = "Cluster") +
                    base_theme +
                    theme(legend.position = legend_pos,
                          legend.background = element_rect(fill = "white", colour = "black"),
                          legend.key.size = unit(0.5, "cm"),
                          legend.title = element_text(face = "bold"),
                          plot.background = element_rect(fill = "white", colour = NA),
                          axis.title.x = element_text(size = font_size * 1.2),
                          axis.title.y = element_text(size = font_size * 1.2))

    # Adding cluster center labels with the same color mapping
    plotData <- plotData +
                geom_label(data = cluster_data, aes(x = tsne1, y = tsne2, label = as.character(label), color = pandora_cluster),
                           fill = "white",
                           size = font_size / 2,
                           fontface = "bold",
                           show.legend = FALSE)

    return(plotData)
}

# Helper function to normalize scores with NA handling
#' @keywords internal
normalize <- function(x) {
    if (all(is.na(x))) return(rep(0.5, length(x)))
    rng <- range(x, na.rm = TRUE)
    if (rng[1] == rng[2]) return(rep(0.5, length(x)))
    (x - rng[1]) / (rng[2] - rng[1])
}
