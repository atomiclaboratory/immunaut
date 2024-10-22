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

#' Preprocess a Dataset Using Specified Methods
#'
#' This function preprocesses a dataset by applying a variety of transformation methods, 
#' such as centering, scaling, or imputation. Users can also specify columns to exclude 
#' from preprocessing. The function supports a variety of preprocessing methods, including 
#' dimensionality reduction and imputation techniques, and ensures proper method application order.
#'
#' @param data A data frame or matrix representing the dataset to be preprocessed.
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
#'        - Other methods such as `"BoxCox"`, `"YeoJohnson"`, `"range"`, etc.
#'
#' @importFrom caret preProcess
#' @importFrom dplyr filter arrange select %>%
#' @importFrom stats predict
#'
#' @return A list containing:
#' - `processedMat`: The preprocessed dataset.
#' - `preprocessParams`: The preprocessing parameters that were applied to the dataset.
#'
#' @details
#' The function applies various transformations to the dataset as specified by the user. It ensures 
#' that methods are applied in the correct order to maintain data integrity and consistency. If fewer 
#' than two columns remain after excluding specified columns, the function halts and returns `NULL`. 
#' The function also handles categorical columns by skipping their transformation. Users can also 
#' specify outcome variables for specialized preprocessing.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' result <- preProcessData(iris[, 1:4], excludeClasses = "Sepal.Length", 
#'                          methods = c("center", "scale"))
#' print(result$processedMat)
#' }
#'
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
#' @examples
#' data <- data.frame(A = c("1", "2", "apple"), B = c("3", "banana", "4"), stringsAsFactors = TRUE)
#' cleaned_data <- castAllStringsToNA(data, excludeColumns = c("B"))
#' print(cleaned_data)
#' 
#' @export
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
#' @return boolean TRUE if the variable is considered empty, FALSE otherwise.
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

    message(paste0("====> Optimal resolution: ", optimal_resolution, " with modularity: ", best_modularity, " and clusters: ", best_clusters))

    return(list(optimal_resolution = optimal_resolution, best_modularity = best_modularity, best_clusters = best_clusters))
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
#' \dontrun{
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
  # Set seed for reproducibility
  set.seed(1337)
  
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
      mean_val <- runif(1, min = -20, max = 20)  # Mean closer to each other for more overlap
      sd_val <- cluster_overlap_sd                # Standard deviation to control overlap
      feature_column[cluster_labels == cluster] <- rnorm(cluster_size, mean = mean_val, sd = sd_val)
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
#' @examples
#' \dontrun{
#' # Example usage
#' dataset <- data.frame(pandora_cluster = c(1, 2, 100, 3, 100), values = 1:5)
#' settings <- list(datasetAnalysisRemoveOutliersDownstream = TRUE)
#' clean_data <- remove_outliers(dataset, settings)
#' print(clean_data)
#' }
remove_outliers <- function(dataset, settings) {
    if(settings$datasetAnalysisRemoveOutliersDownstream == TRUE) {
        print("===> INFO: Trying to remove outliers from dataset")
        if("pandora_cluster" %in% names(dataset)) {
            if(100 %in% dataset$pandora_cluster) {
                dataset <- dataset[dataset$pandora_cluster != 100, ]
                print("===> INFO: Rows with pandora_cluster == 100 have been removed.")
            } else {
                print("===> INFO: Cluster 100 does not exist in pandora_cluster.")
            }
        } else {
            print("===> INFO: No outliers detected")
        }
    }
    return(dataset)
}

#' Plot Clustered t-SNE Results
#'
#' This function generates a t-SNE plot with cluster assignments using consistent color mappings. 
#' It includes options for plotting points based on their t-SNE coordinates and adding cluster 
#' labels at the cluster centroids. The plot is saved as an SVG file in a temporary directory.
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
#' @importFrom ggplot2 ggplot aes geom_point geom_label labs theme theme_classic scale_color_manual element_text element_rect theme_set unit
#' @importFrom grDevices svg dev.off colorRampPalette
#' @importFrom RColorBrewer brewer.pal

#' @examples
#' \dontrun{
#' # Example usage
#' plot <- plot_clustered_tsne(info.norm, cluster_data, settings)
#' print(plot)
#' }
#' @export
plot_clustered_tsne <- function(info.norm, cluster_data, settings){
    # Ensure the theme is a valid ggplot2 theme
    if (!exists(settings$theme, envir = asNamespace("ggplot2"))) {
        message(paste0("Invalid ggplot2 theme: ", settings$theme, ". Using 'theme_classic' instead."))
        settings$theme <- "theme_classic"
    }
    
    # Apply the theme using ggplot2 namespace
    theme_to_apply <- get(settings$theme, envir = asNamespace("ggplot2"))(base_size = settings$fontSize)
    theme_set(theme_to_apply)

    info.norm$pandora_cluster <- as.character(info.norm$pandora_cluster)
    info.norm$pandora_cluster <- as.numeric(info.norm$pandora_cluster)

    cluster_data$pandora_cluster <- as.character(cluster_data$pandora_cluster)
    cluster_data$pandora_cluster <- as.numeric(cluster_data$pandora_cluster)

    # Convert 'cluster' to a factor with consistent levels in both data frames
    unique_clusters <- sort(unique(c(info.norm$pandora_cluster, cluster_data$pandora_cluster)))

    info.norm$pandora_cluster <- factor(info.norm$pandora_cluster, levels = unique_clusters)
    cluster_data$pandora_cluster <- factor(cluster_data$pandora_cluster, levels = unique_clusters)

    colorsTemp <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(min(8, length(unique_clusters)), settings$colorPalette)
    )(length(unique_clusters))

    # Create the plot with consistent color mapping
    plotData <- ggplot(info.norm, aes(x = tsne1, y = tsne2)) + 
                    geom_point(aes(color = pandora_cluster), size = settings$pointSize, alpha = 0.7) +  # Color by cluster for points
                    scale_color_manual(values = colorsTemp) +  # Use Brewer palette for consistent color scale
                    labs(x = "t-SNE dimension 1", y = "t-SNE dimension 2", color = "Cluster") +  # Label axes and legend
                    theme_classic(base_size = settings$fontSize) +  # Use a classic theme as base
                    theme(legend.position = settings$legendPosition,  # Adjust legend position
                          legend.background = element_rect(fill = "white", colour = "black"),  # Legend background
                          legend.key.size = unit(0.5, "cm"),  # Size of legend keys
                          legend.title = element_text(face = "bold"),  # Bold legend title
                          plot.background = element_rect(fill = "white", colour = NA),  # White plot background
                          axis.title.x = element_text(size = settings$fontSize * 1.2),  # Increase X axis label size
                          axis.title.y = element_text(size = settings$fontSize * 1.2))  # Increase Y axis label size

    # Adding cluster center labels with the same color mapping
    plotData <- plotData +
                geom_label(data = cluster_data, aes(x = tsne1, y = tsne2, label = as.character(label), color = pandora_cluster),
                           fill = "white",  # Background color of the label; adjust as needed
                           size = settings$fontSize / 2,  # Adjust text size within labels as needed
                           fontface = "bold",  # Make text bold
                           show.legend = FALSE)  # Do not show these labels in the legend

    return(plotData)
}


#' Auto ML Model Building with Caret
#'
#' This function automates the process of building machine learning models using the caret package. 
#' It supports both binary and multi-class classification and trains models based on a user-specified 
#' list of machine learning algorithms. The function splits the dataset into training and testing sets, 
#' trains the models using cross-validation, and computes performance metrics such as confusion matrix, 
#' AUROC (for binary classification), and prAUC (for binary classification).
#'
#' @param dataset_ml A data frame containing the dataset for training. All columns except the outcome 
#'   column should contain the features.
#' @param outcome A string specifying the name of the outcome column in the dataset. The outcome must 
#'   be a factor with two or more levels.
#' @param selectedPackages A character vector specifying the caret machine learning models to train. 
#'   For example, \code{c("nb", "rpart")}.
#' @param selectedPartitionSplit A numeric value between 0 and 1 specifying the proportion of the dataset 
#'   to use for training. Defaults to 0.7 (70%).
#'
#' @details
#' The function handles binary and multi-class classification problems and trains models using the caret 
#' package. It performs preprocessing (scaling, centering, and imputation of missing values), trains models 
#' using cross-validation, and calculates performance metrics including confusion matrix, post-resampling, 
#' and AUROC for binary classification problems.
#'
#' The function automatically handles splitting the data into training and testing sets using the specified 
#' partition split. The performance metrics for binary classification problems include AUROC and prAUC. For 
#' multi-class classification, these metrics are not computed, but confusion matrices and post-resampling 
#' statistics are provided.
#'
#' @return A list where each element is a trained model corresponding to the algorithms specified in 
#'   \code{selectedPackages}. Each element contains:
#'   \itemize{
#'     \item{\code{info}}: General information about the model, including resampling indices, problem type, 
#'         and outcome mapping.
#'     \item{\code{training}}: The trained model and variable importance.
#'     \item{\code{predictions}}: Predictions on the test set, including processed probabilities, confusion 
#'         matrix, and performance metrics such as AUROC and prAUC (if applicable).
#'   }
#'
#' @importFrom caret train createDataPartition trainControl confusionMatrix varImp postResample
#' @importFrom pROC roc auc
#' @importFrom PRROC pr.curve
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' dataset_ml <- generate_demo_data(n_subjects = 1000, n_features = 200)
#' selectedPackages <- c("nb", "rpart")
#' outcome <- "pandora_cluster"
#' results <- auto_simon_ml(dataset_ml, outcome, selectedPackages, 0.7)
#' }
#'
#' @keywords internal
auto_simon_ml <- function(dataset_ml, outcome, selectedPackages, selectedPartitionSplit = 0.7) {
  set.seed(1337)  # Set seed for reproducibility

  # Create an empty list to store model results
  model_list <- list()

  # Prepare data
  outcome_col <- dataset_ml[[outcome]]

  # Encode outcome to factor for classification and apply make.names to the levels
  if (!is.factor(outcome_col)) {
    outcome_col <- as.factor(outcome_col)
  }

  # Ensure factor levels are valid R variable names
  levels(outcome_col) <- make.names(levels(outcome_col))

  # Split the data into training and testing sets
  trainIndex <- caret::createDataPartition(outcome_col, p = selectedPartitionSplit, list = FALSE)
  trainData <- dataset_ml[trainIndex, ]
  testData <- dataset_ml[-trainIndex, ]

  # Determine if the problem is binary or multi-class classification
  is_binary_classification <- length(unique(outcome_col)) == 2

  # Iterate through the selected packages (models) and train them
  for (model_name in selectedPackages) {
    print(paste("Training model:", model_name))

    # Define the control object for cross-validation
    trainControlObj <- caret::trainControl(
      method = "cv",  # Cross-validation
      number = 5,     # 5-fold CV
      savePredictions = "final",  # Save predictions for final model
      classProbs = TRUE,  # Compute class probabilities
      summaryFunction = if (is_binary_classification) caret::twoClassSummary else caret::multiClassSummary
    )

    # Train the model
    trained_model <- caret::train(
      as.formula(paste(outcome, "~ .")),
      data = trainData,
      method = model_name,  # e.g., "nb" for naive Bayes
      trControl = trainControlObj,
      metric = "ROC",  # Use ROC as a performance metric
      preProcess = c("center", "scale", "medianImpute")  # Preprocessing
    )

    # Make predictions on the test set
    predictions <- predict(trained_model, newdata = testData)
    probabilities <- predict(trained_model, newdata = testData, type = "prob")

    # Calculate performance metrics
    prediction_confusion_matrix <- caret::confusionMatrix(predictions, testData[[outcome]])
    post_resample <- caret::postResample(predictions, testData[[outcome]])

    # For binary classification, calculate AUROC and prAUC
    if (is_binary_classification) {
      roc_obj <- pROC::roc(testData[[outcome]], probabilities[, 2])
      auroc <- pROC::auc(roc_obj)
      prAUC <- PRROC::pr.curve(
        scores.class0 = probabilities[, 2],
        weights.class0 = testData[[outcome]] == levels(testData[[outcome]])[2]
      )$auc.integral
    } else {
      auroc <- NA  # AUROC isn't typically computed for multi-class, so set to NA
      prAUC <- NA  # Likewise, prAUC isn't used for multi-class
    }

    # Store the model details in the model list
    model_list[[model_name]] <- list(
      info = list(
        resampleID = trainIndex,
        problemType = if (is_binary_classification) "Binary Classification" else "Multi-Class Classification",
        data = trainData,
        outcome = outcome,
        outcome_mapping = levels(outcome_col)
      ),
      training = list(
        raw = trained_model,
        varImportance = caret::varImp(trained_model)
      ),
      predictions = list(
        raw = predictions,
        processed = probabilities,
        prAUC = prAUC,
        AUROC = auroc,
        postResample = post_resample,
        confusionMatrix = prediction_confusion_matrix
      )
    )

    print(paste("Finished training model:", model_name))
  }

  # Return the list of models with details
  return(model_list)
}
