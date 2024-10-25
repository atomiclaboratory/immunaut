#' Main function to carry out Immunaut Analysis
#'
#' This function performs clustering and dimensionality reduction analysis on a dataset using user-defined settings.
#' It handles various preprocessing steps, dimensionality reduction via t-SNE, multiple clustering methods, and
#' generates associated plots based on user-defined or default settings.
#'
#' @param dataset A data frame representing the dataset on which the analysis will be performed. The dataset must 
#' contain numeric columns for dimensionality reduction and clustering.
#' @param settings A named list containing settings for the analysis. If NULL, defaults will be used. The settings list may contain:
#' 
#' \describe{
#'   \item{fileHeader}{A data frame mapping the original column names to remapped column names. Used for t-SNE input preparation.}
#'   \item{selectedColumns}{Character vector of columns to be used for the analysis. Defaults to NULL.}
#'   \item{cutOffColumnSize}{Numeric. The maximum size of the dataset in terms of columns. Defaults to 50,000.}
#'   \item{excludedColumns}{Character vector of columns to exclude from the analysis. Defaults to NULL.}
#'   \item{groupingVariables}{Character vector of columns to use for grouping the data during analysis. Defaults to NULL.}
#'   \item{colorVariables}{Character vector of columns to use for coloring in the plots. Defaults to NULL.}
#'   \item{preProcessDataset}{Character vector of preprocessing methods to apply (e.g., scaling, normalization). Defaults to NULL.}
#'   \item{fontSize}{Numeric. Font size for plots. Defaults to 12.}
#'   \item{pointSize}{Numeric. Size of points in plots. Defaults to 1.5.}
#'   \item{theme}{Character. The ggplot2 theme to use (e.g., "theme_gray"). Defaults to "theme_gray".}
#'   \item{colorPalette}{Character. Color palette for plots (e.g., "RdPu"). Defaults to "RdPu".}
#'   \item{aspect_ratio}{Numeric. The aspect ratio of plots. Defaults to 1.}
#'   \item{clusterType}{Character. The clustering method to use. Options are "Louvain", "Hierarchical", "Mclust", "Density". Defaults to "Louvain".}
#'   \item{removeNA}{Logical. Whether to remove rows with NA values. Defaults to FALSE.}
#'   \item{datasetAnalysisGrouped}{Logical. Whether to perform grouped dataset analysis. Defaults to FALSE.}
#'   \item{plot_size}{Numeric. The size of the plot. Defaults to 12.}
#'   \item{knn_clusters}{Numeric. The number of clusters for KNN-based clustering. Defaults to 250.}
#'   \item{perplexity}{Numeric. The perplexity parameter for t-SNE. Defaults to NULL (automatically determined).}
#'   \item{exaggeration_factor}{Numeric. The exaggeration factor for t-SNE. Defaults to NULL.}
#'   \item{max_iter}{Numeric. The maximum number of iterations for t-SNE. Defaults to NULL.}
#'   \item{theta}{Numeric. The Barnes-Hut approximation parameter for t-SNE. Defaults to NULL.}
#'   \item{eta}{Numeric. The learning rate for t-SNE. Defaults to NULL.}
#'   \item{clustLinkage}{Character. Linkage method for hierarchical clustering. Defaults to "ward.D2".}
#'   \item{clustGroups}{Numeric. The number of groups for hierarchical clustering. Defaults to 9.}
#'   \item{distMethod}{Character. Distance metric for clustering. Defaults to "euclidean".}
#'   \item{minPtsAdjustmentFactor}{Numeric. Adjustment factor for the minimum points in DBSCAN clustering. Defaults to 1.}
#'   \item{epsQuantile}{Numeric. Quantile to compute the epsilon parameter for DBSCAN clustering. Defaults to 0.9.}
#'   \item{assignOutliers}{Logical. Whether to assign outliers in the clustering step. Defaults to TRUE.}
#'   \item{excludeOutliers}{Logical. Whether to exclude outliers from clustering. Defaults to TRUE.}
#'   \item{legendPosition}{Character. Position of the legend in plots (e.g., "right", "bottom"). Defaults to "right".}
#'   \item{datasetAnalysisClustLinkage}{Character. Linkage method for dataset-level analysis. Defaults to "ward.D2".}
#'   \item{datasetAnalysisType}{Character. Type of dataset analysis (e.g., "heatmap"). Defaults to "heatmap".}
#'   \item{datasetAnalysisRemoveOutliersDownstream}{Logical. Whether to remove outliers during downstream dataset analysis (e.g., machine learning). Defaults to FALSE.}
#'   \item{datasetAnalysisSortColumn}{Character. The column used to sort dataset analysis results. Defaults to "cluster".}
#'   \item{datasetAnalysisClustOrdering}{Numeric. The order of clusters for analysis. Defaults to 1.}
#'   \item{anyNAValues}{Logical. Whether the dataset contains NA values. Defaults to FALSE.}
#'   \item{categoricalVariables}{Logical. Whether the dataset contains categorical variables. Defaults to FALSE.}
#'   \item{resolution_increments}{Numeric vector. The resolution increments to be used for Louvain clustering. Defaults to \code{c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)}.}
#'   \item{min_modularities}{Numeric vector. The minimum modularities to test for clustering. Defaults to \code{c(0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9)}.}
#'   \item{target_clusters_range}{Numeric vector. The range of acceptable clusters to identify. Defaults to \code{c(3, 6)}.}
#'   \item{pickBestClusterMethod}{Character. The method to use for picking the best clustering result ("Modularity", "Silhouette", or "ML"). Defaults to "Modularity".}
#' }
#'
#' @importFrom dplyr select filter group_by summarise_all rename
#' @importFrom rlang is_null
#' @importFrom stats hclust dist na.omit
#' @importFrom mclust Mclust
#' @importFrom fpc dbscan
#'
#' @return A list containing the following:
#' \itemize{
#'   \item \code{tsne_calc}: The t-SNE results object.
#'   \item \code{tsne_clust}: The clustering results.
#'   \item \code{dataset}: A list containing the original dataset, the preprocessed dataset, and a dataset with machine learning-ready data.
#'   \item \code{clusters}: The final cluster assignments.
#'   \item \code{settings}: The list of settings used for the analysis.
#' }
#'
#' @examples
#' \donttest{
#'   data <- matrix(runif(2000), ncol=20)
#'   settings <- list(clusterType = "Louvain", 
#'   resolution_increments = c(0.05, 0.1), 
#'   min_modularities = c(0.3, 0.5))
#'   result <- immunaut(data.frame(data), settings)
#'   print(result$clusters)
#' }
#'
#' @export
immunaut <- function(dataset, settings = list()){

    # Check if dataset is empty
    if(is_var_empty(dataset) == TRUE){
        print("Dataset is empty")
        return(NULL)
    }

    # Check if settings is empty
    if(is_var_empty(settings) == TRUE){
        print("Settings is empty")
        return(NULL)
    }

    # Check if dataset is a data.frame
    if(is.data.frame(dataset) == FALSE){
        print("Dataset is not a data.frame")
        return(NULL)
    }

    # Check if settings is a list
    if(is.list(settings) == FALSE){
        print("Settings is not a data.frame")
        return(NULL)
    }

    # Check if settings is a data.frame
    if(is.data.frame(settings$fileHeader) == FALSE){
        print("settings$fileHeader is not a data.frame! Please use 'immunaut::generate_file_header' function.")
        return(NULL)
    }

    if(is_var_empty(settings$selectedColumns) == TRUE){
        settings$selectedColumns = NULL
    }

    if(is_var_empty(settings$cutOffColumnSize) == TRUE){
        settings$cutOffColumnSize = 50000
    }

    if(is_var_empty(settings$excludedColumns) == TRUE){
        settings$excludedColumns = NULL
    }

    if(is_var_empty(settings$groupingVariables) == TRUE){
        settings$groupingVariables = NULL
    }

    if(is_var_empty(settings$colorVariables) == TRUE){
        settings$colorVariables = NULL
    }

    if(is_var_empty(settings$preProcessDataset) == TRUE){
        settings$preProcessDataset = NULL
    }

    if(is_var_empty(settings$fontSize) == TRUE){
        settings$fontSize <- 12
    }

    if(is_var_empty(settings$pointSize) == TRUE){
        settings$pointSize <- 1.5
    }

    if(is_var_empty(settings$theme) == TRUE){
        settings$theme <- "theme_gray"
    }

    if(is_var_empty(settings$colorPalette) == TRUE){
        settings$colorPalette <- "RdPu"
    }

    if(is_var_empty(settings$aspect_ratio) == TRUE){
        settings$aspect_ratio <- 1
    }

    if(is_var_empty(settings$clusterType) == TRUE){
        settings$clusterType <- "Louvain"
    }

    ## Louvain Specific START
    if(is_var_empty(settings$resolution_increments) == TRUE){
        settings$resolution_increments <-  c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
        #settings$resolution_increments <- c(0.01, 0.1, 0.2, 0.3, 0.4)
    }

    if(is_var_empty(settings$min_modularities) == TRUE){
        settings$min_modularities <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9)
        #settings$min_modularities <- c(0.5, 0.6, 0.7, 0.8) 
    }

    if(is_var_empty(settings$target_clusters_range) == TRUE){
        settings$target_clusters_range <- c(3, 6)
    }

    if(is_var_empty(settings$pickBestClusterMethod) == TRUE){
        settings$pickBestClusterMethod <- "Modularity" ## Modularity, Silhouette, Overall, SIMON
    }

    if(is_var_empty(settings$selectedPartitionSplit) == TRUE){
        settings$selectedPartitionSplit <- 0.7
    }

    if(is_var_empty(settings$selectedPackages) == TRUE){
        settings$selectedPackages <- c("rf", "RRF", "RRFglobal", "gcvEarth", "cforest", "nb")
    }

    if(is_var_empty(settings$trainingTimeout) == TRUE){
        settings$trainingTimeout <- 360
    }

    ## Louvain Specific END

    if(is_var_empty(settings$removeNA) == TRUE){
        settings$removeNA = FALSE
    }

    if(is_var_empty(settings$seed) == TRUE){
        settings$seed = 1337
    }

    if(is_var_empty(settings$datasetAnalysisGrouped) == TRUE){
        settings$datasetAnalysisGrouped = FALSE
    }

    if(is_var_empty(settings$plot_size) == TRUE){
        settings$plot_size <- 12
    }

    if(is_var_empty(settings$knn_clusters) == TRUE){
        settings$knn_clusters <- 250
    }

    if(is_var_empty(settings$perplexity) == TRUE){
        settings$perplexity <- NULL
    }
    if(is_var_empty(settings$exaggeration_factor) == TRUE){
        settings$exaggeration_factor <- NULL
    }
    if(is_var_empty(settings$max_iter) == TRUE){
        settings$max_iter <- NULL
    }
    if(is_var_empty(settings$theta) == TRUE){
        settings$theta <- NULL
    }
    if(is_var_empty(settings$eta) == TRUE){
        settings$eta <- NULL
    }

    if(is_var_empty(settings$clustLinkage) == TRUE){
        settings$clustLinkage = "ward.D2"
    }

    if(is_var_empty(settings$clustGroups) == TRUE){
        settings$clustGroups = 9
    }

    ## OUTLIER DETECTION START
    if(is_var_empty(settings$distMethod) == TRUE){
        settings$distMethod = "euclidean"
    }

    if(is_var_empty(settings$minPtsAdjustmentFactor) == TRUE){
        settings$minPtsAdjustmentFactor = 1
    }

    if(is_var_empty(settings$epsQuantile) == TRUE){
        settings$epsQuantile = 0.9
    }

    if(is_var_empty(settings$assignOutliers) == TRUE){
        settings$assignOutliers = TRUE
    }
    
    if(is_var_empty(settings$excludeOutliers) == TRUE){
        settings$excludeOutliers = TRUE
    }
    
    ## OUTLIER DETECTION END

    if(is_var_empty(settings$legendPosition) == TRUE){
        settings$legendPosition = "right"
    }

    ## dataset analysis settings
    if(is_var_empty(settings$datasetAnalysisClustLinkage) == TRUE){
        settings$datasetAnalysisClustLinkage = "ward.D2"
    }

    if(is_var_empty(settings$datasetAnalysisType) == TRUE){
        settings$datasetAnalysisType = "heatmap"
    }

    if(is_var_empty(settings$datasetAnalysisRemoveOutliersDownstream) == TRUE){
        settings$datasetAnalysisRemoveOutliersDownstream = FALSE
    }


    if(is_var_empty(settings$datasetAnalysisSortColumn) == TRUE){
        settings$datasetAnalysisSortColumn = "cluster"
    }

    if(is_var_empty(settings$datasetAnalysisClustOrdering) == TRUE){
        settings$datasetAnalysisClustOrdering = 1
    }

    if(is_var_empty(settings$anyNAValues) == TRUE){
        settings$anyNAValues <- FALSE
    }
    
    if(is_var_empty(settings$categoricalVariables) == TRUE){
        settings$categoricalVariables <- FALSE
    }

    settings$fileHeader$remapped = as.character(settings$fileHeader$remapped)
    settings$fileHeader$original = as.character(settings$fileHeader$original)


    if(!is.null(settings$groupingVariables)){
        settings$groupingVariables <- settings$fileHeader %>% filter(remapped %in% settings$groupingVariables)
        settings$groupingVariables <- settings$groupingVariables$remapped
    }

    # If no columns are selected, select by cut of size
    if(is_null(settings$selectedColumns)) {
        settings$selectedColumns <- utils::tail(selectedColumns$remapped, n=settings$cutOffColumnSize)
    }

    # Remove grouping variables from selectedColumns and excludedColumns
    if(!is_null(settings$groupingVariables)) {
        if(is_null(settings$selectedColumns)) {
            settings$selectedColumns <-  setdiff(settings$selectedColumns, settings$groupingVariables)
        }
        if(is_null(settings$excludedColumns)) {
            settings$excludedColumns <-  setdiff(settings$excludedColumns, settings$groupingVariables)
        }
        if(is_null(settings$colorVariables)) {
            settings$colorVariables <-  setdiff(settings$colorVariables, settings$groupingVariables)
        }
    }

    # Remove any excluded columns from selected columns
    if(!is_null(settings$excludedColumns)) {
        ## Remove excluded from selected columns
        settings$selectedColumns <-  setdiff(settings$selectedColumns, settings$excludedColumns)
        # settings$selectedColumns <- settings$selectedColumns[settings$selectedColumns %!in% settings$excludedColumns]
    }



	# 0. Remove any undefined columns from initial dataset
	dataset_filtered <- dataset[, names(dataset) %in% c(settings$selectedColumns, settings$groupingVariables)]

	# 1. Cast all non numeric values to NA

    vars_to_cast <- c(settings$colorVariables, settings$groupingVariables)
    if (is.null(vars_to_cast)){
        vars_to_cast <- character(0)
    }else{
        message(paste("==> Casting to NA: ", vars_to_cast))
    }

    num_test <- dataset_filtered %>% select(where(is.numeric))

    for (groupVariable in settings$groupingVariables) {
        if(groupVariable %in% names(num_test)){
            dataset_filtered[[groupVariable]] <- paste("g",dataset_filtered[[groupVariable]],sep="_")
        }
    }

    dataset_filtered <- castAllStringsToNA(dataset_filtered, vars_to_cast)

	# 2. preProcessResample
    if(!is.null(settings$preProcessDataset) && length(settings$preProcessDataset) > 0){
    	preProcessMapping <- preProcessResample(dataset_filtered, 
    	    settings$preProcessDataset, 
    	    settings$groupingVariables, 
    	    settings$groupingVariables,
            settings)

       dataset_filtered <- preProcessMapping$datasetData
   }

	# 3. remove NA is any left
    if(settings$removeNA == TRUE){
        message("=====> Removing NA Values")
        dataset_filtered <- na.omit(dataset_filtered)
    }

	# 4. calculate_tsne
    set.seed(settings$seed)
    tsne_calc <- calculate_tsne(dataset_filtered, settings)
    

	# 5. cluster tsne
    message("===> Clustering using", settings$clusterType)
    set.seed(settings$seed)
    if(settings$clusterType == "Louvain"){

        tsne_clust <- list()
        iteration <- 1
        for (res_increment in settings$resolution_increments) {
            for (min_modularity in settings$min_modularities) {
                tmp <- cluster_tsne_knn_louvain(tsne_calc$info.norm, tsne_calc$tsne.norm, settings, res_increment, min_modularity)

                if(tmp$num_clusters < min(settings$target_clusters_range) && tmp$num_clusters > max(settings$target_clusters_range)){
                    message("===> INFO: Skipping cluster with ", tmp$num_clusters, " clusters")
                    next
                }
                
                tsne_clust[[iteration]] <- tmp
                iteration <- iteration + 1
            }
        }

        if(settings$pickBestClusterMethod == "Modularity"){
            tsne_clust <- pick_best_cluster_modularity(tsne_clust)
        }else if(settings$pickBestClusterMethod == "Silhouette"){
            tsne_clust <- pick_best_cluster_silhouette(tsne_clust)
        }else if(settings$pickBestClusterMethod == "Overall"){
            tsne_clust <- pick_best_cluster_overall(tsne_clust, tsne_calc)
        }else if(settings$pickBestClusterMethod == "SIMON"){
            best_cluster <- pick_best_cluster_simon(dataset, tsne_clust, tsne_calc, settings)
            tsne_clust <- best_cluster$tsne_clust
            
            message("===> SIMON: Best Score: ", best_cluster$combined_score)
            message("===> SIMON: AUROC Table: ")
            print(best_cluster$model_auroc_table)

        }else{
            tsne_clust <- pick_best_cluster_modularity(tsne_clust)
        }

    }else if(settings$clusterType == "Hierarchical"){
       tsne_clust <- cluster_tsne_hierarchical(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Mclust"){
       tsne_clust <- cluster_tsne_mclust(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Density"){
       tsne_clust <- cluster_tsne_density(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else{
        tsne_clust <- list()
        iteration <- 1
        for (res_increment in settings$resolution_increments) {
            for (min_modularity in settings$min_modularities) {
                tmp <- cluster_tsne_knn_louvain(tsne_calc$info.norm, tsne_calc$tsne.norm, settings, res_increment, min_modularity)

                if(tmp$num_clusters < min(settings$target_clusters_range) && tmp$num_clusters > max(settings$target_clusters_range)){
                    message("===> INFO: Skipping cluster with ", tmp$num_clusters, " clusters")
                    next
                }
                
                tsne_clust[[iteration]] <- tmp
                iteration <- iteration + 1
            }
        }

        if(settings$pickBestClusterMethod == "Modularity"){
            tsne_clust <- pick_best_cluster_modularity(tsne_clust)
        }else if(settings$pickBestClusterMethod == "Silhouette"){
            tsne_clust <- pick_best_cluster_silhouette(tsne_clust)
        }else if(settings$pickBestClusterMethod == "Overall"){
            tsne_clust <- pick_best_cluster_overall(tsne_clust, tsne_calc)
        }else if(settings$pickBestClusterMethod == "SIMON"){
            best_cluster <- pick_best_cluster_simon(dataset, tsne_clust, tsne_calc, settings)
            tsne_clust <- best_cluster$tsne_clust
            
            message("===> SIMON: Best Score: ", best_cluster$combined_score)
            message("===> SIMON: AUROC Table: ")
            print(best_cluster$model_auroc_table)

        }else{
            tsne_clust <- pick_best_cluster_modularity(tsne_clust)
        }
    }

    ## Get clusters from tsne_clust$info.norm[["pandora_cluster"]] and add it to original dataset in "dataset" variable
    dataset_with_clusters <- dataset
    if(nrow(dataset_with_clusters) == nrow(tsne_clust$info.norm)){
        dataset_with_clusters$pandora_cluster <- tsne_clust$info.norm$pandora_cluster
    }

    dataset_filtered_with_clusters <- dataset_filtered
    if(nrow(dataset_filtered_with_clusters) == nrow(tsne_clust$info.norm)){
        dataset_filtered_with_clusters$pandora_cluster <- tsne_clust$info.norm$pandora_cluster
    }
    
    dataset_ml <- dataset
    if(nrow(dataset_ml) == nrow(tsne_clust$info.norm)){
        dataset_ml$pandora_cluster <- tsne_clust$info.norm$pandora_cluster
        ## Should we remove any outliers, if any detected?
        dataset_ml <- remove_outliers(dataset_ml, settings)
        dataset_ml <- dplyr::rename(dataset_ml, immunaut = pandora_cluster)
        dataset_ml <- dataset_ml[, c("immunaut", setdiff(names(dataset_ml), "immunaut"))]
    }

    results <- list(
        tsne_calc = tsne_calc,
        tsne_clust = tsne_clust,
        dataset = list(
            original = dataset,
            preprocessed = dataset_filtered,
            dataset_ml = dataset_ml
        ),
        clusters = tsne_clust$info.norm$pandora_cluster,
        settings = settings
    )

    return(results)
}
