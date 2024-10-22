#' Main function to carry out immunaut analysis
#'
#' This function performs clustering and dimensionality reduction analysis on a dataset using user-defined settings.
#' It handles various pre-processing steps, dimensionality reduction via t-SNE, multiple clustering methods, and
#' generates associated plots based on user-defined or default settings.
#'
#' @param dataset A data frame representing the dataset on which the analysis will be performed. The dataset must 
#' contain numeric columns for dimensionality reduction and clustering.
#' @param settings A named list containing settings for the analysis. If NULL, defaults will be used. The settings list may contain:
#' 
#' \describe{
#'   \item{fileHeader}{A data frame mapping the original column names to remapped column names. Used for t-SNE input preparation.}
#'   \item{selectedColumns}{Character vector of columns to be used for the analysis. Defaults to NULL.}
#'   \item{cutOffColumnSize}{Numeric, the maximum size of the dataset in terms of columns. Defaults to 50,000.}
#'   \item{excludedColumns}{Character vector of columns to exclude from the analysis. Defaults to NULL.}
#'   \item{groupingVariables}{Character vector of columns to use for grouping the data during analysis. Defaults to NULL.}
#'   \item{colorVariables}{Character vector of columns to use for coloring in the plots. Defaults to NULL.}
#'   \item{preProcessDataset}{Character vector of pre-processing methods to apply (e.g., scaling, normalization). Defaults to NULL.}
#'   \item{fontSize}{Numeric, font size for plots. Defaults to 12.}
#'   \item{pointSize}{Numeric, size of points in plots. Defaults to 1.5.}
#'   \item{theme}{Character, the ggplot2 theme to use (e.g., "theme_gray"). Defaults to "theme_gray".}
#'   \item{colorPalette}{Character, color palette for plots (e.g., "RdPu"). Defaults to "RdPu".}
#'   \item{aspect_ratio}{Numeric, the aspect ratio of plots. Defaults to 1.}
#'   \item{clusterType}{Character, the clustering method to use. Options are "Louvain", "Hierarchical", "Mclust", "Density". Defaults to "Louvain".}
#'   \item{removeNA}{Logical, whether to remove rows with NA values. Defaults to FALSE.}
#'   \item{datasetAnalysisGrouped}{Logical, whether to perform grouped dataset analysis. Defaults to FALSE.}
#'   \item{plot_size}{Numeric, the size of the plot. Defaults to 12.}
#'   \item{knn_clusters}{Numeric, the number of clusters for KNN-based clustering. Defaults to 250.}
#'   \item{perplexity}{Numeric, the perplexity parameter for t-SNE. Defaults to NULL (automatically determined).}
#'   \item{exaggeration_factor}{Numeric, the exaggeration factor for t-SNE. Defaults to NULL.}
#'   \item{max_iter}{Numeric, the maximum number of iterations for t-SNE. Defaults to NULL.}
#'   \item{theta}{Numeric, the Barnes-Hut approximation parameter for t-SNE. Defaults to NULL.}
#'   \item{eta}{Numeric, the learning rate for t-SNE. Defaults to NULL.}
#'   \item{clustLinkage}{Character, linkage method for hierarchical clustering. Defaults to "ward.D2".}
#'   \item{clustGroups}{Numeric, the number of groups for hierarchical clustering. Defaults to 9.}
#'   \item{distMethod}{Character, distance metric for clustering. Defaults to "euclidean".}
#'   \item{minPtsAdjustmentFactor}{Numeric, adjustment factor for the minimum points in DBSCAN clustering. Defaults to 1.}
#'   \item{epsQuantile}{Numeric, quantile to compute the epsilon parameter for DBSCAN clustering. Defaults to 0.9.}
#'   \item{assignOutliers}{Logical, whether to assign outliers in the clustering step. Defaults to TRUE.}
#'   \item{excludeOutliers}{Logical, whether to exclude outliers from clustering. Defaults to TRUE.}
#'   \item{legendPosition}{Character, position of the legend in plots (e.g., "right", "bottom"). Defaults to "right".}
#'   \item{datasetAnalysisClustLinkage}{Character, linkage method for dataset-level analysis. Defaults to "ward.D2".}
#'   \item{datasetAnalysisType}{Character, type of dataset analysis (e.g., "heatmap"). Defaults to "heatmap".}
#'   \item{datasetAnalysisRemoveOutliersDownstream}{Logical, whether to remove outliers during downstream dataset analysis. Defaults to TRUE.}
#'   \item{datasetAnalysisSortColumn}{Character, the column used to sort dataset analysis results. Defaults to "cluster".}
#'   \item{datasetAnalysisClustOrdering}{Numeric, the order of clusters for analysis. Defaults to 1.}
#'   \item{anyNAValues}{Logical, whether the dataset contains NA values. Defaults to FALSE.}
#'   \item{categoricalVariables}{Logical, whether the dataset contains categorical variables. Defaults to FALSE.}
#' }
#'
#' @importFrom dplyr select filter group_by summarize_all
#' @importFrom rlang is_null
#' @importFrom stats hclust dist na.omit
#' @importFrom mclust Mclust
#' @importFrom fpc dbscan
#'
#' @return A data frame with cluster assignments added as a new column. If NULL, warning messages will be printed for any erroneous input parameters.
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

    if(is_var_empty(settings$removeNA) == TRUE){
        settings$removeNA = FALSE
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
        settings$datasetAnalysisRemoveOutliersDownstream = TRUE
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
        print(paste("==> Casting to NA: ", vars_to_cast))
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
    	    settings$groupingVariables)

       dataset_filtered <- preProcessMapping$datasetData
   }

	# 3. remove NA is any left
    if(settings$removeNA == TRUE){
        print(paste0("=====> Removing NA Values"))
        dataset_filtered <- na.omit(dataset_filtered)
    }

	# 4. calculate_tsne
    set.seed(1337)
    tsne_calc <- calculate_tsne(dataset_filtered, settings)
    

	# 5. cluster tsne
    print(paste0("===> Clustering using", settings$clusterType))
    set.seed(1337)
    if(settings$clusterType == "Louvain"){
       tsne_clust <- cluster_tsne_knn_louvain(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Hierarchical"){
       tsne_clust <- cluster_tsne_hierarchical(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Mclust"){
       tsne_clust <- cluster_tsne_mclust(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Density"){
       tsne_clust <- cluster_tsne_density(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else{
       tsne_clust <- cluster_tsne_knn_louvain(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
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
    
    dataset_with_clusters <- remove_outliers(dataset_with_clusters, settings)
    data_for_heatmap <- remove_outliers(tsne_clust$info.norm, settings)

    results <- list(
        tsne_calc = tsne_calc,
        tsne_clust = tsne_clust,
        settings = settings,
        
        dataset = dataset,
        dataset_with_clusters = dataset_with_clusters,
        dataset_filtered = dataset_filtered,
        dataset_filtered_with_clusters = dataset_filtered_with_clusters,

        data_for_heatmap = data_for_heatmap,
        num_samples = nrow(dataset_with_clusters)
    )

    return(results)
}
