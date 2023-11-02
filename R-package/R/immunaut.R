#' Main function to carry out immunaut analysis
#'
#' This function performs different types of clustering on given dataset with user-defined 
#' settings and generates associated plots. It performs error checking, pre-processing, 
#' dimension reduction via t-SNE, clustering, and data visualization in sequence.
#'
#' @param dataset A data frame representing the dataset on which the analysis will be performed.
#' @param settings A named list containing the settings for the analysis. If left NULL, defaults will be used.
#'
#' @importFrom dplyr select filter group_by summarize_all
#' @importFrom rlang is_null
#' @importFrom stats hclust dist
#' @importFrom mclust Mclust
#' @importFrom fpc dbscan
#'
#' @return The final dataset having an additional column for cluster assignments. If NULL, warning messages would 
#'         be printed for any erroneous input parameters.
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

	# Define settings input defaults
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

    if(is_var_empty(settings$plot_size) == TRUE){
        settings$plot_size <- 12
    }

    if(is_var_empty(settings$knn_clusters) == TRUE){
        settings$knn_clusters <- 250
    }

    if(is_var_empty(settings$perplexity) == TRUE){
        settings$perplexity <- 30
    }
    if(is_var_empty(settings$clustLinkage) == TRUE){
        settings$clustLinkage = "ward.D2"
    }
    if(is_var_empty(settings$clustGroups) == TRUE){
        settings$clustGroups = 9
    }
    
    if(is_var_empty(settings$reachabilityDistance) == TRUE){
        settings$reachabilityDistance = 2
    }

    if(is_var_empty(settings$legendPosition) == TRUE){
        settings$legendPosition = "right"
    }

    ## Analysis settings
    if(is_var_empty(settings$datasetAnalysisClustLinkage) == TRUE){
        settings$datasetAnalysisClustLinkage = "ward.D2"
    }

    if(is_var_empty(settings$datasetAnalysisType) == TRUE){
        settings$datasetAnalysisType = "heatmap"
    }

    if(is_var_empty(settings$datasetAnalysisSortColumn) == TRUE){
        settings$datasetAnalysisSortColumn = "cluster"
    }

    if(is_var_empty(settings$datasetAnalysisClustOrdering) == TRUE){
        settings$datasetAnalysisClustOrdering = 1
    }


	# 0. Remove any undefined columns from initial dataset
	dataset_filtered <- dataset[, names(dataset) %in% c(settings$selectedColumns, settings$groupingVariables)]

	# 1. castAllStringsToNA
    dataset_filtered <- castAllStringsToNA(dataset_filtered, c(settings$colorVariables, settings$groupingVariables))

	# 2. preProcessResample
	preProcessMapping <- preProcessResample(dataset_filtered, 
	    settings$preProcessDataset, 
	    settings$groupingVariables, 
	    settings$groupingVariables)

	dataset_filtered <- preProcessMapping$datasetData

	# 3. remove NA is any left
    if(settings$removeNA == TRUE){
        print(paste0("=====> Removing NA Values"))
        dataset_filtered <- na.omit(dataset_filtered)
    }

	# 4. calculate_tsne
    tsne_calc <- calculate_tsne(dataset_filtered, settings)
    

	# 5. cluster tsne
    print(paste0("===> Clustering using", settings$clusterType))
    set.seed(1337)
    if(settings$clusterType == "Louvain"){
       clust_plot_tsne <- cluster_tsne_knn_louvain(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Hierarchical"){
       clust_plot_tsne <- cluster_tsne_hierarchical(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Mclust"){
       clust_plot_tsne <- cluster_tsne_mclust(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else if(settings$clusterType == "Density"){
       clust_plot_tsne <- cluster_tsne_density(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }else{
       clust_plot_tsne <- cluster_tsne_knn_louvain(tsne_calc$info.norm, tsne_calc$tsne.norm, settings)
    }

	# 6. add clusters to dataset

    ## Get clusters from clust_plot_tsne$info.norm[["cluster"]] and add it to original dataset in "dataset" variable
    dastaset_with_clusters <- dataset
    if(nrow(dastaset_with_clusters) == nrow(clust_plot_tsne$info.norm)){
        dastaset_with_clusters$cluster <- clust_plot_tsne$info.norm$cluster
    }


    return(dastaset_with_clusters)
}
