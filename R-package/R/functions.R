#' Calculate t-SNE
#'
#' This function performs a t-SNE analysis on a given dataset.
#'
#' @param dataset The dataset to perform the analysis on.
#' @param settings A list of settings for the analysis.
#' @param removeGroups If TRUE, removes the grouping variables from the dataset.
#' 
#' @importFrom Rtsne Rtsne
#' @importFrom dplyr select where mutate recode filter %>%
#' @importFrom rlang is_null
#'
#' @return A list containing the analyzed data, t-SNE results and column headers.
#' @export
calculate_tsne <- function(dataset, settings, removeGroups = TRUE){

	info.norm <- dataset

	new_colnames <- setNames(settings$fileHeader$original, settings$fileHeader$remapped)
	names(info.norm) <- recode(names(info.norm), !!!new_colnames)

    if(!is.null(settings$groupingVariables) && removeGroups == TRUE){
    	print(paste0("====> Removing grouping variables"))
    	dataset <- dataset %>% select(-any_of(settings$groupingVariables)) 
    }

    ## To be sure remove all other non numeric columns
	tsne_data <- dataset %>% select(where(is.numeric))

    ## Check perplexity
    perplexity <- settings$perplexity
    if(nrow(tsne_data) < settings$perplexity){
    	perplexity <- 1
    	print(paste0("====> Quick-fix - Adjusting perplexity to: ", perplexity))
    }
	header_mapped <- settings$fileHeader %>% filter(remapped %in% names(tsne_data))

	pca.scale <- FALSE
	## If preProcessDataset is not NULL, then we need to scale the data before applying t-SNE
	if(is.null(settings$preProcessDataset)){
		pca.scale <- TRUE
	}

	## Perform tSNE
	tsne.norm  <- Rtsne(
		as.matrix(tsne_data), 
		perplexity = perplexity, 
		pca = TRUE, ## Whether an initial PCA step should be performed
		verbose = FALSE, 
		max_iter = 2000, 
		pca_scale = pca.scale,  
		pca_center = pca.scale, 
		check_duplicates = FALSE)

	## Add tSNE results to the dataset
	info.norm <- info.norm %>% mutate(tsne1 = tsne.norm$Y[, 1], tsne2 = tsne.norm$Y[,2])

	return(list(info.norm = info.norm, tsne.norm = tsne.norm, tsne_columns = header_mapped$original))
}
#' Cluster t-SNE using KNN and Louvain method
#'
#' This function applies K-Nearest Neighbors (KNN) clustering and Louvain 
#' community detection on t-SNE results to identify clusters.
#'
#' @param info.norm The normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results.
#' @param settings A list of settings for the analysis.
#'
#' @importFrom FNN get.knn
#' @importFrom igraph graph_from_data_frame simplify cluster_louvain membership
#' @importFrom dplyr group_by select summarize_all
#'
#' @return A list containing the analyzed data with cluster assignment and cluster centroids.
#' @export
cluster_tsne_knn_louvain <- function(info.norm, tsne.norm, settings){
	set.seed(1337)
	knn_clusters <- settings$knn_clusters
    if(nrow(tsne.norm$Y) < knn_clusters){
    	knn_clusters <- round(nrow(tsne.norm$Y) / 2)
    	print(paste0("====> Quick-fix - Rtsne->tsne.norm->Y rows: ",nrow(tsne.norm$Y)," Adjusting KNN k to half of it: ", knn_clusters))
    }

    print(paste0("====>Maximum number of nearest neighbors to search: ", knn_clusters))


	knn.norm = FNN::get.knn(as.matrix(tsne.norm$Y), k = knn_clusters)
	knn.norm = data.frame(
					from = rep(1:nrow(knn.norm$nn.index), knn_clusters), 
					to = as.vector(knn.norm$nn.index), 
					weight = 1/(1 + as.vector(knn.norm$nn.dist))
				)

	nw.norm = igraph::graph_from_data_frame(knn.norm, directed = FALSE)
	nw.norm = igraph::simplify(nw.norm)
	lc.norm = igraph::cluster_louvain(nw.norm)

	info.norm$cluster = as.factor(igraph::membership(lc.norm))

	lc.cent = info.norm %>% group_by(cluster) %>% 
							select(tsne1, tsne2) %>% 
							summarize_all(mean)

	return(list(info.norm = info.norm, cluster_data = lc.cent))
}

#' Hierarchical clustering on t-SNE results
#'
#' This function applies Hierarchical Clustering on t-SNE results.

#' @param info.norm The normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results.
#' @param settings A list of settings for the analysis.
#'
#' @importFrom stats hclust dist
#' @importFrom dplyr group_by select summarize_all %>%
#'
#' @return A list containing the analyzed data with cluster assignment and cluster centroids.
#' @export
cluster_tsne_hierarchical <- function(info.norm, tsne.norm, settings){
	set.seed(1337)

	hc.norm = hclust(dist(tsne.norm$Y), method = settings$clustLinkage) 
	
	info.norm$cluster = factor(cutree(hc.norm, settings$clustGroups))

	lc.cent = info.norm %>% group_by(cluster) %>% 
							select(tsne1, tsne2) %>% 
							summarize_all(mean)


	return(list(info.norm = info.norm, cluster_data = lc.cent))
}

#' Mclust clustering on t-SNE results
#'
#' This function applies Mclust clustering on t-SNE results.
#'
#' @param info.norm The normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results.
#' @param settings A list of settings for the analysis.
#'
#' @importFrom mclust Mclust
#' @importFrom dplyr group_by select summarize_all
#'
#' @return A list containing the analyzed data with cluster assignment and cluster centroids.
#' @export
cluster_tsne_mclust <- function(info.norm, tsne.norm, settings){

    print(paste("==> cluster_tsne_mclust clustGroups: ", settings$clustGroups))

	set.seed(1337)
	mc.norm = mclust::Mclust(tsne.norm$Y, settings$clustGroups)

	info.norm$cluster = factor(mc.norm$classification)

	lc.cent = info.norm %>% group_by(cluster) %>% 
							select(tsne1, tsne2) %>% 
							summarize_all(mean)

	return(list(info.norm = info.norm, cluster_data = lc.cent))
}

#' Density-based clustering on t-SNE results
#'
#' This function applies Density-Based Spatial Clustering of Applications with Noise (DBSCAN) on t-SNE results.
#'
#' @param info.norm The normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results.
#' @param settings A list of settings for the analysis.
#'
#' @importFrom fpc dbscan
#' @importFrom dplyr group_by select summarize_all
#'
#' @return A list containing the analyzed data with cluster assignment and cluster centroids.
#' @export
cluster_tsne_density <- function(info.norm, tsne.norm, settings){
	set.seed(1337)
	ds.norm = fpc::dbscan(tsne.norm$Y, settings$reachabilityDistance)
	info.norm$cluster = factor(ds.norm$cluster)
	lc.cent = info.norm %>% group_by(cluster) %>% 
							select(tsne1, tsne2) %>% 
							summarize_all(mean)

	return(list(info.norm = info.norm, cluster_data = lc.cent))
}
