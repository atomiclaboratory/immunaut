#' Perform t-Distributed Stochastic Neighbor Embedding (t-SNE)
#'
#' The `calculate_tsne` function reduces high-dimensional data into a 2-dimensional space using 
#' t-SNE for visualization and analysis. This function dynamically adjusts t-SNE parameters 
#' based on the characteristics of the dataset, ensuring robust handling of edge cases.
#' It also performs data validation, such as checking for sufficient data, removing zero variance 
#' columns, and adjusting perplexity for optimal performance.
#'
#' @param dataset A data frame or matrix containing the dataset to be processed. Must contain numeric columns.
#' @param settings A list of settings for t-SNE, which may include `fileHeader`, `groupingVariables`, `perplexity`, 
#' `max_iter`, `eta`, `theta`, `exaggeration_factor`, and `preProcessDataset`.
#' @param removeGroups Logical, indicating whether to remove grouping variables before performing t-SNE. Default is TRUE.
#'
#' @return A list containing:
#' - `info.norm`: The dataset with the t-SNE coordinates (`tsne1`, `tsne2`) added.
#' - `tsne.norm`: The output from the `Rtsne` function.
#' - `tsne_columns`: The names of the t-SNE columns used.
#' - `initial_dims`: The number of dimensions used in the initial PCA step.
#' - `perplexity`: The perplexity parameter used.
#' - `exaggeration_factor`: The exaggeration factor used.
#' - `max_iter`: The number of iterations used.
#' - `theta`: The Barnes-Hut approximation parameter used.
#' - `eta`: The learning rate used.
#'
#' @importFrom dplyr select where mutate %>% any_of
#' @importFrom Rtsne Rtsne
#' @importFrom stats prcomp
#' @importFrom plyr mapvalues
#' 
#' @examples
#' \dontrun{
#' dataset <- data.frame(matrix(runif(1000), nrow = 100))
#' settings <- list(
#'   fileHeader = data.frame(original = colnames(dataset), remapped = colnames(dataset)),
#'   perplexity = 30,
#'   max_iter = 1000,
#'   eta = 200,
#'   theta = 0.5
#' )
#' result <- calculate_tsne(dataset, settings)
#' print(result$info.norm)
#' }
#'
#' @export
calculate_tsne <- function(dataset, settings, removeGroups = TRUE){
    set.seed(1337)

    # Start logging
    message("===> Starting t-SNE calculation")

    info.norm <- dataset

	# Remap column names if necessary
	if (!is.null(settings$fileHeader)) {
	    existing_columns <- intersect(names(info.norm), settings$fileHeader$remapped)  # Columns present in both
	    if (length(existing_columns) > 0) {
	        map_from <- settings$fileHeader$remapped[settings$fileHeader$remapped %in% existing_columns]
	        map_to <- settings$fileHeader$original[settings$fileHeader$remapped %in% existing_columns]
	        
	        names(info.norm) <- plyr::mapvalues(names(info.norm), from = map_from, to = map_to)
	        message("===> Remapped column names based on settings")
	    } else {
	        message("===> No matching columns to remap")
	    }
	}


    # Optionally remove grouping variables
    if (!is.null(settings$groupingVariables) && removeGroups == TRUE) {
        message(paste0("====> Removing grouping variables: ", toString(settings$groupingVariables)))
        dataset <- dataset %>% select(-any_of(settings$groupingVariables)) 
    }

    # Keep only numeric columns
    message(paste0("===> INFO: Keep only numeric columns"))
    tsne_data <- dataset %>% select(where(is.numeric))

    # Ensure there are numeric columns to process
    if (ncol(tsne_data) < 1) {
        stop("Not enough numeric columns to perform t-SNE.")
    }

    # Remove zero variance columns
    message(paste0("===> INFO: Remove zero variance columns"))
    tsne_data <- tsne_data %>% select(where(~ var(., na.rm = TRUE) != 0))
    if (ncol(tsne_data) < 1) {
        stop("Not enough variable numeric columns to perform t-SNE.")
    }

    # Check for missing values
    if (any(is.na(tsne_data))) {
        stop("Input data for t-SNE contains missing values.")
    }

    num_samples <- nrow(tsne_data)
    num_features <- ncol(tsne_data)

    # Ensure sufficient samples for t-SNE
    if (num_samples < 4) {
        stop("Not enough data to perform t-SNE (minimum 4 samples required).")
    }

    message(paste0("===> INFO: Perform PCA to calculate initial_dims"))
    # Perform PCA to calculate initial_dims
    pca_result <- prcomp(tsne_data, scale. = TRUE)
    explained_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
    initial_dims <- ifelse(any(explained_variance <= 0.9), max(which(explained_variance <= 0.9)), 1)
    initial_dims <- min(max(initial_dims, 1), ncol(tsne_data), 100)

    message(paste0("===> INFO: Using initial_dims: ", initial_dims))

    # Adjust perplexity dynamically if not provided
    if (!is.null(settings$perplexity) && settings$perplexity > 0) {
        perplexity <- settings$perplexity
        message(paste0("===> INFO: Using provided perplexity: ", perplexity))
    } else {
        max_perplexity <- floor((num_samples - 1) / 3)
        if (max_perplexity < 1) stop("Not enough data to compute perplexity.")
        perplexity <- min(30, max_perplexity)
        message(paste0("===> INFO: Using dynamic perplexity: ", perplexity))
    }

    header_mapped <- settings$fileHeader %>% filter(remapped %in% names(tsne_data))

    pca.scale <- TRUE
    if(!is.null(settings$preProcessDataset) && length(settings$preProcessDataset) > 0){
        pca.scale <- FALSE
    }


    # Set t-SNE parameters
    # Check if settings are provided and not zero
    if (!is.null(settings$max_iter) && settings$max_iter != 0 ||
        !is.null(settings$eta) && settings$eta != 0) {
        # Use the provided settings
        max_iter <- settings$max_iter
        eta <- settings$eta

        theta <- settings$theta
    } else {
        # Adjust max_iter and other parameters based on dataset size
        if (num_samples < 500) {
            max_iter <- 10000  # Increased iterations for small datasets
            theta <- 0         # Use exact t-SNE
            eta <- 500         # Higher learning rate
        } else {
            # Adjust max_iter based on dataset complexity
            base_iter <- 3000
            complexity_factor <- sqrt(num_samples * num_features) / 500
            max_iter <- base_iter + (500 * complexity_factor)
            max_iter <- min(max_iter, 10000)
            max_iter <- round(max_iter, 0)
            
            eta <- 150
            # Dynamically adjust theta and eta based on dataset size
            if (num_samples < 5000) {
                theta <- 0.2
                eta <- 250
            } else {
                theta <- 0.5
                eta <- 250
            }
        }
    }


    # Set exaggeration_factor
    if (!is.null(settings$exaggeration_factor) && settings$exaggeration_factor != 0) {
        exaggeration_factor <- settings$exaggeration_factor
    } else {
        # Adjust exaggeration_factor based on dataset size
        if (num_samples < 500) {
            exaggeration_factor <- 4
        } else if (num_samples < 2000) {
            exaggeration_factor <- 8
        } else {
            exaggeration_factor <- 12
        }
    }

    message(paste0("===> INFO: Using max_iter: ", max_iter))
    message(paste0("===> INFO: Using theta: ", theta))
    message(paste0("===> INFO: Using eta: ", eta))
    message(paste0("===> INFO: Using exaggeration_factor: ", exaggeration_factor))

    tsne.norm <- Rtsne::Rtsne(
        as.matrix(tsne_data),
        dims = 2,
        perplexity = perplexity,
        pca = TRUE,
        pca_center = pca.scale,
        pca_scale = pca.scale,
        check_duplicates = FALSE,
        initial_dims = initial_dims,
        max_iter = max_iter,
        theta = theta,
        eta = eta,
        exaggeration_factor = exaggeration_factor,
        verbose = FALSE,
        num_threads = 1
    )

    info.norm <- info.norm %>% mutate(tsne1 = tsne.norm$Y[, 1], tsne2 = tsne.norm$Y[,2])

    return(list(
        info.norm = info.norm,
        tsne.norm = tsne.norm, 
        tsne_columns = header_mapped$original, 
        initial_dims = initial_dims, 
        perplexity = perplexity, 
        exaggeration_factor = exaggeration_factor,
        max_iter = max_iter, 
        theta = theta, 
        eta = eta
    ))
}

#' Perform KNN and Louvain Clustering on t-SNE Results
#'
#' This function performs clustering on t-SNE results by first applying K-Nearest Neighbors (KNN) to construct a graph, 
#' and then using the Louvain method for community detection. The function dynamically adjusts KNN parameters based on the 
#' size of the dataset, ensuring scalability. Additionally, it computes the silhouette score to evaluate cluster quality 
#' and calculates cluster centroids for visualization.
#'
#' @param info.norm A data frame containing the normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results object, which includes the 2D t-SNE coordinates in the `Y` matrix.
#' @param settings A list of settings for the analysis, including:
#' - `knn_clusters`: The number of nearest neighbors to use for KNN (default: 250).
#' - `start_resolution`: The starting resolution for Louvain clustering.
#' - `end_resolution`: The maximum resolution to test.
#' - `min_modularity`: The minimum acceptable modularity for valid clusterings.
#'
#' @importFrom FNN get.knn
#' @importFrom igraph graph_from_data_frame simplify cluster_louvain membership modularity
#' @importFrom dplyr group_by select summarise across left_join n
#' @importFrom cluster silhouette
#' @importFrom stats dist median
#'
#' @return A list containing:
#' - `info.norm`: The input data frame with an additional `pandora_cluster` column for cluster assignments.
#' - `cluster_data`: A data frame containing cluster centroids and labeled clusters.
#' - `avg_silhouette_score`: The average silhouette score, a measure of clustering quality.
#' - `modularity`: The modularity score of the Louvain clustering.
#'
#' @details
#' This function constructs a KNN graph using the t-SNE results, then applies the Louvain algorithm for community detection. 
#' It adjusts the KNN parameter dynamically based on the dataset size, ensuring a robust clustering process. Silhouette scores 
#' are computed to assess the quality of the clustering, while cluster centroids and sizes are calculated for easy visualization.
#' NA cluster assignments are handled by assigning them to a separate cluster labeled as "100."
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' tsne_results <- Rtsne::Rtsne(matrix(runif(200), ncol = 2))  # Generate random t-SNE data
#' settings <- list(knn_clusters = 10, start_resolution = 0.1,
#' 					end_resolution = 2, min_modularity = 0.3)
#' result <- cluster_tsne_knn_louvain(info.norm = data.frame(matrix(runif(200), ncol = 2)), 
#'                                    tsne.norm = tsne_results, settings = settings)
#' print(result$cluster_data)
#' }
#'
#' @export
cluster_tsne_knn_louvain <- function(info.norm, tsne.norm, settings){
	set.seed(1337)

    # Log basic dimensions of the input data
    message(paste0("===> INFO: info.norm dimensions: ", paste(dim(info.norm), collapse = " x ")))
    message(paste0("===> INFO: tsne.norm$Y dimensions: ", paste(dim(tsne.norm$Y), collapse = " x ")))

    # Adjust KNN clusters if needed
    knn_clusters <- settings$knn_clusters
    if (nrow(tsne.norm$Y) < knn_clusters) {
        knn_clusters <- round(nrow(tsne.norm$Y) / 2)
        message(paste0("===> INFO: Adjusted KNN clusters to half the number of samples: ", knn_clusters))
    }
    message(paste0("===> INFO: Maximum KNN neighbors set to: ", knn_clusters))


	knn.norm = FNN::get.knn(as.matrix(tsne.norm$Y), k = knn_clusters)
	knn.norm = data.frame(
					from = rep(1:nrow(knn.norm$nn.index), knn_clusters), 
					to = as.vector(knn.norm$nn.index), 
					weight = 1/(1 + as.vector(knn.norm$nn.dist))
				)

    # Build graph from KNN results and simplify it
	nw.norm = igraph::graph_from_data_frame(knn.norm, directed = FALSE)
	nw.norm = igraph::simplify(nw.norm)


    # Find optimal resolution for Louvain clustering
    resolution <- find_optimal_resolution(nw.norm, start_resolution = 0.1, end_resolution = 10,  min_modularity = 0.1)
    lc.norm <- igraph::cluster_louvain(nw.norm, resolution = resolution$optimal_resolution)

    # Log cluster results
    num_clusters <- length(unique(igraph::membership(lc.norm)))
    message(paste0("===> INFO: Number of clusters found: ", num_clusters))
    modularity <- igraph::modularity(lc.norm)
    message(paste0("===> INFO: Modularity score: ", modularity))

    # Assign clusters to the info.norm data frame
    info.norm$pandora_cluster <- as.factor(igraph::membership(lc.norm))

    # Debugging: Check for NA values in pandora_cluster
    num_na_clusters <- sum(is.na(info.norm$pandora_cluster))
    if(num_na_clusters > 0) {
        warning(paste0("===> INFO: Number of NA clusters in pandora_cluster: ", num_na_clusters))    
    }

    # Handle NA clusters by assigning them to cluster "100"
    na_indices <- is.na(info.norm$pandora_cluster)
    num_na <- sum(na_indices)
    if(num_na > 0){
        # Add 100 to the levels of pandora_cluster
        info.norm$pandora_cluster <- factor(info.norm$pandora_cluster, levels = c(levels(info.norm$pandora_cluster), "100"))
        # Now assign 100 to NA indices
        info.norm$pandora_cluster[na_indices] <- "100"
        message(paste0("===> INFO: Replaced ", num_na, " NA cluster assignments with '100'"))
    }

    # Calculate the distance matrix for silhouette score computation
    distance_matrix <- dist(tsne.norm$Y)    
    message(paste0("===> INFO: Distance matrix calculated with size: ", attr(distance_matrix, "Size")))


    # Calculate silhouette scores to evaluate cluster quality
    silhouette_scores <- cluster::silhouette(as.integer(info.norm$pandora_cluster), distance_matrix)
    if (is.matrix(silhouette_scores)) {
        avg_silhouette_score <- mean(silhouette_scores[, "sil_width"], na.rm = TRUE)
        message(paste0("===> INFO: Average silhouette score: ", avg_silhouette_score))
    } else {
        message("===> WARNING: Silhouette score calculation did not return a matrix.")
        avg_silhouette_score <- NA
    }

    # Compute cluster centers
	lc.cent <- info.norm %>%
	    group_by(pandora_cluster) %>%
	    summarise(across(c(tsne1, tsne2), ~ median(.x, na.rm = TRUE)), .groups = 'drop')


    # Log number of cluster centers
    message(paste0("===> INFO: Cluster centers computed for ", nrow(lc.cent), " clusters"))

    # Compute cluster sizes
    cluster_sizes <- info.norm %>%
      group_by(pandora_cluster) %>%
      summarise(num_samples = n(), .groups = 'drop') # Calculate the number of samples in each cluster

    # Add cluster size labels
    lc.cent <- lc.cent %>%
      left_join(cluster_sizes, by = "pandora_cluster")

    # Create the 'label' column that combines cluster ID and number of samples
    lc.cent <- lc.cent %>%
      mutate(label = paste(pandora_cluster, "-", num_samples))

    # Drop the 'num_samples' column if you no longer need it
    lc.cent <- select(lc.cent, -num_samples)

    return(list(info.norm = info.norm, cluster_data = lc.cent, avg_silhouette_score = avg_silhouette_score, modularity = modularity))
}

#' Perform Hierarchical Clustering on t-SNE Results
#'
#' This function applies hierarchical clustering to t-SNE results, allowing for the identification of clusters in 
#' a reduced-dimensional space. The function also handles outliers by using DBSCAN for initial noise detection, 
#' and provides options to include or exclude outliers from the clustering process. Silhouette scores are computed 
#' to evaluate clustering quality, and cluster centroids are returned for visualization.
#'
#' @param info.norm A data frame containing the normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results object, including the 2D t-SNE coordinates (`Y` matrix).
#' @param settings A list of settings for the clustering analysis. The settings must include:
#' - `clustLinkage`: The linkage method for hierarchical clustering (e.g., "ward.D2").
#' - `clustGroups`: The number of groups (clusters) to cut the hierarchical tree into.
#' - `distMethod`: The distance metric to be used (e.g., "euclidean").
#' - `minPtsAdjustmentFactor`: A factor to adjust the minimum number of points required to form a cluster (MinPts).
#' - `epsQuantile`: The quantile used to determine the `eps` value for DBSCAN.
#' - `excludeOutliers`: A logical value indicating whether to exclude outliers detected by DBSCAN from hierarchical clustering.
#' - `pointSize`: A numeric value used to adjust the placement of outlier centroids.
#'
#' @importFrom stats hclust dist cutree median
#' @importFrom dplyr group_by summarise left_join mutate select n
#' @importFrom cluster silhouette
#' @importFrom dbscan kNNdist
#'
#' @return A list containing:
#' - `info.norm`: The input data frame with an additional `pandora_cluster` column for cluster assignments.
#' - `cluster_data`: A data frame with cluster centroids and labeled clusters.
#' - `avg_silhouette_score`: The average silhouette score, providing a measure of clustering quality.
#'
#' @details
#' The function first uses DBSCAN to detect outliers (marked as cluster "100") and then applies hierarchical clustering 
#' on the t-SNE results, either including or excluding the outliers depending on the settings. Silhouette scores are 
#' computed to assess the quality of the clustering. Cluster centroids are calculated and returned, along with the 
#' sizes of each cluster. Outliers, if detected, are handled separately in the final centroid calculation.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' tsne_results <- Rtsne::Rtsne(matrix(runif(200), ncol = 2))  # Generate random t-SNE data
#' settings <- list(clustLinkage = "ward.D2", clustGroups = 5, 
#' 					distMethod = "euclidean", 
#' 					minPtsAdjustmentFactor = 1.5, epsQuantile = 0.9, 
#' 					excludeOutliers = TRUE, pointSize = 1.5)
#' result <- cluster_tsne_hierarchical(info.norm = data.frame(matrix(runif(200), ncol = 2)), 
#'                                     tsne.norm = tsne_results, settings = settings)
#' print(result$cluster_data)
#' }
#'
#' @export
cluster_tsne_hierarchical <- function(info.norm, tsne.norm, settings) {
    set.seed(1337)
    # Validate settings
    if (!"clustLinkage" %in% names(settings) || !"clustGroups" %in% names(settings)) {
        stop("Settings must include 'clustLinkage' and 'clustGroups'.")
    }

    avg_silhouette_score <- 0

    # Prepare data for DBSCAN
    tsne_data <- tsne.norm$Y

    # Calculate minPts and eps dynamically based on settings
    minPts_baseline <- dim(tsne_data)[2] * 2
    minPts <- max(2, settings$minPtsAdjustmentFactor * minPts_baseline)
    k_dist <- dbscan::kNNdist(tsne_data, k = minPts - 1)
    eps_quantile <- settings$epsQuantile
    eps <- stats::quantile(k_dist, eps_quantile)
    dbscan_result <- dbscan::dbscan(tsne_data, eps = eps, minPts = minPts)

    # Mark outliers as cluster "100"
    dbscan_result$cluster[dbscan_result$cluster == 0] <- 100
    # Update info.norm with DBSCAN results (cluster assignments, including marked outliers)
    info.norm$pandora_cluster <- as.factor(dbscan_result$cluster)
    non_noise_indices <- which(dbscan_result$cluster != 100) # Outliers are now marked as "100"
    noise_indices <- which(dbscan_result$cluster == 100)

    # Include or exclude outliers in the hierarchical clustering based on settings
    data_for_clustering <- if (settings$excludeOutliers) tsne_data[non_noise_indices, ] else tsne_data
    indices_for_clustering <- if (settings$excludeOutliers) non_noise_indices else seq_len(nrow(tsne_data))

    if (settings$excludeOutliers) {
        message("Excluding outliers from hierarchical clustering.")
    } else {
        message("Including outliers in hierarchical clustering.")
    }

    if (length(indices_for_clustering) >= 2) {
        dist_matrix <- dist(data_for_clustering, method = settings$distMethod)
        hc.norm <- hclust(dist_matrix, method = settings$clustLinkage)
        h_clusters <- cutree(hc.norm, settings$clustGroups)

        # Ensure the cluster labels are part of the factor levels before assignment
        h_clusters_factor <- as.factor(h_clusters)
        levels(info.norm$pandora_cluster) <- union(levels(info.norm$pandora_cluster), levels(h_clusters_factor))

        # Assign hierarchical clustering results back to info.norm$pandora_cluster
        if(length(indices_for_clustering) < nrow(tsne_data)){
            info.norm$pandora_cluster[indices_for_clustering] <- h_clusters_factor
        } else {
            info.norm$pandora_cluster <- h_clusters_factor
        }

        # Replace NA values with 100 specifically
        na_indices <- is.na(info.norm$pandora_cluster)
        info.norm$pandora_cluster[na_indices] <- "100"

        # Calculate distances based on the exact data used for clustering
        distance_matrix <- dist(data_for_clustering)
        # Ensure cluster labels are integers and align with the distance matrix
        cluster_labels <- as.integer(factor(info.norm$pandora_cluster[indices_for_clustering]))
        # Calculate silhouette scores using the aligned data
        silhouette_scores <- cluster::silhouette(cluster_labels, distance_matrix)

        if(is.matrix(silhouette_scores)) {
            # Extract the silhouette widths from the scores
            silhouette_widths <- silhouette_scores[, "sil_width"]
            avg_silhouette_score <- mean(silhouette_widths, na.rm = TRUE)
        }

        if(length(noise_indices) > 0){
            print(paste("====> Noise indices: ", length(noise_indices)))
            if(!"100" %in% levels(info.norm$pandora_cluster)) {
                info.norm$pandora_cluster <- factor(info.norm$pandora_cluster, levels = c(levels(info.norm$pandora_cluster), "100"))
                info.norm$pandora_cluster[noise_indices] <- "100"
            }
        }

        print(paste("====> Noise indices done"))

    } else {
        warning("Not enough data points for hierarchical clustering.")
    }

    # Ensure all cluster assignments, including outliers marked as "100", are recognized as valid levels
    info.norm$pandora_cluster <- factor(info.norm$pandora_cluster, levels = unique(as.character(info.norm$pandora_cluster)))

    # Compute cluster centers based on final clustering results
    # Compute median values for all clusters
    lc.cent <- info.norm %>%
        group_by(pandora_cluster) %>%
        summarise(
            tsne1 = median(tsne1, na.rm = TRUE),
            tsne2 = median(tsne2, na.rm = TRUE),
            .groups = 'drop'
        )

    # Adjust "100" cluster separately (outliers)
    lc.cent <- lc.cent %>%
        mutate(
            tsne1 = ifelse(pandora_cluster == "100", tsne1 + settings$pointSize / 2, tsne1),
            tsne2 = ifelse(pandora_cluster == "100", tsne2 + settings$pointSize / 2, tsne2)
        )

    # Compute cluster sizes (number of samples per cluster)
    cluster_sizes <- info.norm %>%
        group_by(pandora_cluster) %>%
        summarise(num_samples = n(), .groups = 'drop') # Calculate the number of samples in each cluster

    # Join the cluster sizes back to the lc.cent dataframe to include the number of samples per cluster
    lc.cent <- lc.cent %>%
        left_join(cluster_sizes, by = "pandora_cluster")

    # Create the 'label' column that combines cluster ID and number of samples
    lc.cent <- lc.cent %>%
        mutate(label = paste(pandora_cluster, "-", num_samples))

    # Drop the 'num_samples' column if you no longer need it
    lc.cent <- select(lc.cent, -num_samples)

    return(list(info.norm = info.norm, cluster_data = lc.cent, avg_silhouette_score = avg_silhouette_score))
}



#' Apply Mclust Clustering on t-SNE Results
#'
#' This function performs Mclust clustering on the 2D t-SNE results, which are derived from high-dimensional data. 
#' It includes an initial outlier detection step using DBSCAN, and the user can specify whether to exclude outliers 
#' from the clustering process. Silhouette scores are computed to evaluate the quality of the clustering, and cluster 
#' centroids are returned for visualization, with outliers handled separately.
#'
#' @param info.norm A data frame containing the normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results object, including the 2D t-SNE coordinates (`Y` matrix).
#' @param settings A list of settings for the clustering analysis, including:
#' - `clustGroups`: The number of groups (clusters) for Mclust to fit.
#' - `minPtsAdjustmentFactor`: A factor to adjust the minimum number of points required to form a cluster (MinPts) in DBSCAN.
#' - `epsQuantile`: The quantile used to determine the `eps` value for DBSCAN.
#' - `excludeOutliers`: A logical value indicating whether to exclude outliers detected by DBSCAN from the Mclust clustering.
#' - `pointSize`: A numeric value used to adjust the placement of outlier centroids.
#'
#' @importFrom mclust Mclust mclustBIC
#' @importFrom dplyr group_by summarise left_join mutate n
#' @importFrom stats dist median
#' @importFrom dbscan kNNdist
#' @importFrom cluster silhouette
#'
#' @return A list containing:
#' - `info.norm`: The input data frame with an additional `pandora_cluster` column for cluster assignments.
#' - `cluster_data`: A data frame with cluster centroids and labeled clusters.
#' - `avg_silhouette_score`: The average silhouette score, providing a measure of clustering quality.
#'
#' @details
#' The function first uses DBSCAN to detect outliers (marked as cluster "100") and then applies Mclust clustering on the t-SNE 
#' results. Outliers can be either included or excluded from the clustering, depending on the settings. Silhouette scores are 
#' calculated to assess the quality of the clustering. Cluster centroids are returned, along with the sizes of each cluster, 
#' and outliers are handled separately in the centroid calculation.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' tsne_results <- Rtsne::Rtsne(matrix(runif(200), ncol = 2))
#' settings <- list(clustGroups = 3, minPtsAdjustmentFactor = 1.5, 
#' 					epsQuantile = 0.9, excludeOutliers = TRUE, 
#' 					pointSize = 1.5)
#' result <- cluster_tsne_mclust(info.norm = data.frame(matrix(runif(200), ncol = 2)), 
#'                               tsne.norm = tsne_results, settings = settings)
#' print(result$cluster_data)
#' }
#'
#' @export
cluster_tsne_mclust <- function(info.norm, tsne.norm, settings) {
    set.seed(1337)
    print(paste("==> cluster_tsne_mclust clustGroups: ", settings$clustGroups))

    avg_silhouette_score <- 0

    # Prepare data for DBSCAN
    tsne_data <- tsne.norm$Y

    # Calculate minPts and eps dynamically based on settings
    minPts_baseline <- dim(tsne_data)[2] * 2
    minPts <- max(2, settings$minPtsAdjustmentFactor * minPts_baseline)
    k_dist <- dbscan::kNNdist(tsne_data, k = minPts - 1)
    eps_quantile <- settings$epsQuantile
    eps <- stats::quantile(k_dist, eps_quantile)
    
    dbscan_result <- dbscan::dbscan(tsne_data, eps = eps, minPts = minPts)

    # Mark outliers as cluster "100"
    dbscan_result$cluster[dbscan_result$cluster == 0] <- 100

    # Update info.norm with DBSCAN results (cluster assignments, including marked outliers)
    info.norm$pandora_cluster <- as.factor(dbscan_result$cluster)
    non_noise_indices <- which(dbscan_result$cluster != 100) # Outliers are now marked as "100"
    noise_indices <- which(dbscan_result$cluster == 100)

    # Include or exclude outliers in the Mclust clustering based on settings
    data_for_clustering <- if (settings$excludeOutliers) tsne_data[non_noise_indices, ] else tsne_data
    indices_for_clustering <- if (settings$excludeOutliers) non_noise_indices else seq_len(nrow(tsne_data))

    if (settings$excludeOutliers) {
        message("Excluding outliers from Mclust clustering.")
    } else {
        message("Including outliers in Mclust clustering.")
    }


    if (length(indices_for_clustering) >= 2) {
        message(paste("====> Number of indices for clustering: ", length(indices_for_clustering)))

        mc.norm <- mclust::Mclust(data_for_clustering, G = settings$clustGroups)

        # Ensure that pandora_cluster has the correct levels, including from mc.norm$classification
        mc_classification <- as.factor(mc.norm$classification)
        levels(info.norm$pandora_cluster) <- union(levels(info.norm$pandora_cluster), levels(mc_classification))

        # Assign clustering results back to info.norm$pandora_cluster
        if(length(indices_for_clustering) < nrow(tsne_data)) {
            info.norm$pandora_cluster[indices_for_clustering] <- mc_classification
        } else {
            info.norm$pandora_cluster <- mc_classification
        }

        # Replace NA values with 100 specifically
        na_indices <- is.na(info.norm$pandora_cluster)
        info.norm$pandora_cluster[na_indices] <- "100"

        # Calculate distances based on the exact data used for clustering
        distance_matrix <- dist(data_for_clustering)
        # Ensure cluster labels are integers and align with the distance matrix
        cluster_labels <- as.integer(factor(info.norm$pandora_cluster[indices_for_clustering]))

        # Calculate silhouette scores using the aligned data
        silhouette_scores <- cluster::silhouette(cluster_labels, distance_matrix)
        if(is.matrix(silhouette_scores)) {
            # Extract the silhouette widths from the scores
            silhouette_widths <- silhouette_scores[, "sil_width"]
            avg_silhouette_score <- mean(silhouette_widths, na.rm = TRUE)
        }

        # Handle noise/outlier indices
        if(length(noise_indices) > 0){
            message(paste("====> Noise indices: ", length(noise_indices)))
            if(!"100" %in% levels(info.norm$pandora_cluster)) {
                info.norm$pandora_cluster <- factor(info.norm$pandora_cluster, levels = c(levels(info.norm$pandora_cluster), "100"))
                info.norm$pandora_cluster[noise_indices] <- "100"
            }
        }

    } else {
        warning("Not enough data points for hierarchical clustering.")
    }

    # Ensure all cluster assignments, including outliers marked as "100", are recognized as valid levels
    info.norm$pandora_cluster <- factor(info.norm$pandora_cluster, levels = unique(as.character(info.norm$pandora_cluster)))

    # Compute cluster centers based on final clustering results
	lc.cent <- info.norm %>%
	    group_by(pandora_cluster) %>%
	    summarise(
	        tsne1 = median(tsne1, na.rm = TRUE),
	        tsne2 = median(tsne2, na.rm = TRUE),
	        .groups = 'drop'
	    )

	# Adjust "100" cluster separately (outliers)
	lc.cent <- lc.cent %>%
	    mutate(
	        tsne1 = ifelse(pandora_cluster == "100", tsne1 + settings$pointSize / 2, tsne1),
	        tsne2 = ifelse(pandora_cluster == "100", tsne2 + settings$pointSize / 2, tsne2)
	    )

    # Compute cluster sizes (number of samples per cluster)
    cluster_sizes <- info.norm %>%
      group_by(pandora_cluster) %>%
      summarise(num_samples = n(), .groups = 'drop') # Calculate the number of samples in each cluster
    # Join the cluster sizes back to the lc.cent dataframe to include the number of samples per cluster
    lc.cent <- lc.cent %>%
      left_join(cluster_sizes, by = "pandora_cluster")
    # Create the 'label' column that combines cluster ID and number of samples
    lc.cent <- lc.cent %>%
      mutate(label = paste(pandora_cluster, "-", num_samples))
    # Drop the 'num_samples' column if you no longer need it
    lc.cent <- select(lc.cent, -num_samples)

    return(list(info.norm = info.norm, cluster_data = lc.cent, avg_silhouette_score = avg_silhouette_score))
}


#' Perform Density-Based Clustering on t-SNE Results Using DBSCAN
#'
#' This function applies Density-Based Spatial Clustering of Applications with Noise (DBSCAN) 
#' on t-SNE results to identify clusters and detect noise points. It dynamically calculates the 
#' `MinPts` and `eps` parameters based on the t-SNE results and settings provided. Additionally, 
#' the function computes silhouette scores to evaluate cluster quality and returns cluster centroids 
#' along with cluster sizes.
#'
#' @param info.norm A data frame containing the normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results object, including the 2D t-SNE coordinates (`Y` matrix).
#' @param settings A list of settings for the DBSCAN clustering. These settings include:
#' - `minPtsAdjustmentFactor`: A factor to adjust the minimum number of points required to form a cluster (MinPts).
#' - `epsQuantile`: The quantile used to determine the `eps` value for DBSCAN.
#'
#' @importFrom dplyr group_by select summarise n across left_join
#' @importFrom stats dist
#' @importFrom cluster silhouette
#'
#' @return A list containing:
#' - `info.norm`: The input data frame with an additional `pandora_cluster` column for cluster assignments.
#' - `cluster_data`: A data frame with cluster centroids and labeled clusters.
#' - `avg_silhouette_score`: The average silhouette score, providing a measure of clustering quality.
#'
#' @details
#' The function first calculates `MinPts` based on the dimensionality of the t-SNE data and adjusts 
#' it using the provided `minPtsAdjustmentFactor`. The `eps` value is determined dynamically from the 
#' k-nearest neighbors distance using the quantile specified by `epsQuantile`. DBSCAN is then applied 
#' to the t-SNE data, and any NA values in the cluster assignments are replaced with a predefined 
#' outlier cluster ID (100). Finally, the function calculates cluster centroids, sizes, and silhouette 
#' scores to evaluate cluster separation and quality.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' tsne_results <- Rtsne::Rtsne(matrix(runif(200), ncol = 2)) # Generate random t-SNE data
#' settings <- list(minPtsAdjustmentFactor = 1.5, epsQuantile = 0.9)
#' result <- cluster_tsne_density(info.norm = data.frame(matrix(runif(200), ncol = 2)), 
#'                                tsne.norm = tsne_results, settings = settings)
#' print(result$cluster_data)
#' }
#'
#' @export
cluster_tsne_density <- function(info.norm, tsne.norm, settings){
    set.seed(1337)

    # Prepare data for DBSCAN
    tsne_data <- tsne.norm$Y

    # Calculate minPts and eps dynamically based on settings
    minPts_baseline <- dim(tsne_data)[2] * 2
    minPts <- max(2, settings$minPtsAdjustmentFactor * minPts_baseline)
    k_dist <- dbscan::kNNdist(tsne_data, k = minPts - 1)

    eps_quantile <- settings$epsQuantile
    eps <- stats::quantile(k_dist, eps_quantile)

	ds.norm = fpc::dbscan(tsne_data, eps = eps, MinPts = minPts)
	info.norm$pandora_cluster = factor(ds.norm$cluster)

    print(paste("====> Density-based clustering"))

    # Replace NA values with 100 specifically
    na_indices <- is.na(info.norm$pandora_cluster)
    info.norm$pandora_cluster[na_indices] <- 100

    # Compute the distance matrix based on t-SNE results
    distance_matrix <- dist(tsne_data)
    silhouette_scores <- cluster::silhouette(as.integer(info.norm$pandora_cluster), distance_matrix)
    if(is.matrix(silhouette_scores)) {
        # Extract the silhouette widths from the scores
        silhouette_widths <- silhouette_scores[, "sil_width"]
        avg_silhouette_score <- mean(silhouette_widths, na.rm = TRUE)
    }

    # Compute cluster centers based on final clustering results
    lc.cent <- info.norm %>%
        group_by(pandora_cluster) %>%
        summarise(across(c(tsne1, tsne2), ~ median(.x, na.rm = TRUE)), .groups = 'drop')

    # Compute cluster sizes (number of samples per cluster)
    cluster_sizes <- info.norm %>%
      group_by(pandora_cluster) %>%
      summarise(num_samples = n(), .groups = 'drop') # Calculate the number of samples in each cluster
    # Join the cluster sizes back to the lc.cent dataframe to include the number of samples per cluster
    lc.cent <- lc.cent %>%
      left_join(cluster_sizes, by = "pandora_cluster")
    # Create the 'label' column that combines cluster ID and number of samples
    lc.cent <- lc.cent %>%
      mutate(label = paste(pandora_cluster, "-", num_samples))
    # Drop the 'num_samples' column if you no longer need it
    lc.cent <- select(lc.cent, -num_samples)


	return(list(info.norm = info.norm, cluster_data = lc.cent, avg_silhouette_score = avg_silhouette_score))
}

#' Automated Machine Learning Model Building
#'
#' This function automates the process of building machine learning models using the caret package. 
#' It supports both binary and multi-class classification and allows users to specify a list of 
#' machine learning algorithms to be trained on the dataset. The function splits the dataset into 
#' training and testing sets, applies preprocessing steps, and trains models using cross-validation.
#' It computes relevant performance metrics such as confusion matrix, AUROC (for binary classification), 
#' and prAUC (for binary classification).
#'
#' @param dataset_ml A data frame containing the dataset for training. All columns except the outcome 
#'   column should contain the features.
#' @param settings A list containing the following parameters:
#'   \itemize{
#'     \item{\code{outcome}}: A string specifying the name of the outcome column in \code{dataset_ml}. Defaults to "immunaut" if not provided.
#'     \item{\code{excludedColumns}}: A vector of column names to be excluded from the training data. Defaults to \code{NULL}.
#'     \item{\code{preProcessDataset}}: A vector of preprocessing steps to be applied (e.g., \code{c("center", "scale", "medianImpute")}). Defaults to \code{NULL}.
#'     \item{\code{selectedPartitionSplit}}: A numeric value specifying the proportion of data to be used for training. Must be between 0 and 1. Defaults to 0.7.
#'     \item{\code{selectedPackages}}: A character vector specifying the machine learning algorithms to be used for training (e.g., \code{"nb"}, \code{"rpart"}). Defaults to \code{c("nb", "rpart")}.
#'   }
#'
#' @details
#' The function performs preprocessing (e.g., centering, scaling, and imputation of missing values) on the dataset based on the provided settings. 
#' It splits the data into training and testing sets using the specified partition, trains models using cross-validation, and computes performance metrics.
#' 
#' For binary classification problems, the function calculates AUROC and prAUC. For multi-class classification, it calculates macro-averaged AUROC, though prAUC is not used.
#' 
#' The function returns a list of trained models along with their performance metrics, including confusion matrix, variable importance, and post-resample metrics.
#'
#' @return A list where each element corresponds to a trained model for one of the algorithms specified in 
#'   \code{settings$selectedPackages}. Each element contains:
#'   \itemize{
#'     \item{\code{info}}: General information about the model, including resampling indices, problem type, 
#'         and outcome mapping.
#'     \item{\code{training}}: The trained model object and variable importance.
#'     \item{\code{predictions}}: Predictions on the test set, including probabilities, confusion matrix, 
#'         post-resample statistics, AUROC (for binary classification), and prAUC (for binary classification).
#'   }
#'
#' @importFrom caret train createDataPartition trainControl confusionMatrix varImp postResample
#' @importFrom pROC roc auc
#' @importFrom PRROC pr.curve
#' @importFrom stats as.formula
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' dataset_ml <- generate_demo_data(n_subjects = 1000, n_features = 200)
#' settings <- list(
#'   selectedPackages = c("nb", "rpart"),
#'   selectedPartitionSplit = 0.7
#' )
#' results <- auto_simon_ml(dataset_ml, settings)
#' }
#'
#' @export
auto_simon_ml <- function(dataset_ml, settings) {
    set.seed(1337)  # Set seed for reproducibility
    
    if (is_var_empty(settings$outcome) == TRUE) {
        settings$outcome = "immunaut"
    }

    if (is_var_empty(settings$excludedColumns) == TRUE) {
        settings$excludedColumns = NULL
    }

    if (is_var_empty(settings$preProcessDataset) == TRUE) {
        settings$preProcessDataset = NULL
    }
    if (is_var_empty(settings$selectedPartitionSplit) == TRUE) {
        settings$selectedPartitionSplit = 0.7
    }
    if (is_var_empty(settings$selectedPackages) == TRUE) {
        settings$selectedPackages = c("nb", "rpart")
    }
    
    ## If the outcome column is not found, return an error
    if (!settings$outcome %in% colnames(dataset_ml)) {
        stop(paste(
            "Outcome column",
            settings$outcome,
            "not found in the dataset."
        ))
    }

    ##  Exclude columns from the dataset if specified
    if (!is.null(settings$excludedColumns)) {
        dataset_ml <- dataset_ml[, !colnames(dataset_ml) %in% settings$excludedColumns]
    }
    
    ## If no packages are selected, return an error
    if (length(settings$selectedPackages) == 0) {
        stop("No machine learning packages selected for training.")
    }
    
    ## If the selected partition split is invalid, return an error
    if (settings$selectedPartitionSplit <= 0 ||
        settings$selectedPartitionSplit >= 1) {
        stop("Invalid partition split value. Please choose a value between 0 and 1.")
    }


    # Preprocess dataset
    if (!is.null(settings$preProcessDataset)) {
        preProcessMapping <- preProcessResample(dataset_ml, settings$preProcessDataset, settings$outcome, settings$outcome)
        dataset_ml <- preProcessMapping$datasetData

    }else if (is.null(settings$preProcessDataset) && anyNA(dataset_ml)) {
        message("No preprocessing steps specified, but missing values detected. Applying default median imputation.")
        # Apply default preprocessing (median imputation for NAs, and optionally scaling/centering)
        preProcessMapping <- preProcessResample(dataset_ml, 
                                                c("medianImpute", "scale", "center"), 
                                                settings$outcome, 
                                                c(settings$outcome, settings$excludedColumns))
        dataset_ml <- preProcessMapping$datasetData
    }

    ## Make sure outcome levels are valid
    levels(dataset_ml[[settings$outcome]]) <- make.names(levels(dataset_ml[[settings$outcome]]))
    
    # Create an empty list to store model results
    model_list <- list()
    
    # Prepare data
    outcome_col <- dataset_ml[[settings$outcome]]
    
    # Encode outcome to factor for classification and apply make.names to the levels
    if (!is.factor(outcome_col)) {
        outcome_col <- as.factor(outcome_col)
    }
    
    # Ensure factor levels are valid R variable names
    levels(outcome_col) <- make.names(levels(outcome_col))
    
    # Split the data into training and testing sets
    trainIndex <-
        caret::createDataPartition(outcome_col,
                                   p = settings$selectedPartitionSplit,
                                   list = FALSE)
    trainData <- dataset_ml[trainIndex,]
    testData <- dataset_ml[-trainIndex,]
    
    # Determine if the problem is binary or multi-class classification
    is_binary_classification <- length(unique(outcome_col)) == 2
    
    
    # Iterate through the selected packages (models) and train them
    for (model_name in settings$selectedPackages) {
        message(paste("===> INFO: Training model:", model_name))
        
        # Define the control object for cross-validation
        trainControlObj <- caret::trainControl(
            method = "cv",
            # Cross-validation
            number = 5,
            # 5-fold CV
            savePredictions = "final",
            # Save predictions for final model
            classProbs = TRUE,
            # Compute class probabilities
            summaryFunction = if (is_binary_classification)
                caret::twoClassSummary
            else
                caret::multiClassSummary
        )
        
        # Train the model
        trained_model <- caret::train(
            as.formula(paste(settings$outcome, "~ .")),
            data = trainData,
            method = model_name,
            # e.g., "nb" for naive Bayes
            trControl = trainControlObj,
            metric = "ROC", # Use ROC as a performance metric
            preProcess = NULL
            
        )
        
        # Make predictions on the test set
        predictions <- predict(trained_model, newdata = testData)
        probabilities <-
            predict(trained_model, newdata = testData, type = "prob")
        
        # Calculate performance metrics
        prediction_confusion_matrix <-
            caret::confusionMatrix(predictions, testData[[settings$outcome]])
        post_resample <-
            caret::postResample(predictions, testData[[settings$outcome]])
        
        # For binary classification, calculate AUROC and prAUC
        if (is_binary_classification) {
            roc_obj <- pROC::roc(testData[[settings$outcome]], probabilities[, 2])
            auroc <- pROC::auc(roc_obj)
            prAUC <- PRROC::pr.curve(
                scores.class0 = probabilities[, 2],
                weights.class0 = testData[[settings$outcome]] == levels(testData[[settings$outcome]])[2]
            )$auc.integral
        } else {
            # Macro-Averaged AUROC for multi-class classification
            roc_list <-
                lapply(levels(testData[[settings$outcome]]), function(cls) {
                    pROC::roc(testData[[settings$outcome]] == cls, probabilities[, cls])
                })
            auroc <-
                mean(sapply(roc_list, pROC::auc))  # Macro average
            prAUC <-
                NA  # Likewise, prAUC isn't used for multi-class
        }
        
        # Store the model details in the model list
        model_list[[model_name]] <- list(
            info = list(
                resampleID = trainIndex,
                problemType = if (is_binary_classification)
                    "Binary Classification"
                else
                    "Multi-Class Classification",
                data = trainData,
                outcome = settings$outcome,
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
        
        message(paste("===> INFO: Finished training model:", model_name))
    }
    
    # Return the list of models with details
    return(model_list)
}
