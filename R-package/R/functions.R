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
#' @keywords internal
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
#' @param tsne.norm A list containing the t-SNE results, including a 2D t-SNE coordinate matrix in the `Y` element.
#' @param settings A list of settings for the analysis, including:
#' \itemize{
#'   \item `knn_clusters`: The number of nearest neighbors to use for KNN (default: 250).
#'   \item `target_clusters_range`: A numeric vector specifying the target range for the number of clusters.
#'   \item `start_resolution`: The starting resolution for Louvain clustering.
#'   \item `end_resolution`: The maximum resolution to test.
#'   \item `min_modularity`: The minimum acceptable modularity for valid clusterings.
#' }
#' @param resolution_increment The step size for incrementing the Louvain clustering resolution. Defaults to 0.1.
#' @param min_modularity The minimum modularity score allowed for a valid clustering. Defaults to 0.5.
#'
#' @importFrom FNN get.knn
#' @importFrom igraph graph_from_data_frame simplify cluster_louvain membership modularity
#' @importFrom dplyr group_by select summarise across left_join n mutate
#' @importFrom cluster silhouette
#' @importFrom stats dist median
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item `info.norm`: The input data frame with an additional `pandora_cluster` column for cluster assignments.
#'   \item `cluster_data`: A data frame containing cluster centroids and cluster labels.
#'   \item `avg_silhouette_score`: The average silhouette score, a measure of clustering quality.
#'   \item `modularity`: The modularity score of the Louvain clustering.
#'   \item `num_clusters`: The number of clusters found.
#' }
#'
#' @details 
#' This function begins by constructing a KNN graph from the t-SNE results, then applies the Louvain algorithm for 
#' community detection. The KNN parameter is dynamically adjusted based on the size of the dataset to ensure scalability. 
#' The function evaluates clustering quality using silhouette scores and calculates cluster centroids for visualization. 
#' NA cluster assignments are handled by assigning them to a separate cluster labeled as "100."
#'
#' @keywords internal
cluster_tsne_knn_louvain <- function(info.norm, tsne.norm, settings, resolution_increment = 0.1, min_modularity = 0.5){

    # Adjust KNN clusters if needed
    knn_clusters <- settings$knn_clusters
    if (nrow(tsne.norm$Y) < knn_clusters) {
        knn_clusters <- round(nrow(tsne.norm$Y) / 2)
        message(paste0("===> INFO: Adjusted KNN clusters to half the number of samples: ", knn_clusters))
    }
    # Determine the dimensionality of the dataset
    n_obs <- nrow(tsne.norm$Y)
    n_vars <- ncol(tsne.norm$Y)

    # Choose the nearest neighbor algorithm based on dimensionality
    algorithm <- if (n_vars > n_obs * 0.15) {
        "cover_tree"  # Preferred for higher dimensionality
    } else if (n_vars < 30) {
        "kd_tree"  # Better suited for lower dimensions
    } else {
        "brute"  # Default option for general cases
    }

	knn.norm = FNN::get.knn(as.matrix(tsne.norm$Y), k = knn_clusters, algorithm = algorithm)
	knn.norm = data.frame(
					from = rep(1:nrow(knn.norm$nn.index), knn_clusters), 
					to = as.vector(knn.norm$nn.index), 
					weight = 1/(1 + as.vector(knn.norm$nn.dist)))

    # Build graph from KNN results and simplify it
	nw.norm = igraph::graph_from_data_frame(knn.norm, directed = FALSE)
	nw.norm = igraph::simplify(nw.norm)

    # Find optimal resolution for Louvain clustering
    resolution <- find_optimal_resolution(nw.norm, 
                                            start_resolution = 0.1, 
                                            end_resolution = 10, 
                                            resolution_increment = resolution_increment, 
                                            min_modularity = min_modularity, 
                                            target_clusters_range = settings$target_clusters_range
                                         )
    if (is.null(resolution)) {
        optimal_resolution <- NULL
    } else {
        optimal_resolution <- resolution$selected$optimal_resolution
    }
    
    # Initialize a list to store the results
    cluster_results <- list()
    num_clusters_list <- numeric(10)

    for (i in 1:10) {
        if (is.null(optimal_resolution)) {
            lc.norm <- igraph::cluster_louvain(nw.norm)
        }else{
            lc.norm <- igraph::cluster_louvain(nw.norm, resolution = optimal_resolution)
        }
        # Log cluster results
        num_clusters <- length(unique(igraph::membership(lc.norm)))
        # Store the result
        cluster_results[[i]] <- lc.norm
        num_clusters_list[i] <- num_clusters
    }
    
    # Find the most frequent number of clusters
    most_frequent_clusters <- as.numeric(names(sort(table(num_clusters_list), decreasing = TRUE)[1]))
    # Get the corresponding lc.norm where the number of clusters matches the most frequent one
    lc.norm <- cluster_results[[which(num_clusters_list == most_frequent_clusters)[1]]]

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

    return(list(info.norm = info.norm, cluster_data = lc.cent, 
        avg_silhouette_score = avg_silhouette_score, modularity = modularity, 
        num_clusters = num_clusters,
        resolution = resolution))
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
#'
#' @keywords internal
cluster_tsne_hierarchical <- function(info.norm, tsne.norm, settings) {
    set.seed(settings$seed)
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
#'
#' @keywords internal
cluster_tsne_mclust <- function(info.norm, tsne.norm, settings) {
    set.seed(settings$seed)
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
#'
#' @keywords internal
cluster_tsne_density <- function(info.norm, tsne.norm, settings){
    set.seed(settings$seed)

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
#' @importFrom R.utils withTimeout
#' @importFrom parallel detectCores makePSOCKcluster stopCluster
#' @importFrom doParallel registerDoParallel
#'
#' @examples
#' \dontrun{
#' dataset <- read.csv("fc_wo_noise.csv", header = TRUE, row.names = 1)
#' 
#' # Generate a file header for the dataset to use in downstream analysis
#' file_header <- generate_file_header(dataset)
#' 
#' settings <- list(
#'     fileHeader = file_header,
#'     # Columns selected for analysis
#'     selectedColumns = c("ExampleColumn1", "ExampleColumn2"), 
#'     clusterType = "Louvain",
#'     removeNA = TRUE,
#'     preProcessDataset = c("scale", "center", "medianImpute", "corr", "zv", "nzv"),
#'     target_clusters_range = c(3,4),
#'     resolution_increments = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
#'     min_modularities = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9),
#'     pickBestClusterMethod = "Modularity",
#'     seed = 1337
#' )
#' 
#' result <- immunaut(dataset, settings)
#' dataset_ml <- result$dataset$original
#' dataset_ml$pandora_cluster <- tsne_clust[[i]]$info.norm$pandora_cluster
#' dataset_ml <- dplyr::rename(dataset_ml, immunaut = pandora_cluster)
#' dataset_ml <- dataset_ml[, c("immunaut", setdiff(names(dataset_ml), "immunaut"))]
#' settings_ml <- list(
#'     excludedColumns = c("ExampleColumn0"),
#'     preProcessDataset = c("scale", "center", "medianImpute", "corr", "zv", "nzv"),
#'     selectedPartitionSplit = split,  # Use the current partition split
#'     selectedPackages = c("rf", "RRF", "RRFglobal", "rpart2", "c5.0", "sparseLDA", 
#'     "gcvEarth", "cforest", "gaussPRPoly", "monmlp", "slda", "spls"),
#'     trainingTimeout = 180  # Timeout 3 minutes
#' )
#' ml_results <- auto_simon_ml(dataset_ml, settings_ml)
#' }
#'
#' @export
auto_simon_ml <- function(dataset_ml, settings) {
    set.seed(settings$seed)
    

    if (is_var_empty(settings$outcome) == TRUE) {
        settings$outcome = "immunaut"
    }

    if (is_var_empty(settings$selectedColumns) == TRUE) {
        settings$selectedColumns = NULL
    }

    if (is_var_empty(settings$excludedColumns) == TRUE) {
        settings$excludedColumns = NULL
    }

    if (is_var_empty(settings$trainingTimeout) == TRUE) {
        settings$trainingTimeout = 180
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
    if (!is.null(settings$selectedColumns)) {
        message(paste("Selected columns: ", paste(c(settings$outcome, settings$selectedColumns), collapse = ", ")))
        dataset_ml <- dataset_ml[, c(settings$outcome, settings$selectedColumns)]
    }

    ##  Exclude columns from the dataset if specified
    if (!is.null(settings$excludedColumns)) {
        message(paste("Excluding columns: ", paste(settings$excludedColumns, collapse = ", ")))
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
        preProcessMapping <- preProcessResample(dataset_ml, settings$preProcessDataset, settings$outcome, settings$outcome, settings)
        dataset_ml <- preProcessMapping$datasetData

    }else if (is.null(settings$preProcessDataset) && anyNA(dataset_ml)) {
        message("No preprocessing steps specified, but missing values detected. Applying default median imputation.")
        # Apply default preprocessing (median imputation for NAs, and optionally scaling/centering)
        preProcessMapping <- preProcessResample(dataset_ml, 
                                                c("medianImpute", "scale", "center"), 
                                                settings$outcome, 
                                                c(settings$outcome, settings$excludedColumns), settings)
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


    message(paste0("===> INFO: Unique outcome levels: ", paste(levels(dataset_ml[[settings$outcome]]), collapse = ", ")))
    message(paste0("===> INFO: Dataset columns after preprocessing: ", paste(colnames(dataset_ml), collapse = ", ")))
    
    # Split the data into training and testing sets
    trainIndex <-
        caret::createDataPartition(outcome_col,
                                   p = settings$selectedPartitionSplit,
                                   list = FALSE)
    trainData <- dataset_ml[trainIndex,]
    testData <- dataset_ml[-trainIndex,]
    
    # Determine if the problem is binary or multi-class classification
    is_binary_classification <- length(unique(outcome_col)) == 2

    message(paste("===> INFO: Problem type:", ifelse(is_binary_classification, "Binary Classification", "Multi-Class Classification")))
    

    num_cores <- parallel::detectCores(logical = TRUE)
    cl <- parallel::makePSOCKcluster(num_cores-1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl)) 

    # Iterate through each model in selectedPackages for training and evaluation
    for (model_name in settings$selectedPackages) {
        message(paste("===> INFO: Training model:", model_name))
        
        # Wrap model training in tryCatch and timeout for error and timeout handling
        tryCatch({
            R.utils::withTimeout({
                
                # Define control for cross-validation, using summary function based on classification type
                train_control <- caret::trainControl(
                    method = "cv",
                    number = 5,
                    savePredictions = "final",
                    classProbs = TRUE,
                    summaryFunction = if (is_binary_classification)
                        caret::twoClassSummary
                    else
                        caret::multiClassSummary
                )
                
                # Train the model with specified formula, data, method, and control
                trained_model <- caret::train(
                    as.formula(paste(settings$outcome, "~ .")),
                    data = trainData,
                    method = model_name,
                    trControl = train_control,
                    metric = "ROC",  # Use ROC as a performance metric
                    preProcess = c("center", "scale")  # Standardize data
                )
                
                # Generate predictions on test data
                predictions <- predict(trained_model, newdata = testData)
                probabilities <- predict(trained_model, newdata = testData, type = "prob")
                
                # Calculate confusion matrix and resampling metrics
                confusion_matrix <- caret::confusionMatrix(predictions, testData[[settings$outcome]])
                post_resample_metrics <- caret::postResample(predictions, testData[[settings$outcome]])
                
                # Initialize AUROC and prAUC placeholders
                auroc <- NA
                prAUC <- NA
                
                # Performance metrics for binary and multi-class classifications
                if (is_binary_classification) {
                    # Binary AUROC and prAUC
                    roc_curve <- pROC::roc(testData[[settings$outcome]], probabilities[, 2])
                    auroc <- pROC::auc(roc_curve)
                    prAUC <- PRROC::pr.curve(
                        scores.class0 = probabilities[, 2],
                        weights.class0 = testData[[settings$outcome]] == levels(testData[[settings$outcome]])[2]
                    )$auc.integral
                } else {
                    # Macro-averaged AUROC for multi-class
                    roc_curves <- lapply(levels(testData[[settings$outcome]]), function(class) {
                        pROC::roc(testData[[settings$outcome]] == class, probabilities[, class])
                    })
                    auroc <- mean(sapply(roc_curves, pROC::auc), na.rm = TRUE)
                    
                    # Weighted AUROC for imbalanced classes
                    class_counts <- table(testData[[settings$outcome]])
                    total_samples <- sum(class_counts)
                    weighted_auroc <- sum(sapply(seq_along(levels(testData[[settings$outcome]])), function(i) {
                        class_name <- levels(testData[[settings$outcome]])[i]
                        class_roc <- roc_curves[[i]]
                        pROC::auc(class_roc) * (class_counts[class_name] / total_samples)
                    }), na.rm = TRUE)
                    
                    # Calculate macro-averaged F1 score
                    f1_scores <- sapply(levels(testData[[settings$outcome]]), function(class) {
                        caret::confusionMatrix(predictions, testData[[settings$outcome]], mode = "prec_recall")$byClass["F1"]
                    })
                    macro_f1 <- mean(f1_scores, na.rm = TRUE)
                }
                
                # Store model details and metrics in the model list
                model_list[[model_name]] <- list(
                    info = list(
                        resampleID = trainIndex,
                        problemType = if (is_binary_classification) "Binary Classification" else "Multi-Class Classification",
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
                        weightedAUROC = if (!is_binary_classification) weighted_auroc else NA,
                        macroF1 = if (!is_binary_classification) macro_f1 else NA,
                        postResample = post_resample_metrics,
                        confusionMatrix = confusion_matrix
                    )
                )
                
                message(paste("===> INFO: Finished training model:", model_name))
            }, timeout = settings$trainingTimeout)  # Set timeout for 3 minutes (180 seconds)
        }, TimeoutException = function(ex) {
            message(paste("===> ERROR: Model training for", model_name, "timed out."))
        }, error = function(e) {
            message(paste("===> ERROR: Failed to train model:", model_name, "Error message:", e$message))
        })
    }

    
    # Return the list of models with details
    return(list(models = model_list, dataset = dataset_ml, trainData = trainData, testData = testData, is_binary_classification = is_binary_classification))
}

#' Select the Best Clustering Based on Weighted Scores: AUROC, Modularity, and Silhouette
#'
#' This function selects the optimal clustering configuration from a list of `t-SNE` clustering results
#' by evaluating each configuration's AUROC, modularity, and silhouette scores. These scores are combined
#' using a weighted average, allowing for a more comprehensive assessment of each configuration's relevance.
#'
#' @param dataset A data frame representing the original dataset, where each observation will be assigned cluster labels
#'                from each clustering configuration in \code{tsne_clust}.
#' @param tsne_clust A list of clustering results from different t-SNE configurations, with each element containing 
#'                   \code{pandora_cluster} assignments and clustering information.
#' @param tsne_calc An object containing t-SNE results on \code{dataset}.
#' @param settings A list of settings for machine learning model training and scoring, including:
#' \describe{
#'   \item{excludedColumns}{A character vector of columns to exclude from the analysis.}
#'   \item{preProcessDataset}{A character vector of preprocessing steps (e.g., scaling, centering).}
#'   \item{selectedPartitionSplit}{Numeric; the partition split ratio for train/test splits.}
#'   \item{selectedPackages}{Character vector of machine learning models to train.}
#'   \item{trainingTimeout}{Numeric; time limit (in seconds) for training each model.}
#'   \item{weights}{A list of weights for scoring criteria: \code{weights$AUROC}, \code{weights$modularity}, 
#'   and \code{weights$silhouette} (default is 0.4, 0.3, and 0.3 respectively).}
#' }
#'
#' @details
#' For each clustering configuration in \code{tsne_clust}, this function:
#' \enumerate{
#'   \item Assigns cluster labels to the dataset.
#'   \item Trains machine learning models specified in \code{settings} on the dataset with cluster labels.
#'   \item Evaluates each model based on AUROC, modularity, and silhouette scores.
#'   \item Selects the clustering configuration with the highest weighted average score as the best clustering result.
#' }
#'
#' @return A list containing the best clustering configuration (with the highest weighted score) and its associated information.
#'
#' @import dplyr
#' 
#' @keywords internal
pick_best_cluster_simon <- function(dataset, tsne_clust, tsne_calc, settings) {
    if (length(tsne_clust) == 0) {
        stop("The tsne_clust list is empty.")
    }
    
    # Set default weights if not provided in settings
    weights <- settings$weights
    if (is.null(weights)) {
        weights <- list(AUROC = 0.5, modularity = 0.3, silhouette = 0.2)
    }

    # Initialize arrays for holding each metric
    all_aurocs <- numeric(length(tsne_clust))
    all_modularities <- numeric(length(tsne_clust))
    all_silhouettes <- numeric(length(tsne_clust))

    # Gather AUROC, modularity, and silhouette for all clusters
    for (i in seq_along(tsne_clust)) {
        dataset_ml <- dataset
        dataset_ml$pandora_cluster <- tsne_clust[[i]]$info.norm$pandora_cluster
        dataset_ml <- dplyr::rename(dataset_ml, immunaut = pandora_cluster)
        dataset_ml <- dataset_ml[, c("immunaut", setdiff(names(dataset_ml), "immunaut"))]

        # Train models on current cluster configuration and calculate average AUROC
        ml_results <- auto_simon_ml(dataset_ml, settings)
        model_auroc_table <- data.frame(Model = character(), AUROC = numeric(), stringsAsFactors = FALSE)

        for (model_name in names(ml_results$models)) {
            auroc_value <- if (ml_results$is_binary_classification) {
                ml_results$models[[model_name]][["predictions"]][["AUROC"]]
            } else {
                if (!is.na(ml_results$models[[model_name]][["predictions"]][["weightedAUROC"]])) {
                    ml_results$models[[model_name]][["predictions"]][["weightedAUROC"]]
                } else {
                    ml_results$models[[model_name]][["predictions"]][["AUROC"]]
                }
            }
            if (!is.na(auroc_value)) {
                model_auroc_table <- rbind(model_auroc_table, data.frame(Model = model_name, AUROC = auroc_value))
            }
        }

        # Calculate the average AUROC for the top 5 models and store in arrays
        all_aurocs[i] <- mean(utils::head(model_auroc_table[order(model_auroc_table$AUROC, decreasing = TRUE), "AUROC"], 5), na.rm = TRUE)
        all_modularities[i] <- tsne_clust[[i]]$modularity
        all_silhouettes[i] <- tsne_clust[[i]]$avg_silhouette_score
    }

    # Apply normalization to all metrics
    norm_aurocs <- normalize(all_aurocs)
    norm_modularities <- normalize(all_modularities)
    norm_silhouettes <- normalize(all_silhouettes)

    # Track the best score and associated cluster
    best_score <- -Inf
    best_cluster <- NULL

    # Calculate and evaluate the combined score for each configuration
    for (i in seq_along(tsne_clust)) {
        combined_score <- (weights$AUROC * norm_aurocs[i]) + 
                          (weights$modularity * norm_modularities[i]) + 
                          (weights$silhouette * norm_silhouettes[i])

        # Logging for diagnostics
        message(paste("===> SIMON: Cluster", i,
                      " Clusters: ", tsne_clust[[i]]$num_clusters,
                      " MOD:", round(norm_modularities[i], 3),
                      " SILH:", round(norm_silhouettes[i], 3),
                      " AUROC:", round(norm_aurocs[i], 3),
                      " CS:", round(combined_score, 3)))

        # Update best cluster if this one has a higher score
        if (combined_score > best_score) {
            best_score <- combined_score
            best_cluster <- list(
                tsne_clust = tsne_clust[[i]],
                combined_score = combined_score,
                modularity = all_modularities[i],
                silhouette = all_silhouettes[i],
                auroc = all_aurocs[i]
            )
        }
    }

    if (is.null(best_cluster)) {
        stop("No valid clustering result found with non-missing values for AUROC, modularity, and silhouette.")
    }

    message(paste("===> INFO: Best clustering selected with combined score:", round(best_score, 3)))
    return(best_cluster)
}


#' Pick Best Cluster by Modularity
#'
#' This function selects the best cluster from a list of clustering results
#' based on the highest modularity score.
#'
#' @param tsne_clust A list of clustering results where each element contains clustering information,
#' including the modularity score.
#'
#' @return Returns the clustering result with the highest modularity score.
#' 
#' @details The function iterates over a list of clustering results (`tsne_clust`) and 
#' selects the cluster with the highest modularity score. If no clusters are valid or 
#' the `tsne_clust` list is empty, the function will stop and return an error.
#' 
#'
#' @keywords internal
pick_best_cluster_modularity <- function(tsne_clust) {
    if (length(tsne_clust) == 0) {
        stop("The tsne_clust list is empty.")
    }
    
    best_modularity <- -Inf
    best_cluster <- NULL
    
    # Iterate through tsne_clust to find the cluster with the highest modularity
    for (i in seq_along(tsne_clust)) {
        modularity <- tsne_clust[[i]]$modularity
        
        if (!is.null(modularity) && modularity > best_modularity) {
            best_modularity <- modularity
            best_cluster <- tsne_clust[[i]]
        }
    }
    
    if (is.null(best_cluster)) {
        stop("No valid clusters found.")
    }
    
    message(paste("===> INFO: Best cluster selected with modularity: ", best_modularity, " Clusters: ", best_cluster$num_clusters))
    
    return(best_cluster)
}

#' Pick Best Cluster by Silhouette Score
#'
#' This function selects the best cluster from a list of clustering results
#' based on the highest average silhouette score.
#'
#' @param tsne_clust A list of clustering results where each element contains clustering information,
#' including the average silhouette score.
#'
#' @return Returns the clustering result with the highest average silhouette score.
#' 
#' @details The function iterates over a list of clustering results (`tsne_clust`) and 
#' selects the cluster with the highest average silhouette score. If no clusters are valid or 
#' the `tsne_clust` list is empty, the function will stop and return an error.
#' 
#'
#' @keywords internal
pick_best_cluster_silhouette <- function(tsne_clust) {
    if (length(tsne_clust) == 0) {
        stop("The tsne_clust list is empty.")
    }
    
    best_silhouette <- -Inf
    best_cluster <- NULL
    
    # Iterate through tsne_clust to find the cluster with the highest average silhouette score
    for (i in seq_along(tsne_clust)) {
        silhouette <- tsne_clust[[i]]$avg_silhouette_score
        
        if (!is.null(silhouette) && silhouette > best_silhouette) {
            best_silhouette <- silhouette
            best_cluster <- tsne_clust[[i]]
        }
    }
    
    if (is.null(best_cluster)) {
        stop("No valid clusters found.")
    }
    
    message(paste("Best cluster selected with silhouette score:", best_silhouette, " Clusters: ", best_cluster$num_clusters))
    return(best_cluster)
}

#' Pick the Best Clustering Result Based on Multiple Metrics
#'
#' This function evaluates multiple clustering results based on various metrics such as modularity, silhouette score, 
#' Davies-Bouldin Index (DBI), and Calinski-Harabasz Index (CH). It normalizes the scores across all metrics, 
#' calculates a combined score for each clustering result, and selects the best clustering result.
#'
#' @param tsne_clust A list of clustering results. Each result should contain metrics such as modularity, silhouette score, 
#' and cluster assignments for the dataset.
#' @param tsne_calc A list containing the t-SNE results. It includes the t-SNE coordinates of the dataset used for clustering.
#'
#' @return The clustering result with the highest combined score based on modularity, silhouette score, 
#' Davies-Bouldin Index (DBI), and Calinski-Harabasz Index (CH).
#'
#' @details 
#' The function computes four different metrics for each clustering result:
#' \itemize{
#'   \item Modularity: A measure of the quality of the division of the network into clusters.
#'   \item Silhouette score: A measure of how similar data points are to their own cluster compared to other clusters.
#'   \item Davies-Bouldin Index (DBI): A ratio of within-cluster distances to between-cluster distances, with lower values being better.
#'   \item Calinski-Harabasz Index (CH): The ratio of the sum of between-cluster dispersion to within-cluster dispersion, with higher values being better.
#' }
#' The scores for each metric are normalized between 0 and 1, and an overall score is calculated for each clustering result. The clustering result with the highest overall score is selected as the best.
#'
#' @importFrom clusterSim index.DB
#' @importFrom fpc cluster.stats
#' @importFrom dplyr filter
#' 
#'
#' @keywords internal
pick_best_cluster_overall <- function(tsne_clust, tsne_calc) {
   if (length(tsne_clust) == 0) {
        stop("The tsne_clust list is empty.")
    }
    
    # Initialize a list to store scores for each clustering result
    score_list <- lapply(tsne_clust, function(clust) {
        # Ensure modularity and silhouette are numeric
        modularity <- as.numeric(clust$modularity)
        silhouette <- as.numeric(clust$avg_silhouette_score)
        
        # Convert cluster labels (pandora_cluster) to numeric if needed
        cluster_labels <- as.numeric(as.factor(clust$info.norm$pandora_cluster)) 
        
        # Calculate Davies-Bouldin Index (lower is better)
        dbi <- tryCatch({
            as.numeric(index.DB(tsne_calc$info.norm, cluster_labels)$DB)
        }, error = function(e) NA)  # Handle error in DB index calculation
        
        # Calculate Calinski-Harabasz Index (higher is better)
        ch_index <- tryCatch({
            as.numeric(cluster.stats(d = dist(tsne_calc$info.norm), cluster_labels)$ch)
        }, error = function(e) NA)  # Handle error in CH index calculation
        
        # Return a list with all scores
        list(modularity = modularity, 
             silhouette = silhouette, 
             dbi = dbi, 
             ch_index = ch_index)
    })
    
    # Remove any NULL or missing values from the score_list
    score_list <- Filter(function(x) {
        !is.null(x$modularity) && !is.null(x$silhouette) && !is.na(x$modularity) && !is.na(x$silhouette)
    }, score_list)
    
    # Check if there are any valid scores to evaluate
    if (length(score_list) == 0) {
        stop("No valid clustering results with all necessary metrics.")
    }
    
    # Debugging message: show score list length
    message("Valid score list calculated: ", length(score_list), " valid results")
    
    # Normalize and combine the scores for each clustering result
    combined_scores <- sapply(score_list, function(x) {
        if (any(is.na(c(x$modularity, x$silhouette, x$dbi, x$ch_index)))) {
            return(NA)  # Skip if any score is NA
        }
        mean(c(normalize(as.numeric(x$modularity)), 
               normalize(as.numeric(x$silhouette)), 
               1 - normalize(as.numeric(x$dbi)),  # Inverse DBI since lower is better
               normalize(as.numeric(x$ch_index))))
    })
    
    # Debugging message: show combined scores
    message("Combined scores: ", paste(combined_scores, collapse = ", "))
    
    # Check if combined_scores is empty or contains only NAs
    if (all(is.na(combined_scores))) {
        stop("No valid combined scores found.")
    }
    
    # Find the index of the clustering result with the highest combined score
    best_index <- which.max(combined_scores)
    
    # Check if best_index is valid
    if (length(best_index) == 0 || is.na(best_index)) {
        stop("Unable to find the best clustering result (no valid index found).")
    }
    
    # Return the best clustering result
    return(tsne_clust[[best_index]])
}
