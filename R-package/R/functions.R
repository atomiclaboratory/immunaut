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
#'
#' @export
calculate_tsne <- function(dataset, settings, removeGroups = TRUE){
    set.seed(1337)

    # Start logging
    message("===> Starting t-SNE calculation")

    info.norm <- dataset

    # Remap column names if necessary
    if (!is.null(settings$fileHeader)) {
        names(info.norm) <- plyr::mapvalues(names(info.norm), from = settings$fileHeader$remapped, to = settings$fileHeader$original)
        message("===> Remapped column names based on settings")
    }

    # Optionally remove grouping variables
    if (!is.null(settings$groupingVariables) && removeGroups == TRUE) {
        message(paste0("====> Removing grouping variables: ", toString(settings$groupingVariables)))
        dataset <- dataset %>% select(-any_of(settings$groupingVariables)) 
    }

    # Keep only numeric columns
    tsne_data <- dataset %>% select(where(is.numeric))

    # Ensure there are numeric columns to process
    if (ncol(tsne_data) < 1) {
        stop("Not enough numeric columns to perform t-SNE.")
    }

    # Remove zero variance columns
    tsne_data <- tsne_data %>% select(where(~ var(.) != 0))
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

    # Perform PCA to calculate initial_dims
    pca_result <- prcomp(tsne_data, scale. = TRUE)
    explained_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
    initial_dims <- ifelse(any(explained_variance <= 0.9), max(which(explained_variance <= 0.9)), 1)
    initial_dims <- min(max(initial_dims, 1), ncol(tsne_data), 100)

    message(paste0("Using initial_dims: ", initial_dims))

    # Adjust perplexity dynamically if not provided
    if (!is.null(settings$perplexity) && settings$perplexity > 0) {
        perplexity <- settings$perplexity
        message(paste0("Using provided perplexity: ", perplexity))
    } else {
        max_perplexity <- floor((num_samples - 1) / 3)
        if (max_perplexity < 1) stop("Not enough data to compute perplexity.")
        perplexity <- min(30, max_perplexity)
        message(paste0("Using dynamic perplexity: ", perplexity))
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

    message(paste0("Using max_iter: ", max_iter))
    message(paste0("Using theta: ", theta))
    message(paste0("Using eta: ", eta))
    message(paste0("Using exaggeration_factor: ", exaggeration_factor))

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

#' Cluster t-SNE Results Using KNN and Louvain Method
#'
#' This function performs clustering on t-SNE results using K-Nearest Neighbors (KNN) 
#' to build a graph and the Louvain method for community detection. It dynamically adjusts 
#' KNN parameters based on the dataset size and provides additional information 
#' about clustering quality using modularity and silhouette scores.
#'
#' @param info.norm A data frame containing the normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results object, including the 2D t-SNE coordinates.
#' @param settings A list of settings for the analysis, including `knn_clusters` and clustering options.
#'
#' @importFrom FNN get.knn
#' @importFrom igraph graph_from_data_frame simplify cluster_louvain membership modularity
#' @importFrom dplyr group_by select summarise across left_join n
#' @importFrom cluster silhouette
#' @importFrom stats median
#' @return A list containing:
#' - `info.norm`: The input data frame with an additional `pandora_cluster` column for cluster assignments.
#' - `cluster_data`: A data frame with cluster centers and labeled clusters.
#' - `avg_silhouette_score`: The average silhouette score for cluster quality evaluation.
#' - `modularity`: The modularity score of the Louvain clustering.
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
    print(paste0("===> INFO: Number of NA clusters in pandora_cluster: ", num_na_clusters))

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

#' Hierarchical clustering on t-SNE results
#'
#' This function applies Hierarchical Clustering on t-SNE results.
#'
#' @param info.norm The normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results.
#' @param settings A list of settings for the analysis.
#'
#' @importFrom stats hclust dist cutree
#' @importFrom dplyr group_by select summarise n left_join summarize_all %>%
#' @importFrom stats median
#'
#' @return A list containing the analyzed data with cluster assignment and cluster centroids.
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

        if(length(indices_for_clustering) < nrow(tsne_data)){
            info.norm$pandora_cluster[indices_for_clustering] <- as.factor(h_clusters)
        }else{
            info.norm$pandora_cluster <- as.factor(h_clusters)
        }

        # Replace NA values with 100 specifically
        na_indices <- is.na(info.norm$pandora_cluster)
        info.norm$pandora_cluster[na_indices] <- 100

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
#' @importFrom stats median
#'
#' @return A list containing the analyzed data with cluster assignment and cluster centroids.
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
        mc.norm <- mclust::Mclust(data_for_clustering, G = settings$clustGroups)

        if(length(indices_for_clustering) < nrow(tsne_data)){
            info.norm$pandora_cluster[indices_for_clustering] <- as.factor(mc.norm$classification)
        }else{
            info.norm$pandora_cluster <- as.factor(mc.norm$classification)
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


#' Density-based clustering on t-SNE results
#'
#' This function applies Density-Based Spatial Clustering of Applications with Noise (DBSCAN) on t-SNE results.
#'
#' @param info.norm The normalized data on which the t-SNE analysis was carried out.
#' @param tsne.norm The t-SNE results.
#' @param settings A list of settings for the analysis.
#'
#' @importFrom fpc dbscan
#' @importFrom dplyr group_by select summarise n across left_join
#' @importFrom stats dist
#' @importFrom cluster silhouette
#'
#' @return A list containing the analyzed data with cluster assignment and cluster centroids.
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
