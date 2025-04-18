% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{cluster_tsne_mclust}
\alias{cluster_tsne_mclust}
\title{Apply Mclust Clustering on t-SNE Results}
\usage{
cluster_tsne_mclust(info.norm, tsne.norm, settings)
}
\arguments{
\item{info.norm}{A data frame containing the normalized data on which the t-SNE analysis was carried out.}

\item{tsne.norm}{The t-SNE results object, including the 2D t-SNE coordinates (\code{Y} matrix).}

\item{settings}{A list of settings for the clustering analysis, including:
\itemize{
\item \code{clustGroups}: The number of groups (clusters) for Mclust to fit.
\item \code{minPtsAdjustmentFactor}: A factor to adjust the minimum number of points required to form a cluster (MinPts) in DBSCAN.
\item \code{epsQuantile}: The quantile used to determine the \code{eps} value for DBSCAN.
\item \code{excludeOutliers}: A logical value indicating whether to exclude outliers detected by DBSCAN from the Mclust clustering.
\item \code{pointSize}: A numeric value used to adjust the placement of outlier centroids.
}}
}
\value{
A list containing:
\itemize{
\item \code{info.norm}: The input data frame with an additional \code{pandora_cluster} column for cluster assignments.
\item \code{cluster_data}: A data frame with cluster centroids and labeled clusters.
\item \code{avg_silhouette_score}: The average silhouette score, providing a measure of clustering quality.
}
}
\description{
This function performs Mclust clustering on the 2D t-SNE results, which are derived from high-dimensional data.
It includes an initial outlier detection step using DBSCAN, and the user can specify whether to exclude outliers
from the clustering process. Silhouette scores are computed to evaluate the quality of the clustering, and cluster
centroids are returned for visualization, with outliers handled separately.
}
\details{
The function first uses DBSCAN to detect outliers (marked as cluster "100") and then applies Mclust clustering on the t-SNE
results. Outliers can be either included or excluded from the clustering, depending on the settings. Silhouette scores are
calculated to assess the quality of the clustering. Cluster centroids are returned, along with the sizes of each cluster,
and outliers are handled separately in the centroid calculation.
}
\keyword{internal}
