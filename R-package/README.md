immunaut
================
Ivan Tomic <info@ivantomic.com>

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation

You can install the released version of **immunaut** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("immunaut")
```

Or you can install **immunaut** directly from **GitHub** with use of
following commands:

``` r
# install.packages("devtools")
devtools::install_github("atomiclaboratory/immunaut", subdir = 'R-package')
```

## Initial setup

``` r
library("immunaut")

# Generate a demo dataset with 1000 subjects, 200 features, 4 clusters, and a 10% probability of missing values
dataset <- generate_demo_data(n_subjects = 1000, n_features = 200, 
                                desired_number_clusters = 4, # Approximate number of clusters
                                cluster_overlap_sd = 35, # Standard deviation for cluster overlap
                                missing_prob = 0.1) # Probability of missing values

# Generate a file header for the dataset to use in downstream analysis
file_header <- generate_file_header(dataset)

settings <- list(
    fileHeader = file_header,
    seed = 1337,
    selectedColumns = colnames(dataset),  # Columns selected for analysis
    # Exclude outcome, age, and gender columns from the analysis
    excludedColumns = c("outcome", "age", "gender"),
    removeNA = TRUE,
    
    clusterType = "Louvain",
    target_clusters_range = c(3,4),
    resolution_increments =c(0.01, 0.1, 0.2, 0.3, 0.4),
    min_modularities = c(0.5, 0.6, 0.7, 0.8),
    pickBestClusterMethod = "Modularity",
    weights = list(AUROC = 0.9, modularity = 0.05, silhouette = 0.05),
    
    preProcessDataset = c("scale", "center", "medianImpute", "corr", "zv", "nzv"),
    selectedPartitionSplit = 0.7,  # Use the current partition split
    selectedPackages = c("rf", "gcvEarth"),
    trainingTimeout = 180
)
```

## Example 1: Perform t-SNE and Louvain Clustering and Machine Learning

``` r
# Perform t-SNE and Louvain clustering using the 'immunaut' function
result <- immunaut(dataset, settings)

# Plot the clustered t-SNE results using ggplot2
p <- plot_clustered_tsne(result$tsne_clust$info.norm, 
                                result$tsne_clust$cluster_data, 
                                result$settings) 
print(p) # Display the plot
```

<img src="man/figures/README-example-1-1.png" width="100%" />

``` r

# Extract the dataset with the applied clustering from the result
dataset_ml <- result$dataset$dataset_ml
# Run the auto_simon_ml function to train machine learning models on the dataset
model_results <- auto_simon_ml(dataset_ml, settings)

# Extract the names of the models
model_names <- names(model_results$models)

# Create a data frame to store the model names and their corresponding AUROC values
model_auroc_table <- data.frame(
  Model = character(),
  AUROC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the models and extract AUROC values (One-vs-Rest) for Multiclass Models
for (model_name in model_names) {
  auroc_value <- model_results$models[[model_name]][["predictions"]][["AUROC"]]
  # Add the model name and its AUROC to the table
  model_auroc_table <- rbind(model_auroc_table, data.frame(Model = model_name, AUROC = auroc_value))
}

library(ggplot2)
# Create a bar chart with AUROC values
ggplot(model_auroc_table, aes(x = Model, y = AUROC, fill = Model)) +
  geom_bar(stat = "identity") +  # Create bars
  geom_text(aes(label = round(AUROC, 3)), vjust = -0.5) +  # Add AUROC values above bars
  ggtitle("AUROC for Models") +
  xlab("Model") + 
  ylab("AUROC") +
  theme_minimal() +  # Use a minimal theme
  scale_fill_brewer(palette = "Set3")
```

<img src="man/figures/README-example-1-2.png" width="100%" />

## Example 2: Switch to DBSCAN Clustering

``` r
# Update settings for DBSCAN clustering
settings$clusterType <- "Density"
settings$minPtsAdjustmentFactor <- 1.5
settings$epsQuantile <- 0.9

# Run t-SNE and DBSCAN clustering
dbscan_result <- immunaut(dataset, settings)
#> [1] "====> Density-based clustering"
```

## Example 3: Perform Mclust Clustering

``` r
# Update settings for Mclust clustering
settings$clusterType <- "Mclust"
settings$clustGroups <- 3  # Specify the number of clusters for Mclust

# Run t-SNE and Mclust clustering
mclust_result <- immunaut(dataset, settings)
#> [1] "==> cluster_tsne_mclust clustGroups:  3"
#> fitting ...
#>   |                                                                                                                                            |                                                                                                                                    |   0%  |                                                                                                                                            |=========                                                                                                                           |   7%  |                                                                                                                                            |==================                                                                                                                  |  13%  |                                                                                                                                            |==========================                                                                                                          |  20%  |                                                                                                                                            |===================================                                                                                                 |  27%  |                                                                                                                                            |============================================                                                                                        |  33%  |                                                                                                                                            |=====================================================                                                                               |  40%  |                                                                                                                                            |==============================================================                                                                      |  47%  |                                                                                                                                            |======================================================================                                                              |  53%  |                                                                                                                                            |===============================================================================                                                     |  60%  |                                                                                                                                            |========================================================================================                                            |  67%  |                                                                                                                                            |=================================================================================================                                   |  73%  |                                                                                                                                            |==========================================================================================================                          |  80%  |                                                                                                                                            |==================================================================================================================                  |  87%  |                                                                                                                                            |===========================================================================================================================         |  93%  |                                                                                                                                            |====================================================================================================================================| 100%
```

## Example 4: Perform Hierarchical Clustering

``` r
# Update settings for Hierarchical clustering
settings$clusterType <- "Hierarchical"
settings$clustLinkage <- "ward.D2"
settings$clustGroups <- 3

# Run t-SNE and Mclust clustering
hierarchical_result <- immunaut(dataset, settings)
#> [1] "====> Noise indices:  25"
#> [1] "====> Noise indices done"
```
