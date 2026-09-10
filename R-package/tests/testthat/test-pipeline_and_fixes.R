# Unit tests for pipeline robustness, clustering methods, edge cases, and plot generation

test_that("normalize() handles NA and constant vectors safely", {
    x_na <- c(1, 2, NA, 4, 5)
    res_na <- immunaut:::normalize(x_na)
    expect_equal(res_na[1], 0)
    expect_equal(res_na[5], 1)
    expect_true(is.na(res_na[3]))

    x_const <- c(3, 3, 3)
    res_const <- immunaut:::normalize(x_const)
    expect_equal(res_const, rep(0.5, 3))

    x_all_na <- c(NA_real_, NA_real_)
    res_all_na <- immunaut:::normalize(x_all_na)
    expect_equal(res_all_na, rep(0.5, 2))
})

test_that("is_var_empty() accurately identifies empty variables", {
    expect_true(immunaut:::is_var_empty(NULL))
    expect_true(immunaut:::is_var_empty(""))
    expect_true(immunaut:::is_var_empty(character(0)))
    expect_false(immunaut:::is_var_empty("valid"))
    expect_false(immunaut:::is_var_empty(123))
    expect_false(immunaut:::is_var_empty(data.frame(a = 1)))
})

test_that("remove_outliers() handles NAs safely without dropping valid rows", {
    df_outliers <- data.frame(
        pandora_cluster = factor(c("1", "2", "100", NA)),
        feat = 1:4
    )
    settings_out <- list(datasetAnalysisRemoveOutliersDownstream = TRUE)
    cleaned_df <- immunaut:::remove_outliers(df_outliers, settings_out)
    expect_equal(nrow(cleaned_df), 3)
    expect_false("100" %in% cleaned_df$pandora_cluster)
})

test_that("plot_clustered_tsne() produces valid ggplot and preserves global session theme", {
    info_norm <- data.frame(
        tsne1 = c(1.2, 2.3, -1.1, -2.0),
        tsne2 = c(0.5, -0.8, 1.4, -1.2),
        pandora_cluster = c("Cluster_A", "Cluster_B", "Cluster_A", "Cluster_B")
    )
    cluster_data <- data.frame(
        tsne1 = c(0.05, 0.15),
        tsne2 = c(0.95, -1.0),
        pandora_cluster = c("Cluster_A", "Cluster_B"),
        label = c("Cluster_A - 2", "Cluster_B - 2")
    )
    plot_settings <- list(
        theme = "theme_bw",
        colorPalette = "Set1",
        pointSize = 3,
        fontSize = 10,
        legendPosition = "bottom"
    )
    old_theme <- ggplot2::theme_get()
    p <- plot_clustered_tsne(info_norm, cluster_data, plot_settings)
    expect_s3_class(p, "ggplot")
    expect_identical(ggplot2::theme_get(), old_theme)

    pdf(file = NULL)
    g <- ggplot2::ggplotGrob(p)
    dev.off()
    expect_s3_class(g, "gtable")
})

test_that("immunaut() runs end-to-end with demo data and handles missing values cleanly", {
    set.seed(42)
    demo_data <- generate_demo_data(n_subjects = 50, n_features = 6, missing_prob = 0, desired_number_clusters = 3)
    expect_true(is.data.frame(demo_data))

    # Test with minimal settings
    minimal_settings <- list(
        seed = 42,
        clusterType = "Density",
        minPtsAdjustmentFactor = 1,
        epsQuantile = 0.8
    )
    res_density <- immunaut(dataset = demo_data, settings = minimal_settings)
    expect_true(!is.null(res_density$dataset$dataset_ml))
    expect_true("immunaut" %in% names(res_density$dataset$dataset_ml))
    expect_equal(nrow(res_density$dataset$dataset_ml), nrow(demo_data))

    # Test with removeNA = TRUE
    demo_na <- demo_data
    demo_na[1, "Feature.1"] <- NA
    demo_na[2, "Feature.2"] <- NA
    settings_na <- list(
        seed = 42,
        removeNA = TRUE,
        clusterType = "Density",
        minPtsAdjustmentFactor = 1,
        epsQuantile = 0.8
    )
    res_na <- immunaut(dataset = demo_na, settings = settings_na)
    expect_true("immunaut" %in% names(res_na$dataset$dataset_ml))
    expect_equal(nrow(res_na$dataset$dataset_ml), nrow(demo_data) - 2)
})

test_that("pick_best_cluster_overall evaluates normalized candidate metric vectors correctly", {
    set.seed(42)
    demo_data <- generate_demo_data(n_subjects = 50, n_features = 6, missing_prob = 0, desired_number_clusters = 3)
    settings_overall <- list(
        seed = 42,
        clusterType = "Louvain",
        pickBestClusterMethod = "Overall",
        resolution_increments = c(0.2, 0.6),
        min_modularities = c(0.2, 0.4),
        target_clusters_range = c(2, 5)
    )
    res_overall <- immunaut(dataset = demo_data, settings = settings_overall)
    expect_true(!is.null(res_overall$tsne_clust))
    expect_true(res_overall$tsne_clust$num_clusters >= 2)
})
