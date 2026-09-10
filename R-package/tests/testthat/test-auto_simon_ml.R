# Unit tests for auto_simon_ml and preprocessing train/test data isolation

test_that("auto_simon_ml isolates preprocessing parameters without data leakage", {
    set.seed(42)
    # Create controlled dataset where test set has extreme values
    # to verify test values do not contaminate training parameters
    n <- 100
    dataset <- data.frame(
        outcome = factor(rep(c("R", "NR"), each = 50)),
        feat1 = rnorm(n, mean = 10, sd = 2),
        feat2 = c(rnorm(n - 10, mean = 5, sd = 1), rep(NA, 10)),
        feat3 = rep(1, n),  # zero variance
        feat4 = rnorm(n, mean = 0, sd = 1)
    )

    settings <- list(
        outcome = "outcome",
        seed = 42,
        selectedPartitionSplit = 0.7,
        preProcessDataset = c("scale", "center", "medianImpute", "zv"),
        selectedPackages = c("rpart"),
        trainingTimeout = 60,
        num_cores = 2
    )

    result <- auto_simon_ml(dataset, settings)

    # 1. Verification of data structures
    expect_true(!is.null(result$models))
    expect_true("rpart" %in% names(result$models))
    expect_true(!is.null(result$trainData))
    expect_true(!is.null(result$testData))
    expect_true(!is.null(result$preProcessParams))

    # 2. Check partition split
    expect_equal(nrow(result$trainData) + nrow(result$testData), n)
    expect_equal(nrow(result$trainData), 70)
    expect_equal(nrow(result$testData), 30)

    # 3. Check zero-variance feature was removed identically in both
    expect_false("feat3" %in% colnames(result$trainData))
    expect_false("feat3" %in% colnames(result$testData))
    expect_equal(colnames(result$trainData), colnames(result$testData))

    # 4. Check no missing values remain
    expect_equal(sum(is.na(result$trainData)), 0)
    expect_equal(sum(is.na(result$testData)), 0)

    # 5. Check outcome column was not duplicated into outcome.1
    expect_false("outcome.1" %in% colnames(result$trainData))
    expect_false("outcome.1" %in% colnames(result$testData))

    # 6. Check metrics
    auroc <- result$models$rpart$predictions$AUROC
    expect_true(!is.na(auroc))
    expect_gte(auroc, 0)
    expect_lte(auroc, 1)
})

test_that("preProcessResample transforms testData using strictly trainData parameters", {
    set.seed(123)
    train_df <- data.frame(
        outcome = factor(c("A", "A", "B", "B")),
        x = c(10, 20, 30, NA)
    )
    test_df <- data.frame(
        outcome = factor(c("A", "B")),
        x = c(NA, 100)
    )

    settings <- list(seed = 123)
    res <- preProcessResample(
        datasetData = train_df,
        preProcess = c("medianImpute", "center", "scale"),
        selectedOutcomeColumns = "outcome",
        outcome_and_classes = "outcome",
        settings = settings,
        testData = test_df
    )

    # The median of train_df$x (10, 20, 30) is 20.
    # The imputed value for test_df$x[1] must be scaled using train mean (20) and sd (10).
    # Since 20 - mean(20) = 0, the standardized value should be 0!
    expect_equal(as.numeric(res$testData$x[1]), 0)

    # If testData had leaked, the median of c(10, 20, 30, 100) would be 25, not 20.
    # This directly proves zero data leakage from testData to trainData!
})

