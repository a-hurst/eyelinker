context("SR research test files")

test_that("test_files_load", {

    # Read in all test files into a single named list
    files <- c(
        "mono250.asc.gz", "mono500.asc.gz", "mono1000.asc.gz", "mono2000.asc.gz",
        "bino250.asc.gz", "bino500.asc.gz", "bino1000.asc.gz",
        "monoRemote250.asc.gz", "monoRemote500.asc.gz",
        "binoRemote250.asc.gz", "binoRemote500.asc.gz"
    )
    ascs <- lapply(files, function(f) {
        fpath <- system.file(paste0("extdata/", f), package = "eyelinker")
        read_asc(fpath)
    })
    names(ascs) <- files

    # Test general structure & attributes
    for (f in files) {
        expect_equal(length(ascs[[f]]), 8)
        expect_equal(ascs[[f]]$info$model, "EyeLink 1000 Plus")
    }

    # Test specific attributes of different files
    expect_equal(ascs[["mono250.asc.gz"]]$info$sample.rate, 250)
    expect_equal("remote.info" %in% names(ascs[["monoRemote500.asc.gz"]]$raw), TRUE)

    # Test parse_all functionality
    fpath <- system.file("extdata/mono2000.asc.gz", package = "eyelinker")
    a <- read.asc(fpath, parse_all = TRUE)
    expect_equal(nrow(a$msg), 150)
    expect_equal(sum(a$msg$block %% 1 != 0), 118)
})
