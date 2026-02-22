## Test some edge cases not covered in the example files ##

# Test for error when file contains no "START" messages
test_that("test_err_on_no_data", {
    expect_error(read.asc(I("hello\nthere")))
})


# Fully test raw colname generating code
test_that("test_raw_colnames", {

    # Test monocular first
    info <- list(
        mono = TRUE, velocity = TRUE, resolution = TRUE,
        input = TRUE, buttons = TRUE, tracking = TRUE, htarg = TRUE
    )
    colnames <- get_raw_header(info)
    expect_equal(length(colnames), 15)

    # Test binocular too
    info$mono <- FALSE
    colnames <- get_raw_header(info)
    expect_equal(length(colnames), 20)
})


# Test old mount names with get_mount code
test_that("test_old_mount_names", {

    m <- get_mount("MTABLE")
    expect_equal(m, "Desktop / Monocular / Head Stabilized")

    m <- get_mount("RTABLE")
    expect_equal(m, "Desktop / Monocular / Remote")
})


# Test href and res with event headers
test_that("test_event_headers", {

    info <- list(event.dtype = "GAZE", resolution = TRUE)
    fix_header <- get_fix_header(info)
    expect_equal(tail(fix_header, 2), c("xr", "yr"))

    info$event.dtype <- "HREF"
    fix_header <- get_fix_header(info)
    expect_equal(tail(fix_header, 2), c("xr", "yr"))
    expect_equal("href.aps" %in% fix_header, TRUE)
})


# Test resolution parsing from .asc
test_that("test_get_resolution", {

    # Test case when equals sign present (rare, but happens)
    lines <- c("MSG\t1446373 DISPLAY_COORDS = 0 0 1024 768", "")
    res <- get_resolution(lines)
    expect_equal(res, c(1025, 769))

    # Test GAZE_COORDS parsing where screen size is in floats
    lines <- c("MSG\t2361384 GAZE_COORDS 0.00 0.00 1279.00 1023.00", "")
    res <- get_resolution(lines)
    expect_equal(res, c(1280, 1024))
})


# Test detection of different EyeLink models
test_that("test_get_model", {

    # EyeLink 1
    header <- c(
        "** VERSION: EYELINK REVISION 2.00 (Aug 12 1997)",
        "** RECORDED BY PCXSCR.EXE"
    )
    tracker_info <- get_model(header)
    expect_equal(tracker_info[1], "EyeLink I")
    expect_equal(tracker_info[2], "2.00")

    # EyeLink II
    header <- c(
        "** VERSION: EYELINK II 1",
        "** SOURCE: EYELINK II",
        "** EYELINK II v2.31 Mar 13 2010"
    )
    tracker_info <- get_model(header)
    expect_equal(tracker_info[1], "EyeLink II")
    expect_equal(tracker_info[2], "2.31")

    # EyeLink 1000
    header <- c(
        "** VERSION: EYELINK II 1",
        "** SOURCE: EYELINK CL",
        "** EYELINK II CL v4.56 Aug 18 2010"
    )
    tracker_info <- get_model(header)
    expect_equal(tracker_info[1], "EyeLink 1000")
    expect_equal(tracker_info[2], "4.56")

    # Unspecified
    header <- c("** TYPE: EDF_FILE BINARY EVENT SAMPLE TAGGED")
    tracker_info <- get_model(header)
    expect_equal(tracker_info[1], "Unknown")
    expect_equal(tracker_info[2], "Unknown")
})


# Test different functions and edge cases for raw sample parsing
test_that("test_process_raw", {

    info <- list(
        mono = TRUE, velocity = FALSE, resolution = FALSE, cr = FALSE,
        input = FALSE, buttons = FALSE, tracking = TRUE, htarg = FALSE
    )
    lines <- c(
        "5578547\t 967.5\t 540.2\t 2231.0\t .",
        "5578551\t 967.9\t 540.0\t 2233.0\t .",
        "5578555\t     .\t     .\t    0.0\t ."
    )

    # Test handling of unnecessary cr.info column
    raw <- process_raw(lines, c(1, 1, 1), info)
    expect_equal(ncol(raw), 5)
    expect_equal(nrow(raw), 3)

    # Test handling of single row only
    raw <- process_raw(lines[1], 1, info)
    expect_equal(ncol(raw), 5)
    expect_equal(nrow(raw), 1)

    # Test handling of unknown columns
    info$tracking <- FALSE
    raw <- expect_warning(process_raw(lines, c(1, 1, 1), info))
    expect_equal(ncol(raw), 6)
    expect_equal(nrow(raw), 3)
    expect_equal("X1" %in% names(raw), TRUE)

    # Test handling of files w/ no samples
    raw <- process_raw(c(), c(), info)
    expect_equal(ncol(raw), 5)
    expect_equal("xp" %in% names(raw), TRUE)

    # Test handling of random rows with too few columns
    info$tracking <- TRUE
    lines[3] <- "5578555\t     .\t     .\t    0.0"
    raw <- process_raw(lines, c(1, 1, 1), info)
    expect_equal(ncol(raw), 5)
    expect_equal(nrow(raw), 2)

    # Test handling of random rows with too many columns
    lines[3] <- "5578555\t   .\t   .\t    0.0\t   .\t   .\t    0.0"
    raw <- process_raw(lines, c(1, 1, 1), info)
    expect_equal(ncol(raw), 5)
    expect_equal(nrow(raw), 2)
})


# Test htarget info handling
test_that("test_remote_info", {

    # Test handling of files w/ HTARG in the header but not in samples
    info <- list(htarg = TRUE, mono = TRUE)
    lines <- c(
        "5578551\t 967.9\t 540.0\t 2233.0\t ...",
        "SFIX R   7545971",
        "5578555\t     .\t     .\t    0.0\t ..."
    )
    out <- handle_htarg(lines, info, c(TRUE, FALSE, TRUE))
    expect_equal(out[[2]]$htarg, FALSE)
    expect_equal(length(out[[1]]), 3)

    # Test regex for htarget detection
    lines <- c(
        "5578551\t 967.9\t 540.0\t 2233.0\t ...\t -128.0\t -262.0\t 285.6 .............",
        "5578551\t 967.9\t 540.0\t 2233.0\t ...\t -128.0\t -262.0\t 285.6 ...C.TBLRTB..",
        "5578551\t 967.9\t 540.0\t 2233.0\t ...\t -128.0\t -262.0\t 285.6 ...C.TBL.T.L.",
        "5578551\t 967.9\t 540.0\t 2233.0\t ...\t -128.0\t -262.0\t 285.6 ...CF..L...LR",
        "5578551\t 967.9\t 540.0\t 2233.0\t ..."
    )
    regex <- get_htarg_regex(binocular = FALSE)
    expect_equal(all(grepl(regex, lines[1:4])), TRUE)
    expect_equal(all(grepl(regex, lines[5])), FALSE)

    # Test adding of tab separator before remote.info lines
    out <- handle_htarg(lines, info, rep(TRUE, 5))
    tab_counts <- stri_count_fixed(out[[1]], "\t")
    expect_equal(out[[2]]$htarg, TRUE)
    expect_equal(all(tab_counts[1:4] == 8), TRUE)
    expect_equal(tab_counts[5] == 4, TRUE)
})


# Test event processing edge cases
test_that("test_events", {

    # Test handling of events w/ only one row
    info <- list(resolution = FALSE, event.dtype = "GAZE")
    lines <- c("EFIX R   7545831\t 7545935\t 108\t 355.7\t 228.2\t 130")
    events <- process_fixations(lines, 1, info)
    expect_equal(ncol(events), 8)
    expect_equal(nrow(events), 1)

    # Test handling of files w/ no messages
    msgs <- process_messages(c(), c())
    expect_equal(ncol(msgs), 3)

    # Test handling of NAs in saccades
    info$event.dtype <- "HREF"
    lines <- c(
        "ESACC L  1000370 1000399 30 26 -61 . . 90.09 404 966.1 477.3 . . 2.1e+06 404",
        "ESACC L  1001968 1002251 284 92 -160 88 -149 0.04 453 982.3 452.6 981.4 455.4 0.05 453"
    )
    sacc <- process_saccades(lines, c(1, 1), info)
    expect_equal(all(is.na(c(sacc$ampl[1], sacc$href.ampl[1]))), TRUE)
    expect_equal(any(is.na(c(sacc$ampl[2], sacc$href.ampl[2]))), FALSE)
})


# Test block division edge cases
test_that("test_block_parsing", {

    testfile <- c(
        "** VERSION: EYELINK II 1",
        "** SOURCE: EYELINK II",
        "** EYELINK II v2.31 Mar 13 2010",
        "MSG\t2035323 GAZE_COORDS 0.00 0.00 1919.00 1199.00",
        "MSG\t2035324 !MODE RECORD CR 250 2 1",
        "START\t 2035326\t LEFT\t RIGHT\t SAMPLES\t EVENTS",
        "PRESCALER\t 1",
        "VPRESCALER\t 1",
        "PUPIL\t AREA",
        "EVENTS\tGAZE\tLEFT\tRIGHT\tRATE\t 250.00\tTRACKING\tCR\tFILTER\t2",
        "SAMPLES\tGAZE\tLEFT\tRIGHT\tRATE\t 250.00\tTRACKING\tCR\tFILTER\t2",
        "2035326\t 955.4\t 602.7\t 1022.0\t 959.0\t 602.2\t 1193.0\t.....",
        "2035330\t 955.5\t 602.1\t 1022.0\t 958.7\t 601.6\t 1192.0\t.....",
        "2035334\t 956.8\t 602.4\t 1022.0\t 958.9\t 602.4\t 1191.0\t.....",
        "MSG\t2035338 TRIAL_ID 1"
    )

    # Test handling of no END line
    a <- read.asc(I(testfile))
    expect_equal(nrow(a$raw), 3)
    expect_equal("TRIAL_ID 1" %in% a$msg$text, TRUE)

    # Test handling of END line as last line
    testfile <- c(testfile, "END\t598729\t SAMPLES\t EVENTS\t RES\t 41.91\t 38.82")
    a <- read.asc(I(testfile))
    expect_equal(nrow(a$raw), 3)
    expect_equal("TRIAL_ID 1" %in% a$msg$text, TRUE)

    # Test handling of content after last END line
    testfile <- c(testfile, "MSG\t601379 trialResult 3")
    a <- read.asc(I(testfile))
    expect_equal(nrow(a$raw), 3)
    expect_equal("TRIAL_ID 1" %in% a$msg$text, TRUE)

    # Test handling of content after last END line when parse_all is TRUE
    a <- read.asc(I(testfile), parse_all = TRUE)
    expect_equal(nrow(a$raw), 3)
    expect_equal("TRIAL_ID 1" %in% a$msg$text, TRUE)
    expect_equal("trialResult 3" %in% a$msg$text, TRUE)
    expect_equal(subset(a$msg, block == 1.5)$text[1], "trialResult 3")
})
