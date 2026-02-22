#' Read EyeLink ASC Files
#'
#' Imports data from EyeLink ASC files into (relatively) tidy data frames for analysis and
#' visualization. Event data and/or raw sample data from the files can be imported, along with
#' information about the tracker hardware and configuration. All data is divided into numbered
#' blocks using the "START" and "END" messages in the ASC file.
#'
#' ASC files can contain anywhere between 125 to 2000 rows of samples for every second of recording,
#' meaning that the resulting files can be very large (1.2 million rows of samples for 20 minutes at
#' 1000Hz). As a result, importing some ASC files can be slow, and the resulting data frames can
#' take up 100's of MB of memory. To speed up import and greatly reduce memory load, you can choose
#' to ignore raw samples and only import events by setting the samples parameter to \code{FALSE}.
#'
#' This function returns a list containing the following possible data frames: \describe{
#'   \item{\code{raw}}{Raw sample data}
#'   \item{\code{sacc}}{Saccade end events}
#'   \item{\code{fix}}{Fixation end events}
#'   \item{\code{blinks}}{Blink end events}
#'   \item{\code{msg}}{Messages sent or received by the tracker}
#'   \item{\code{input}}{Input port (TTL) events}
#'   \item{\code{button}}{Button box / gamepad events}
#'   \item{\code{info}}{Tracker settings/configuration metadata}
#' }
#' The names of the columns in these data frames correspond to column names given in the ASC
#' section of the EyeLink 1000 User's Guide.
#'
#' Note that this function cannot import EDFs directly; they must be converted to plain-text ASC
#' using the edf2asc utility before importing.
#'
#' @usage
#' read_asc(fname, samples = TRUE, events = TRUE, parse_all = FALSE)
#'
#' @param fname \code{character} vector indicating the name of the .asc file to import.
#' @param samples \code{logical} indicating whether raw sample data should be imported. Defaults
#'   to \code{TRUE}.
#' @param events \code{logical} indicating whether event data (e.g. saccades, blinks, messages,
#'   etc.) should be imported. Defaults to \code{TRUE}.
#' @param parse_all \code{logical} indicating whether samples/events not within START/END
#'   blocks should be parsed. Defaults to \code{FALSE}.
#' @return A \code{list} of \code{\link[tibble]{tibble}}s containing data from the .asc file.
#'
#' @author Simon Barthelme & Austin Hurst
#' @examples
#' # Example file from SR research that ships with the package
#' fpath <- system.file("extdata/mono500.asc.gz", package = "eyelinker")
#' dat <- read_asc(fpath)
#' plot(dat$raw$time, dat$raw$xp, xlab = "Time (ms)", ylab = "Eye position along x-axis (px)")
#'
#' @export read_asc

# TODO:
#  - Check for multiple unique RECCFG lines, throw an error if so (can this even happen?)
#  - Add parsing of calibration & drift correct info
#  - Add function for reading multiple ASCs at once?
#  - Add function for parsing button samples? They're stored in a weird format


read_asc <- function(fname, samples = TRUE, events = TRUE, parse_all = FALSE) {

    inp <- read_lines(fname, progress = FALSE)

    # Convert to ASCII
    inp <- stri_enc_toascii(inp)

    # Get strings prior to first tab for each line for faster string matching
    inp_first <- stri_split_regex(inp, "\\s", 2, simplify = TRUE)[, 1]

    # Check if any actual data recorded in file
    starts <- inp_first == "START"
    if (!any(starts)) {
        stop("No samples or events found in .asc file.")
    }

    # Read metadata from file before processing
    is_raw <- str_detect(inp_first, "^[0-9]")
    info <- get_info(inp[!is_raw], inp_first[!is_raw])

    # Do some extra processing/sanitizing if there's HTARG info in the file
    if (info$htarg) {
        ret <- handle_htarg(inp, info, is_raw)
        inp <- ret[[1]]
        info <- ret[[2]]
    }

    # Find blocks and mark lines between block ENDs and next block STARTs
    dividers <- c(which(starts), length(inp))
    block <- cumsum(starts)
    for (i in 2:length(dividers)) {
        start <- dividers[i - 1]
        end <- dividers[i]
        endline <- which(inp_first[start:end] == "END") + start - 1
        if (length(endline) > 0 && endline < end) {
            block[endline[1]:end] <- block[endline[1]:end] + 0.5
        }
    }

    # Unless parsing all input, drop any lines not within a block
    block[1:dividers[1]] <- block[1:dividers[1]] + 0.5
    if (!parse_all) {
        in_block <- block %% 1 == 0
        inp <- inp[in_block]
        inp_first <- inp_first[in_block]
        is_raw <- is_raw[in_block]
        block <- block[in_block]
    }

    # Initialize list of data output and process different data types
    out <- list()
    if (samples) {
        out$raw <- process_raw(inp[is_raw], block[is_raw], info)
    }
    if (events) {
        is_sacc <- inp_first == "ESACC"
        out$sacc <- process_saccades(inp[is_sacc], block[is_sacc], info)

        is_fix <- inp_first == "EFIX"
        out$fix <- process_fixations(inp[is_fix], block[is_fix], info)

        is_blink <- inp_first == "EBLINK"
        out$blinks <- process_blinks(inp[is_blink], block[is_blink])

        is_msg <- inp_first == "MSG"
        out$msg <- process_messages(inp[is_msg], block[is_msg])

        is_input <- inp_first == "INPUT"
        out$input <- process_input(inp[is_input], block[is_input])

        is_button <- inp_first == "BUTTON"
        out$button <- process_buttons(inp[is_button], block[is_button])
    }
    info$tracking <- NULL # needed for parsing, but otherwise redundant with CR
    out$info <- info

    out
}


#' @rdname read_asc
#' @export
read.asc <- read_asc  # Alias for backwards compatibility


process_raw <- function(raw, blocks, info) {

    if (length(raw) == 0) {

        # If no sample data in file, create empty raw tibble w/ all applicable columns
        raw <- c("", "")
        blocks <- integer(0)
        colnames <- get_raw_header(info)
        coltypes <- get_coltypes(colnames, float_time = FALSE)

    } else {

        # Determine if timestamps stored as floats (edf2asc option -ftime, useful for 2000 Hz)
        float_time <- is_float(strsplit(raw[1], "\\s+")[[1]][1])

        # Generate column names and types based in info in header
        colnames <- get_raw_header(info)
        coltypes <- get_coltypes(colnames, float_time)

        # Discard any rows with too many or too few columns (usually rows where eye is missing)
        row_length <- stri_count_fixed(raw, "\t") + 1
        med_length <- median(row_length)
        raw <- raw[row_length == med_length]
        blocks <- blocks[row_length == med_length]

        # Verify that generated columns match up with actual maximum row length
        length_diff <- med_length - length(colnames)
        if (length_diff > 0) {
            warning(paste(
                "Unknown columns in raw data.",
                "Assuming first one is time, please check the others"
            ))
            colnames <- c("time", paste0("X", 1:(med_length - 1)))
            coltypes <- paste0(c("i", rep("?", med_length - 1)), collapse = "")
        }
    }

    # Process raw sample data using readr
    if (length(raw) == 1) raw <- c(raw, "")
    raw_df <- read_tsv(
        I(raw), col_names = colnames, col_types = coltypes, na = ".", progress = FALSE
    )
    if (info$tracking && !info$cr) {
        raw_df$cr.info <- NULL  # Drop CR column when not actually used
    }

    # Append block numbers to beginning of data frame
    raw_df <- add_column(raw_df, block = blocks, .before = 1)

    # Replace missing pupil data (zeros) with NAs
    if (!("X1" %in% names(raw_df))) {
        if (info$mono) {
            raw_df$ps[raw_df$ps == 0] <- NA
        } else {
            raw_df$psl[raw_df$psl == 0] <- NA
            raw_df$psr[raw_df$psr == 0] <- NA
        }
    }

    raw_df
}


process_events <- function(rows, blocks, colnames) {

    # If no data, create empty tibble w/ all cols and types
    if (length(rows) == 0) {
        rows <- c("", "")
        blocks <- integer(0)
    }

    # Parse data, dropping useless first columm
    if (length(rows) == 1) rows <- c(rows, "")
    colnames <- c("type", colnames) # first col is event type, which we drop later
    coltypes <- get_coltypes(colnames)
    df <- read_table(rows, col_names = colnames, col_types = coltypes, na = ".")[, -1]

    # Move eye col to end & make factor, append block numbers to beginning of data frame
    if ("eye" %in% colnames) {
        df <- df[, c(2:ncol(df), 1)]
        df$eye <- factor(df$eye, levels = c("L", "R"))
    }
    df <- add_column(df, block = blocks, .before = 1)

    df
}


process_saccades <- function(saccades, blocks, info) {

    sacc_df <- process_events(saccades, blocks, get_sacc_header(info))

    # Set amplitudes for any saccades missing start/end coords to NAs because they're wonky
    ampl_cols <- which(str_detect(names(sacc_df), "ampl"))
    partial <- is.na(sacc_df$sxp) | is.na(sacc_df$exp)
    if (any(partial)) sacc_df[partial, ampl_cols] <- NA

    sacc_df
}


process_fixations <- function(fixations, blocks, info) {
    process_events(fixations, blocks, get_fix_header(info))
}


process_blinks <- function(blinks, blocks) {
    process_events(blinks, blocks, c("eye", "stime", "etime", "dur"))
}


process_messages <- function(msgs, blocks) {

    # Process messages from tracker (needs stringi import)
    msg_mat <- stri_split_fixed(msgs, " ", 2, simplify = TRUE)
    msg_mat[, 1] <- substring(msg_mat[, 1], first = 5)
    msg_df <- as_tibble(msg_mat, .name_repair = ~ c("time", "text"))
    msg_df$time <- as.numeric(msg_df$time)

    # Append block numbers to beginning of data frame
    if (length(blocks) == 0) blocks <- integer(0)
    msg_df <- add_column(msg_df, block = blocks, .before = 1)

    msg_df
}


process_input <- function(input, blocks) {
    process_events(input, blocks, c("time", "value"))
}


process_buttons <- function(button, blocks) {
    process_events(button, blocks, c("time", "button", "state"))
}


handle_htarg <- function(inp, info, is_raw) {

    if (any(is_raw)) {
        # In some cases HTARGET is in header but not samples, so we verify it's really there
        first_sample <- inp[is_raw][1]
        htarg_regex <- get_htarg_regex(!info$mono)
        info$htarg <- str_detect(first_sample, htarg_regex)

        if (info$htarg) {
            # Sometimes remote.info has no tab separating it from previous column, so we have to
            # insert one ourselves for read_tsv to work
            htarg_notab_regex <- paste0("(.*[0-9\\.])\\s(", htarg_regex, ")$")
            has_htarg <- str_detect(inp[is_raw], htarg_notab_regex)
            split_pos <- ifelse(info$mono, -14, -18) # which character to replace w/ tab
            str_sub(inp[is_raw][has_htarg], split_pos, split_pos) <- "\t"
        }
    }

    list(inp, info)
}


# Gets useful metadata about the tracker setup & settings from the file
get_info <- function(nonsample, firstcol) {

    header <- nonsample[firstcol == "**"]
    info <- data.frame(
        date = NA, model = NA, version = NA, sample.rate = NA, cr = NA,
        left = NA, right = NA, mono = NA, screen.x = NA, screen.y = NA, mount = NA,
        filter.level = NA, sample.dtype = NA, event.dtype = NA, pupil.dtype = NA,
        velocity = NA, resolution = NA, htarg = NA, input = NA, buttons = NA,
        tracking = NA
    )

    # Get date/time of recording from file
    info$date <- as.POSIXct(from_header(header, "DATE"), format = "%a %b %d %H:%M:%S %Y")

    # Get tracker model/version info
    version_info <- get_model(header)
    info$model <- version_info[1]
    info$version <- version_info[2]

    # Get tracker mount info
    elclcfg <- nonsample[str_detect(nonsample, fixed("ELCLCFG"))]
    if (length(elclcfg) > 0) {
        info$mount <- get_mount(gsub(".* ELCLCFG\\s+(.*)", "\\1", elclcfg[1]))
    }

    # Get display size from file
    screen_res <- get_resolution(nonsample)
    info$screen.x <- screen_res[1]
    info$screen.y <- screen_res[2]

    # Get pupil size data type (area or diameter)
    pupil_config <- nonsample[firstcol == "PUPIL"]
    if (length(pupil_config) > 0) {
        info$pupil.dtype <- strsplit(tail(pupil_config, 1), "\\s+")[[1]][2]
    }

    # Find the samples and events config lines in the non-sample input, get data types
    events_config <- nonsample[firstcol == "EVENTS"]
    samples_config <- nonsample[firstcol == "SAMPLES"]
    if (length(events_config) > 0) {
        info$event.dtype <- strsplit(tail(events_config, 1), "\\s+")[[1]][2]
    }
    if (length(samples_config) > 0) {
        info$sample.dtype <- strsplit(tail(samples_config, 1), "\\s+")[[1]][2]
    }

    # Get last config line in file (preferring sample config) and extract remaining info
    config <- tail(c(events_config, samples_config), 1)
    if (length(config) > 0) {
        info$sample.rate <- ifelse(
            grepl("RATE", config),
            as.numeric(gsub(".*RATE\\s+([0-9]+\\.[0-9]+).*", "\\1", config)),
            NA
        )
        info$tracking <- grepl("\tTRACKING", config)
        info$cr <- grepl("\tCR", config)
        info$filter.level <- ifelse(
            grepl("FILTER", config),
            as.numeric(gsub(".*FILTER\\s+([0-9]).*", "\\1", config)),
            NA
        )
        info$velocity <- grepl("\tVEL", config)
        info$resolution <- grepl("\tRES", config)
        info$htarg <- grepl("\tHTARG", config)
        info$input <- grepl("\tINPUT", config)
        info$buttons <- grepl("\tBUTTONS", config)
        info$left <- grepl("\tLEFT", config)
        info$right <- grepl("\tRIGHT", config)
        info$mono <- !(info$right & info$left)
    }

    info
}


# Extracts the value of a given field from the ASC header
from_header <- function(header, field) {
    s <- header[grepl(paste0("\\*\\* ", field), header)]
    ifelse(length(s) > 0, gsub(paste0("\\*\\* ", field, ": (.*)"), "\\1", s), NA)
}


# Get the display resolution of the stimulus computer from the file
get_resolution <- function(nonsample) {

    res <- c(NA, NA)
    for (pattern in c("DISPLAY_COORDS", "GAZE_COORDS", "RESOLUTION")) {
        display_xy <- nonsample[str_detect(nonsample, fixed(pattern))]
        if (length(display_xy) == 0) next
        display_xy <- gsub(paste0(".* ", pattern, "\\D+(.*)"), "\\1", display_xy[1])
        display_xy <- as.numeric(unlist(strsplit(display_xy, split = "\\s+")))
        res <- c(display_xy[3] - display_xy[1] + 1, display_xy[4] - display_xy[2] + 1)
        break
    }

    res
}


# Gets the model and software version of the tracker based on the header contents
get_model <- function(header) {

    version_str <- from_header(header, "VERSION")
    version_str2 <- header[grepl("\\*\\* EYELINK II", header)]

    if (is.na(version_str)) {
        model <- "Unknown"
        ver_num <- "Unknown"
    } else if (version_str != "EYELINK II 1") {
        model <- "EyeLink I"
        ver_num <- gsub(".* ([0-9]\\.[0-9]+) \\(.*", "\\1", version_str)
    } else {
        ver_num <- gsub(".* v(.*) [[:alpha:]].*", "\\1", version_str2)
        model <- ifelse(as.numeric(ver_num) < 2.4,
            "EyeLink II",
            ifelse(as.numeric(ver_num) < 5,
                "EyeLink 1000",
                ifelse(as.numeric(ver_num) < 6,
                    "EyeLink 1000 Plus",
                    "EyeLink Portable Duo"
                )
            )
        )
    }

    return(c(model, ver_num))
}


# Get info about the camera mount type for desk-mounted EyeLinks
get_mount <- function(mount_str) {

    # Older EyeLink 1000s may be missing "R" in table mount names, we add one if needed
    if (str_detect(mount_str, "TABLE$")) {
        mount_str <- paste0(mount_str, "R")
    }

    mounts <- list(
        "MTABLER" = "Desktop / Monocular / Head Stabilized",
        "BTABLER" = "Desktop / Binocular / Head Stabilized",
        "RTABLER" = "Desktop / Monocular / Remote",
        "RBTABLER" = "Desktop / Binocular / Remote",
        "AMTABLER" = "Arm Mount / Monocular / Head Stabilized",
        "ARTABLER" = "Arm Mount / Monocular / Remote",
        "TOWER" = "Tower Mount / Monocular / Head Stabilized",
        "BTOWER" = "Tower Mount / Binocular / Head Stabilized",
        "MPRIM" = "Primate Mount / Monocular / Head Stabilized",
        "BPRIM" = "Primate Mount / Binocular / Head Stabilized",
        "MLRR" = "Long-Range Mount / Monocular / Head Stabilized",
        "BLRR" = "Long-Range Mount / Binocular / Head Stabilized"
    )
    mount <- ifelse(mount_str %in% names(mounts), mounts[[mount_str]], NA)

    mount
}


# Generate column names for raw sample data based on gathered info
get_raw_header <- function(info) {

    eyev <- c("xp", "yp", "ps")

    if (!info$mono) {
        eyev <- c(paste0(eyev, "l"), paste0(eyev, "r"))
    }
    if (info$velocity) {
        if (info$mono) {
            eyev <- c(eyev, "xv", "yv")
        } else {
            eyev <- c(eyev, "xvl", "yvl", "xvr", "yvr")
        }
    }
    if (info$resolution) {
        eyev <- c(eyev, "xr", "yr")
    }
    if (info$input) {
        eyev <- c(eyev, "input")
    }
    if (info$buttons) {
        eyev <- c(eyev, "buttons")
    }
    if (info$tracking) {
        # If "TRACKING" in header, cr.info is present in samples whether or not CR actually used
        eyev <- c(eyev, "cr.info")
    }
    if (info$htarg) {
        # Three extra columns for remote set-up
        eyev <- c(eyev, "tx", "ty", "td", "remote.info")
    }

    c("time", eyev)
}


get_event_header <- function(info, xy_cols) {

    base <- c("eye", "stime", "etime", "dur")
    # If event data type is HREF, events contain both HREF and GAZE data, so there are extra columns
    if (info$event.dtype == "HREF") {
        xy_cols <- c(paste0("href.", xy_cols), xy_cols)
    }
    if (info$resolution) {
        xy_cols <- c(xy_cols, "xr", "yr")
    }

    c(base, xy_cols)
}


get_sacc_header <- function(info) {
    get_event_header(info, c("sxp", "syp", "exp", "eyp", "ampl", "pv"))
}


get_fix_header <- function(info) {
    get_event_header(info, c("axp", "ayp", "aps"))
}


# Gets column types from vector of column names, defaults to float unless otherwise specified
get_coltypes <- function(colnames, float_time = TRUE) {

    time_cols <- c("time", "stime", "etime", "dur")
    chr_cols <- c("type", "eye", "cr.info", "remote.info")
    int_cols <- c("button", "state", "value")
    if (!float_time) int_cols <- c(int_cols, time_cols)

    coltypes <- ifelse(
        colnames %in% chr_cols, "c",
        ifelse(colnames %in% int_cols, "i", "d")
    )
    coltypes <- paste0(coltypes, collapse = "")

    coltypes
}


get_htarg_regex <- function(binocular) {

    # HTARG info column consists of '.'s and/or letters (indicating problems)
    htarg_errs <- ifelse(binocular, "MANCFTBLRTBLRTBLR", "MANCFTBLRTBLR")
    htarg_errs <- strsplit(htarg_errs, split = "")[[1]]
    htarg_regex <- paste0("(", htarg_errs, "|\\.)", collapse = "")

    htarg_regex
}


is_float <- function(str) {
    str_detect(str, "\\.")
}
