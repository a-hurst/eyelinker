# eyelinker (development version)

 * Set tidyverse-style aliases to be preferred style in manual and vignettes
 * Modernized syntax in 'Getting Started' vignette
 * Fixed compatibility with latest readr (#5, by Jenny Bryan)

# eyelinker 0.2.1

 * Fixed bug reading screen resolution from certain .asc files
 * Added tidyverse-style aliases for `read.asc`, `whichInterval`, and `%In%`
   (`read_asc`, `which_interval`, and `%within`, respectively)
 * Improved documentation for interval utility functions
 * Fixed handling of files containing random rows with too many columns (#1)
 * Added compatibility with readr 2.0.0 (#2, by Jim Hester)


# eyelinker 0.2.0

 * Complete rewrite of `read.asc`, making imports up to 5x faster
 * Added `samples` option to `read.asc`, allowing users to avoid importing slow
   and memory-consuming raw samples if not necessary
 * Added parsing of tracker model/version/configuration, date of recording,
   stimulus display resolution, sample rate, filter level, and more to `$info`
 * Retains non-missing values in sample rows with any missing data (previously
   replaced with `NA`s)
 * Added support for files with malformed `START`/`END` blocks
 * Added optional support for parsing samples/events outside of `START`/`END` blocks
 * Added support for parsing `INPUT` and `BUTTON` events
 * Added support for parsing fixation and saccade events in `HREF` format
 * Improved handling of files with remote target info
 * Added support for parsing files exported with float time (`-ftime`) in `edf2asc`
 * Fixed bugs parsing files with velocity, resolution, input, and/or button info
   in samples
 * `$info` is now a data.frame instead of a list for easier viewing, and so info
   from multiple files can be easily combined into a single frame


# eyelinker 0.1

 * Initial CRAN release
