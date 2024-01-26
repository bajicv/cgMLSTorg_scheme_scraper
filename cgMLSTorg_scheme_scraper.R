################################################################################
# cgMLST.org webpage scraper

# Author: Vladimir Bajić
# Date: January 2024

# Description:
# This script allows
#   - listing all of the available schemes on cgMLST.org
#   - finding the date and time of the last change on scheme of interest
#   - downloading schemes from cgMLST.org


# Usage:

# To list all available schemes on cgMLST.org
#   Rscript --vanilla cgMLSTorg_scheme_scraper.R

# To finding the date and time of the last change on scheme of interest
#   Rscript --vanilla cgMLSTorg_scheme_scraper.R -f last_change -i Abaumannii1469

# To download specific scheme
#   Rscript --vanilla cgMLSTorg_scheme_scraper.R -f download -i Abaumannii1469
#
################################################################################


# Libraries --------------------------------------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(rvest))
library(knitr)
library(optparse)


# Functions --------------------------------------------------------------------

## Function to get list of available schemes from cgMLST.org
get_list_of_schemes <- function() {
    ### Retrieving the cgMLST.org web page
    document <- read_html("https://www.cgmlst.org/ncs/scheme/")

    ### extract table
    table <-
        document %>%
        html_table() %>%
        .[[1]] %>%
        rename_all(function(x) gsub(" ", "_", x))

    ### extract urls to schemes
    urls <-
        document %>%
        html_elements("table") %>%
        html_elements("a") %>%
        html_attr("href")

    ### bind table with URLSs and create URL IDs
    df <-
        table %>%
        mutate(URL = urls) %>%
        mutate(scheme_URL_ID = str_replace(URL, "^https://www.cgmlst.org/ncs/scheme/schema/", "")) %>%
        mutate(scheme_URL_ID = str_replace(scheme_URL_ID, "/$", ""))

    return(df)
}

## Function to print formatted list of available schemes on cgMLST.org
list_schemes <- function() {
    get_list_of_schemes() %>%
        select(scheme_URL_ID, Scheme, Target_Count, CT_Count) %>%
        kable() %>%
        print()
}

## Function to extract scheme_info based on scheme_url_id
scheme_info <- function(scheme_url_id) {
    read_html(paste0("https://www.cgmlst.org/ncs/scheme/scheme/", scheme_url_id, "/")) %>%
        html_table() %>%
        .[[1]] %>%
        mutate(X1 = if_else(X1 == "", NA_character_, X1)) %>%
        fill(X1) %>%
        group_by(X1) %>%
        summarize(X2 = paste(X2, collapse = "; ")) %>%
        rename(key = X1, value = X2)
}

## Function to extract Version and Last Change of specific scheme
last_change_xtract <- function(scheme_url_id) {
    tmp_scheme_info <-
        scheme_info(scheme_url_id) %>%
        filter(key %in% c("Name", "Version", "Last Change")) %>%
        pivot_wider(names_from = key, values_from = value)

    ### Check if the column exists
    if ("Last Change" %in% colnames(tmp_scheme_info)) {
        ### Mutate the column only if it exists (not all webpages have the same info!)
        tmp_scheme_info %>%
            mutate(
                parsed_date = mdy_hm(str_replace(`Last Change`, ",", "")),
                Last_Change = format(parsed_date, "%Y-%m-%d-%H-%M")
            )
    } else {
        return(tmp_scheme_info)
    }
}

## Function to download zip scheme file
scheme_download_cgMLST <- function(scheme_URL_ID) {
    scheme_timestamp_info <- last_change_xtract(scheme_URL_ID)
    scheme_timestamp <- paste0("v", scheme_timestamp_info$Version, "_LastChange_", scheme_timestamp_info$Last_Change)

    destfile_path <- paste0(scheme_URL_ID, "_", scheme_timestamp, ".zip")

    download.file(
        paste0("https://www.cgmlst.org/ncs/scheme/", scheme_URL_ID, "/alleles/"),
        destfile = destfile_path,
        method = "auto"
    )
    ### Extract the zip file to a directory with the same name
    unzip(destfile_path, exdir = file.path(getwd(), tools::file_path_sans_ext(basename(destfile_path))))
}

## Function to print last changes for given scheme_URL_ID
print_last_change <- function(scheme_URL_ID) {
    last_change_xtract(scheme_URL_ID) %>%
        select(Name, Version, `Last Change`) %>%
        kable() %>%
        print()
}

#-------------------------------------------------------------------------------

# Making option list -----------------------------------------------------------
option_list <- list(
    make_option(c("-f", "--function"), type = "character", help = "Function to be performed. Possible options are: \n\t\t'download': to download scheme specified with -i scheme_URL_ID; and \n\t\t'last_change': to display information about version and/or time of the last change to the scheme.\n", metavar = "character"),
    make_option(c("-i", "--id"), type = "character", help = "scheme URL ID on which to perform function.\n", metavar = "character")
)
# Parsing options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Save list of possible IDs
existing_IDs <- get_list_of_schemes() %>% pull(scheme_URL_ID)

# Check the provided option and execute the corresponding code -----------------
if (is.null(opt$f) & is.null(opt$i)) {
    cat("Current date and time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Printing available cgMLST.org schemes with their IDs:\n")
    list_schemes()
} else if (is.null(opt$f) & !is.null(opt$i)) {
    cat("Please provide function you want to perform using argument -f.\n")
} else if (!is.null(opt$f) & is.null(opt$i)) {
    cat("Please provide scheme_URL_ID using argument -i\n")
} else if (!is.null(opt$f) & !is.null(opt$i)) {
    if (!(opt$i %in% existing_IDs)) {
        cat("Invalid entry for -i. Please specify one of the existing scheme_URL_ID.\n")
        cat("Available schemes and their scheme_URL_ID are:\n")
        list_schemes()
    } else if (opt$f == "last_change") {
        print_last_change(opt$i)
    } else if (opt$f == "download") {
        scheme_download_cgMLST(opt$i)
    } else if (!(opt$f %in% c("last_change", "download"))) {
        cat('Invalid entry for -f. Please specify "-f last_change", or "-f download".\n')
    }
}
