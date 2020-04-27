#' @title gets left bit of string
#' @description just like excel's left func
#' @param text stuff to parse
#' @param num_char how many characters should be returned
#' @return the `num_char` length of chars on the left of the inputed string
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname left
#' @export

left <- function(text, num_char) {
  substr(text, 1, num_char)
}

#' @title gets mid part of string
#' @description just like excel's mid func
#' @param text stuff to parse
#' @param start_num PARAM_DESCRIPTION
#' @param num_char how many characters should be returned
#' @return the `num_char` length of chars in the middle of the inputed string
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mid
#' @export

mid <- function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

#' @title gets right side of string
#' @description just like excel's right func
#' @param text stuff to parse
#' @param num_char how many characters should be returned
#' @return the `num_char` length of chars on the right of the inputed string
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname right
#' @export

right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# Git finder --------------------------------------------------------------

#' @title figure out which folders have git initialized or not
#' @description FUNCTION_DESCRIPTION
#' @param paff what folder contains all the folders that you want to confirm git-ness, Default: '.'
#' @return returns a dataframe of all the folders that DO have git initialized.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1 %>%
#'  }
#' }
#' @rdname gitLister
#' @export
#' @importFrom purrr set_names map
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' @importFrom dplyr filter select mutate left_join %>%
gitLister <- function(paff = "."){
  folders <- list.dirs(path = paff, full.names = TRUE, recursive = FALSE)
  outStuff <- folders %>% purrr::set_names() %>% purrr::map(list.dirs) %>%
    tibble::enframe() %>% tidyr::unnest(value) %>%
    dplyr::filter(grepl(".git", value)) %>% dplyr::select(name) %>% unique %>%
    dplyr::mutate(git = TRUE)
  outStuff <- dplyr::left_join(data.frame(name = folders, stringsAsFactors = FALSE),
                               outStuff, by = "name") %>%
    dplyr::mutate(name = gsub("\\./", "", name))


  tuffreturn(outStuff)
}

#' @title examines all the R files within a folder that have been modified within a certain date
#' and returns a list of all the libraries to install.
#' @description It'll check for all files that start with `.R`, so `.Rmd` etc are included.
#' @param parentFolder what folder contains all the repos that contain the files to check
#' @param howLong specifies which files should be checked, by filtering to files only within
#' `howLong` days ago, Default: 365
#' @param whatLib what folder would you like to install these libraries into? It's wise
#' to install these in a new folder if you are upgrading to a new version of R, Default: .libPaths()[1]
#' @return It spits out to console a command that you can copypaste to install all the CRAN libraries,
#' as well as listing all the non-CRAN libraries it found.
#' @details For now the script is super naive, the next thing it can do is identify the package
#' dependencies and do a bit of a smarter install order.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname libraryFinder
#' @export
#' @importFrom tibble rownames_to_column tibble
#' @importFrom dplyr filter pull %>%
#' @importFrom purrr map discard map_lgl
#' @importFrom stringr str_split str_extract str_squish
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
libraryFinder <- function(parentFolder, howLong = 365, whatLib =.libPaths()[1]){
  ## Get all files
  files <- list.files(parentFolder, pattern = ".+\\.R",
                      recursive = TRUE, full.names = TRUE)
  ## now get the properties of all files, and see which have been recently modified.
  fileProps <- file.info(files) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(mtime >= as.POSIXct(Sys.Date() - howLong)) %>%
    dplyr::pull(rowname)

  ## now read these files and keep all the library or `::` thingies
  allContents <- fileProps %>% purrr::map(readLines) %>% unlist

  libString <- "library\\(.+|require\\(.+|\\@import .+ |\\@importFrom [a-zA-Z0-9_\\.-]+ | .+\\:\\:"

  sumContents <-
    allContents %>%
    ## sometimes packages are hidden after a pipe on the same line... so let's split:
    stringr::str_split("%>%") %>% unlist %>%
    ## could get fancier with lookaround assertions, but this is a good debugging step
    stringr::str_extract(., libString) %>%
    purrr::discard(is.na(.)) %>%
    stringr::str_squish() %>% unique %>%
    sort

  ## clean up strings... seems to be rightmost items, or separated by parenthesis:
  libs <- sumContents %>%
    gsub(".+ |.+\\(|~|:+|\\)|\\[|#", "", .) %>% unique

  ## get CRAN packages
  cranLibs <- xml2::read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html") %>%
    rvest::html_nodes(., "td a") %>% rvest::html_text(.)

  returnDF <- tibble::tibble(library = libs,
                             CRAN = purrr::map_lgl(libs, ~ . %in% cranLibs))

  ## CRAN packages:
  CRANstuff <- returnDF %>% dplyr::filter(CRAN) %>% dplyr::pull(library) %>%
    paste(., collapse = '","')
  cat(paste0('___________________________________________\n
             INSTALL THESE CRAN PACKAGES: \n\n
             install.packages(c("', CRANstuff,  '"), lib = "', whatLib, '")'))

  cat("\n___________________________________________\nNOT ON CRAN \n\n", returnDF %>%
        dplyr::filter(!CRAN) %>% dplyr::pull(library) %>% sort)
}
