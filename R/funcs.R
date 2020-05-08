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
    dplyr::mutate(name = gsub("\\.\\./", "", name))


  return(outStuff)
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
#' @importFrom purrr map discard map_lgl map_chr
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
        dplyr::filter(!CRAN) %>% dplyr::pull(library) %>% sort, "\n\n")
}


#' @title remove intermediate build artifacts that make package development annoying
#' @description deletes the past builds by deleting every `tar.gz` file and `Rcheck`
#' folders in the specified paths. Also will go through every library folder and
#' delete all `LOCK` folders. Use this function when you are building a package but it
#' seems your new modifications aren't being implemented.
#' @param paff what path should we clean? It assumes you're inside a project, therefore
#' the default value, Default: '..'
#' @return Nothing is returned, but the files/folders are deleted.
#' @details CARREFUL!! You're deleting stuff. Please check to make sure this function
#' does what you think it does.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#' @rdname pleaseForTheLoveOfGodLetMeBuild
#' @export
#' @importFrom purrr map
#' @importFrom dplyr %>%
pleaseForTheLoveOfGodLetMeBuild <- function(paff = ".."){
  ## Remove Rcheck and *.tar.gz
  list.files(paff, pattern = "Rcheck", full.names = TRUE) %>% purrr::map(~unlink(., recursive = TRUE))
  list.files(paff, pattern = "tar.gz", full.names = TRUE) %>% purrr::map(~file.remove(.))

  ## Remove those 00LOCK jerk folders
  .libPaths() %>% purrr::map(~list.files(path = ., pattern = "00LOCK", full.names = TRUE)) %>% unlist %>%
    purrr::map(~unlink(., recursive = TRUE))
}

#' @title Identify which folders are git repos, runs `gc` on  git folders, and analyzes.
#' @description This function will output a dataframe that will state whether folders
#' within a specified folder are git repos, and their status. By status I mean,
#' what's the currently checked-out branch, whether
#' there are any changes noticed between the local repo and the remote.
#' @param matchingText PARAM_DESCRIPTION, Default: '.'
#' @param paff path to the file, Default: '..'
#' @return will return a dataframe with the following fields: name,gitRepo (T/F),
#' branch(currently checked out branch), status (ok, behind or ahead of the remote),
#' toCommit (local changes).
#'
#' Also, the console will print out folders that are NOT git repos, folders that ARE git
#' repos but are NOT on the master branch, and git repos that are in general not synced
#' with the remote.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}}
#' @rdname gitOrganizer
#' @export
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>% full_join group_by summarize case_when arrange
gitOrganizer <- function(paff = "..", matchingText = "."){
  ## Run garbage collection on all repos:
  repos <- list.files(paff, pattern = matchingText)
  gc <- repos %>%
    purrr::map(~system(paste0('cd .. && cd "./', ., '" && git gc')))

  ## Which repos have never been added to github:
  gitOrNot <- tibble::tibble(name = repos, gitRepo = gc %>% as.character) %>%
    dplyr::mutate(gitRepo = ifelse(gitRepo == 0, TRUE, FALSE))

  ## Gather the status of each of these github repos
  gitstatus <- repos %>%
    map(~system(paste0('cd .. && cd "./', ., '" && git status'), intern = TRUE))

  ## clean up output
  gitstatus <- tibble(name = repos, result = gitstatus) %>% unnest(result) %>%
    filter(!grepl("fatal", result)) %>%
    mutate(branch = ifelse(grepl("On branch", result), gsub(".+branch ", "", result), NA)) %>%
    mutate(status = ifelse(grepl("Your branch", result), gsub(".+branch ", "", result), NA)) %>%
    mutate(toCommit = ifelse(grepl("commit|track", result), gsub(".+branch ", "", result), NA)) %>%
    filter(!grepl("\\(use", toCommit)) %>% select(-result) %>%
    pivot_longer(cols = branch:toCommit, names_to = "b", values_to = "value") %>%
    filter(!is.na(value)) %>% group_by(name, b) %>%
    summarize(value = paste(value, collapse= ";")) %>%
    mutate(value = gsub(":", "", value)) %>%
    pivot_wider(names_from = b, values_from = value) %>%
    mutate(status = case_when(
      grepl("up to date", status) ~ "OK",
      grepl("behind", status) ~ "behind",
      grepl("ahead", status) ~ "ahead",
      TRUE ~ status
    )) %>%
    mutate(toCommit = case_when(
      toCommit == "nothing to commit, working tree clean" ~ "OK",
      grepl("Changes not staged for commit", toCommit) ~ "Unstaged",
      grepl("	is behind", toCommit) ~ NA_character_,

      TRUE ~ toCommit
    ))

  ## merge w/ gitstatus
  finalDF <- full_join(gitOrNot, gitstatus, by = "name") %>% arrange(name)

  ## cat out some quick analysis:
  finalDF %>% filter(!gitRepo) %>% pull(name) %>% paste(collapse = '\n\t\t') %>%
    cat("\n\nThese folders are NOT git repos: ", "\n\t\t",.)
  finalDF %>% filter(branch != "master") %>% pull(name) %>% paste(collapse = '\n\t\t') %>%
    cat("\n\nThese repos are NOT on master: ", "\n\t\t",.)
  finalDF %>% filter(status != "OK" | toCommit != "OK") %>% pull(name) %>% paste(collapse = '\n\t\t') %>%
    cat("\n\nThese repos are not synced w/ remote: ", "\n\t\t",., "\n\n")
  return(finalDF)
}

#' @title Build all packages within a folder that match a text string
#' @description When multiple people work on many repos, it it easy to see
#' if you are git pulled/pushed up, but it can be difficult or tedious to
#' see if you have built the latest version of each package. Therefore this
#' function helps install all repos that match a certain text-pattern.
#' @param paff path to the folder containing all the packages to build
#' @param matchingText text string that will be used to determine what packages to build
#' @param onlyMaster should we only build repos that are on MASTER branch, Default: TRUE
#' @return returns an output of whether the installs were completed successfully or not.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname packageBulkInstaller
#' @export
#' @importFrom purrr set_names map safely
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' @importFrom dplyr filter pull
packageBulkInstaller <- function(paff, matchingText, onlyMaster = TRUE){
  ## install packages (but make sure from above you're on master for all of them.)
  toInstall <- list.files(paff, pattern = matchingText, full.names = FALSE)

  ## if selected, filter down to master branch.
  if (onlyMaster) {
    gitstatus <- toInstall %>% purrr::set_names() %>%
      purrr::map(~system(paste0('cd ',paff,' && cd "./', ., '" && git status'), intern = TRUE))

    allowed <- gitstatus %>% purrr::map(~head(., 1)) %>% tibble::enframe() %>%
    tidyr::unnest(value) %>% dplyr::filter(grepl("master", value)) %>%
      dplyr::pull(name)

    toInstall <- allowed
  }

  Installer <- toInstall %>% set_names %>%
    purrr::map(~purrr::safely(system)(paste0('R CMD INSTALL ', paff, '/', .), intern = TRUE))
  # browser()

  Installer %>% map("result") %>% enframe %>% unnest(value)
}


#' @title find functions used in each of the functions in a speficied R file
#' @description This function is suitable to identify what exports are required when
#' designing a package
#' @param paff path to the file, Default: 'R'
#' @param matchingText PARAM_DESCRIPTION, Default: '.'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[tidyr]{fill}}
#' @rdname functionsUsedFinder
#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr fill
#' @importFrom readr read_lines
functionsUsedFinder <- function(paff = "R", matchingText = "."){
  ## read all files in selected folder and matching the specified text
  a <- list.files(paff, matchingText, full.names = TRUE) %>%
    map(read_lines) %>%
    ## but throw away comments
    unlist %>% discard(~left(., 1) == "#")


  ## Grab everything that has an open parenthesis or a package-call
  output <- a %>% unlist %>%
    str_extract(".+ ?<- ?function|[a-zA-Z0-9\\._\\-:]+(?=\\()") %>%
    discard(~is.na(.)) %>%
    discard(~left(., 1) == "_")

  ## and prepare to show what functions are used in what functions
  output <- output %>%
    tibble::tibble(
      mainFunc = ifelse(grepl("<-", output),
                                       gsub(" <-.", "", output),
                                       NA),
      usedFuncs = .) %>%
    tidyr::fill(mainFunc) %>%
    mutate(b=grepl("<-", usedFuncs)) %>% filter(!b) %>% select(-b) %>%
    unique()

  return(output)
}




# network -----------------------------------------------------------------


#' @title convert an edge list into 2 data frames: nodes and edges
#' @description This function takes a data.frame like with two or three columns:
#' `FROM` and `TO` (and potentially `VALUE`) and outputs a list containing two
#' dataframes: Nodes and Edges. The simplest way to accomplish this task would be to
#' create an edge list using the igraph function `igraph::from_edgelist()`, and then
#' subsequently we could use `igraph::as_data_frame()` to create each data.frame.
#' This function provides some additional functionality for convenience for the
#' extremely lazy, suitable for subsequent manipulation
#'
#' @param df dataframe with edgeList
#' @param Index Should the IDs of nodes start from 0 or 1. The indexing is important.
#' By default, the edges will be 1-indexed (for VisNetwork), but if you're using
#' network3d, change the indexing to 0, Default: 1
#' @return output will be a list containing both dataframes
#' @details nothin
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname edgeListToNodesEdges
#' @export

edgeListToNodesEdges <- function(df,Index=1){
  nodes <- data.frame(name=df[,1:2] %>% unlist %>% as.character() %>% unique())
  nodes[,1] <- as.character(nodes[,1])

  ## and match to IDs to make edges
  edges <- data.frame(from= match(df[,1],nodes$name),
                      to=   match(df[,2],nodes$name),
                      value=df[,3])

  ## indexing
  if (Index==0){
    edges$from <- as.integer(edges$from - 1)
    edges$to <- as.integer(edges$to - 1)
  }
  list(nodes=nodes,edges=edges)
}
