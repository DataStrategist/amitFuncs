#' @title gets left bit of string
#' @description just like excel's left func
#' @param text stuff to parse
#' @param num_char how many characters should be returned
#' @return the `num_char` length of chars on the left of the inputed string
#' @details DETAILS
#' @tests testthat::expect_equal(left("ABC", 2), "AB")
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  left("ABC", 2)
#'  }
#' }
#' @rdname left
#' @export

# text stuff --------------------------------------------------------------


left <- function(text, num_char) {
  substr(text, 1, num_char)
}

#' @title gets mid part of string
#' @description just like excel's mid func
#' @param text stuff to parse
#' @param start_num PARAM_DESCRIPTION
#' @param num_char how many characters should be returned
#' @tests testthat::expect_equal(mid("ABCDE", 2, 1), "B")
#' @return the `num_char` length of chars in the middle of the inputed string
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  mid("ABCDE", 2, 1)
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
#' @tests testthat::expect_equal(right("ABC", 2), "BC")
#' @return the `num_char` length of chars on the right of the inputed string
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  right("ABC", 2)
#'  }
#' }
#' @rdname right
#' @export

right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

#' @title Given a text with titles, it returns a df with each line with it's accompanying title
#' @description This is suitable for when you have a long text and you're trying to do some NLP on it.
#' Some examples might be: trying to digest documentation, analyzing documentation, converting to a table
#' format something that should have never been entered into word, etc.
#' @param text the character string containing everything you're looking for.
#' @param titleChar what unique character denotes a title?
#' @return returns a data frame with the erstwhile titles pushed into each row
#' @details DETAILS
#' @examples
#'  text <- c("## First","one","however","two","","## Second","otheer"," - Happy","Thoughts")
#'  titleFlattener(text, "##")
#' @test
#' text <- c("## First","one","however","two","","## Second","otheer"," - Happy","Thoughts")
#' a <- titleFlattener(text, "##")
#' expect_equal(a[1,1], "## First")
#' @rdname titleFlattener
#' @export
#' @importFrom dplyr mutate filter %>%
#' @importFrom tidyr fill
titleFlattener <- function(text, titleChar){
  df <- data.frame(stuff = text, stringsAsFactors = FALSE)
  df <- df %>% dplyr::mutate(title = ifelse(grepl(titleChar, text), stuff, NA)) %>%
    tidyr::fill(title) %>%
    dplyr::filter(stuff != title) %>%
    select(title, stuff)
  return(df)
}

#' @title given a df with two columns, stuff and section headers,
#' it outputs the stuff where with section headers are "titles"
#' @description basically this is the opposite of titleFlattener
#' @param textdf the df with the text and the section headers
#' @param spaceAbove should there be a line before the sections, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  text <- c("## First","one","however","two","","## Second","otheer"," - Happy","Thoughts")
#'  titleUnflattener(titleFlattener(text, "##"))
#'  }
#' }
#' @rdname titleUnflattener
#' @export
#' @importFrom dplyr %>%
titleUnflattener <- function(textdf, spaceAbove = TRUE){
  ## maybe add title space before title
  if(spaceAbove) textdf$title <- paste("\n",textdf$title)

  ## Now parse it out
  res <- split(textdf$stuff, textdf$title)
  unlist(mapply(c, names(res), res), use.names = FALSE) %>% cat(sep = "\n")
}

# package stuff --------------------------------------------------------------

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

#' @title Identify which folders are git repos, clean them up and organize them. READ WARNING IN DESCRIPTION
#' @description This function will output a dataframe that will state whether folders
#' within a specified folder are git repos, and their status. By status I mean,
#' what's the currently checked-out branch, whether
#' there are any changes noticed between the local repo and the remote.
#' CAREFUL!!!!!!!!!!!!!!!!!!!!!!!!!!
#' This command will also run gc, delete local merged branches and prune your remote
#' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
#' @rdname gitOrganizer
#' @export
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>% full_join group_by summarize case_when arrange
gitOrganizer <- function(paff = "..", matchingText = "."){
  ## Run garbage collection on all repos:
  repos <- list.files(paff, pattern = matchingText)
  stuff <- 'git gc && git remote prune origin && git branch --merged | egrep -v \"(^\\*|master|dev)\" | xargs git branch -d'
  gc <- repos %>%
    purrr::map(~system(paste0('cd .. && cd "./', ., '" && ', stuff)))

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
  allOfThem <- list.files(paff, pattern = matchingText, full.names = FALSE)

  ## if selected, filter down to master branch.
  if (onlyMaster) {
    if(as.character(Sys.info()[1]) == "Windows"){
      gitstatus <- allOfThem %>% purrr::set_names() %>%
        purrr::map(~shell(paste0('cd ',paff,' && cd "./', ., '" && git status'), intern = TRUE))
    } else {
      gitstatus <- allOfThem %>% purrr::set_names() %>%
        purrr::map(~system(paste0('cd ',paff,' && cd "./', ., '" && git status'), intern = TRUE))
    }

    allowed <- gitstatus %>% purrr::map(~head(., 1)) %>% tibble::enframe() %>%
      tidyr::unnest(value) %>% dplyr::filter(grepl("master", value)) %>%
      dplyr::pull(name)
  } else {
    allowed <- allOfThem
  }

  instaaa <- function(x){
    # browser()
    system(paste0('R CMD INSTALL ', paff, '/', x), intern = TRUE)
    print(paste0(x, " done!"))
  }

  Installer <- allowed %>% set_names %>%
    purrr::map(~safely(instaaa(.)))
  # browser()

  Installer
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param paff PARAM_DESCRIPTION, Default: 'R'
#' @param matchingText PARAM_DESCRIPTION, Default: '.'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @importFrom dplyr %>% discard count
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname functionsFinder
#' @export

functionsFinder <- function(paff = "R", matchingText = "."){
  ## read all files in selected folder and matching the specified text
  a <- list.files(paff, matchingText, full.names = TRUE,recursive = TRUE) %>%
    map(read_lines) %>%
    ## but throw away comments
    unlist %>% discard(~left(., 1) == "#")


  ## Grab everything that has an open parenthesis or a package-call
  output <- a %>% unlist %>%
    str_extract(".+ ?<- ?function|[a-zA-Z0-9\\._\\-:]+(?=\\()") %>%
    discard(~is.na(.)) %>%
    discard(~left(., 1) == "_")

  return(count(tibble(output), output, sort = T))
}

#' @title find functions used in each of the functions in a specified R file
#' @description This function is suitable to identify what exports are required when
#' designing a package. Like a catch all in case sinew fails.
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
#' @rdname functionsUsedFinder
#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr fill
#' @importFrom readr read_lines
functionsUsedFinder <- function(paff = "R", matchingText = "."){
  ## read all files in selected folder and matching the specified text
  a <- list.files(paff, matchingText, full.names = TRUE,recursive = TRUE) %>%
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


#' @title Output a list of steps to do
#' @description There are several steps to complete a package successfully. This function outputs a kind of
#' opinionated short-list or so

#' @return just outputs to console stuff that should be done.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname packageMakerList
#' @export

packageMakerList <- function(){

  txt <-   '-------------------------------------------------------------
Lets create a package!!! These are the cliffnotes,
for a full description, check the blog-post or youtube video (in the help file)
-------------------------------------------------------------------------
available::available("PKGNAME")
 - File >> new project >> new directory >> R package` (enable git)
 - Delete `hello.R` from the `/R` folder
 - Delete `hello.Rd` from the `/man` folder
 - Modify the DESCRIPTION
  person("First", "Last", email = "first.last@example.com",role = c("aut", "cre")))
library(usethis); library(sinew)
 - git commit
use_github(protocol = "https")
use_readme_rmd()
usethis::use_lifecycle_badge(stage = "Experimental")
use_cran_badge()
usethis::use_mit_license("YOUR NAME")
use_pkgdown()
usethis::use_news_md()

 - Create `funcs.R` in the `/R` folder - Copy paste your code inside
    (Try to be explicit `package::function` at least once per used function per your function)
 - createOxygen for each function - commit function to memory, doubleclick title, run addin
sinew::makeImport("R", format = "description") and add to DESCRIPTION
 - In Build menu >> More >> Configure Build tools >> Generate Documentation >> Install and Restart
 - Delete NAMESPACE

Use Rstudio Build and install, or do:
  - devtools::document
  - devtools::build()
  - devtools::install()

goodpractice::gp()

devtools::check()

REMEMBER TO CHECK DOCUMENTATION:
  - Roxygen for each function
    - README.Rmd
    - DESCRIPTION
    - NEWS

pkgdown::build_site()

 - git commit/push
 - set master/docs as the Repos webpage

 - To think about:
usethis::use_code_of_conduct()
use_testthat()
amitFuncs::pleaseForTheLoveOfGodLetMeBuild()
usethis::use_dev_version()
use_vignette()'

  cat(txt)

}

# testing stuff -----------------------------------------------------------
#' @title Create proposals for a testing matrix
#' @description Will go through all the R files in a package,
#' looking for instances of `testFrom` on roxygen2 `param` values in
#' function definitions. Once it finds these, it'll expand a grid of
#' all possibilities. This will provide a shortlist of all options to test
#' the code with, although it will not know if the input combination should
#' fail or pass, and if it does pass it should have a certain value.
#' @return will dump out a character vector of expressions suitable to
#' be paired with their testthat expectations.
#' @details This function is a bit brittle for now. It assumes that the
#' `testFrom` placement will be the last thing on param lines.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname testMatrixMaker
#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr fill unite
#' @importFrom readr read_lines
testMatrixMaker <- function(){
  ## read all files in selected folder
  a <- list.files("./R", full.names = TRUE) %>%
    purrr::map(read_lines) %>% unlist()

  ## keep params and function definition
  mainFrame <- a %>% purrr::keep(~grepl("@param|function\\(", .x)) %>%
    tibble::tibble(param = .) %>%
    dplyr::mutate(funct = ifelse(grepl("function",param),
                                 gsub(" ?<- ?function|\\{", "",param), NA)) %>%
    fill(funct,.direction = "up") %>%
    ##remove functs
    filter(grepl("#", param)) %>%
    split(.$funct) %>%
    map(~pull(., param))

  parsed <- mainFrame %>% map(internalFunctionParser)

  output <- parsed %>%
    map(joiner) %>% enframe %>% unnest(value) %>%
    mutate(name = gsub("(?<=\\().+", "", name, perl = TRUE)) %>%
    mutate(finisher = ")") %>%
    unite(., ., sep = ", ") %>% pull %>%
    gsub(", NA", "", .) %>%
    gsub("\\(, ?", "\\(", .) %>%
    gsub(", ?\\)", "\\)\n", .) %>% noquote()

  text <- "OK, these are all the outputs you requested!\n\n
  All you have to do put your testthat expectation on:\n\n
  Some options for example might be:
  expect_error() or expect_equal()\n\n
  Once that's done, put them in your @tests roxytest block\n\n
  for more information, please see: \n
  https://mikldk.github.io/roxytest/articles/introduction.html\n\n"
  cat(text, output)
}

#' @title internal function for testMatrixMaker
#' @description starts operationalizing the equality of the params
#' @param df dataframe representing each function to be parsed
#' @return gives a df but with variables nicely labelled
#' @details this is probably not necessary, but in the off chance that
#' parameters are listed in roxygen in a different order than they are
#' listed in the function definition itself, it's safest to be super
#' explicit. This also makes mapping here and there easier.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname joiner
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr %>% mutate select

joiner <- function(df){
  df %>%
    rownames_to_column() %>%
    pivot_longer(cols = -rowname, names_to = "nam",
                 values_to = "val") %>%
    mutate(boff = paste(nam, val, sep = " = ")) %>%
    pivot_wider(id_cols = rowname, names_from = nam,
                values_from = boff) %>%
    select(-rowname)
}

#' @title this does most of the heavy lifting of seperating inputs and functions
#' @description FUNCTION_DESCRIPTION
#' @param textString what item should I break down
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname internalFunctionParser
#' @importFrom purrr keep map set_names
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate %>% filter select pull
#' @importFrom stringr str_split
#' @importFrom tidyr unnest fill
internalFunctionParser <- function(textString){
  ## keep params and function definition
  mainFrame <- textString %>% purrr::keep(~grepl("@param|function\\(", .x)) %>%
    tibble::tibble(param = .) %>%
    dplyr::mutate(funct = ifelse(grepl("function",param),
                                 gsub(" ?<- ?function|\\{", "",param), NA)) %>%
    fill(funct,.direction = "up") %>%
    ##remove functs
    filter(grepl("#", param))

  ## extract param and test options
  paramAndOptions <- mainFrame %>% select(param) %>%
    mutate(param= gsub(".+param ", "", param)) %>% pull %>%
    str_split(., " .+testFrom: ?", simplify = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    set_names(c("field", "options")) %>%
    as_tibble

  paramAndOptions$options <- str_split(paramAndOptions$options, ",")

  paramAndOptions %>% unnest(options) %>% split(.$field) %>%
    map(~pull(.)) %>% expand.grid
}


# network stuff -----------------------------------------------------------

#' @title convert an edge list into 2 data frames: nodes and edges
#' @description This function takes a data.frame like with two or three columns:
#' `FROM` and `TO` (and potentially `VALUE`) and outputs a list containing two
#' dataframes: Nodes and Edges. The simplest way to accomplish this task would be to
#' create an edge list using the igraph function `from_edgelist()`, and then
#' subsequently we could use `as_data_frame()` to create each data.frame.
#' This function provides some additional functionality for convenience for the
#' extremely lazy, suitable for subsequent manipulation
#'
#' @param df dataframe with edgeList
#' @param Index Should the IDs of nodes start from 0 or 1. The indexing is important.
#' By default, the edges will be 1-indexed (for VisNetwork), but if you're using
#' network3d, change the indexing to 0, Default: 1
#' @param addLabel should the node names be displayed?
#' @param addHover should the node names be displayed on hover?
#' @param col1 what color should from nodes be?, Default: #A58FAA
#' @param col2 what color should from nodes be?, Default: #A6D6D6
#' @param printResults View node degrees to help w/ trim limits?
#' @return output will be a list containing both dataframes
#' @details nothin
#' @importFrom dplyr pull mutate %>% count bind_rows
#' @examples
#' \dontrun{
#' if(interactive()){
#' g <-
#'  #EXAMPLE1
#'  }
#' }
#' @rdname edgeListToNodesEdges
#' @export

edgeListToNodesEdges <- function(df,addLabel = TRUE, addHover = FALSE,
                                 col1 = "#A58FAA", col2 = "#A6D6D6", Index=1,
                                 printResults = F){
  nodes <- data.frame(name=df[,1:2] %>% unlist %>% as.character() %>% unique())
  nodes[,1] <- as.character(nodes[,1])
  nodes$id <- 1:nrow(nodes)

  ## and match to IDs to make edges
  edges <- data.frame(from= match(dplyr::pull(df[,1]),nodes$name),
                      to=   match(pull(df[,2]),nodes$name),
                      stringsAsFactors = FALSE)
  if (ncol(df) == 3) edges <- edges %>% mutate(value = pull(df,3))

  ## indexing
  if (Index==0){
    edges$from <- as.integer(edges$from - 1)
    edges$to <- as.integer(edges$to - 1)
  }

  if (addLabel) nodes$label <- nodes$name
  if (addHover) nodes$title <- nodes$name

  edges <- edges %>% mutate(arrows = "to")

  ## and add node sizes and colors:
  nodeSizeColors <- bind_rows(
    df %>% select(1) %>% set_names("name") %>%
      count(name, name = "value") %>% mutate(color = col1),
    df %>% select(2) %>% set_names("name") %>%
      count(name, name = "value") %>% mutate(color = col2)
  )

  nodes <- nodes %>% left_join(nodeSizeColors, by = "name")

  if (printResults) {
    par(mfrow=c(1,2))
    nodes$value %>% sort %>% plot(main = "look for 'big' nodes")
    grid()
    nodes$value %>% hist(main="look for 'small' nodes")
  }

  return(list(nodes = nodes, edges = edges))
}

#' @title filter a network according to simple node counts
#' @description This is a dangerous thing to do if you're interested in statistics,
#' but for visualization purposes, it can be useful to make some decisions about what's
#' being removed. For the purposes of this function, the count is the node's degrees.
#' @param object The list that contains nodes and edges, (the output of createNetwork)
#' @param eliminateAbove nodes larger than this number will be omitted, Default: NULL
#' @param eliminateBelow nodes smaller than this number will be omitted, Default: NULL
#' @param printResults View node degrees to help w/ trim limits?
#' @importFrom dplyr %>% filter pull count
#' @return a list with 3 objects, same structure as the input
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname truncateNodes
#' @export

truncateNodes <- function(object, eliminateAbove = NULL, eliminateBelow = NULL,
                          printResults = F){
  # browser()
  ## maybe filter away the most populous ones:

  nodes <- object$nodes
  edges <- object$edges

  if (!is.null(eliminateAbove)) {  ## maybe take off big nodes
    toRemove <- nodes %>% filter(value > eliminateAbove) %>% pull(id)
    nodes <- nodes %>% filter(!id %in% toRemove)
  }

  if (!is.null(eliminateBelow)) {  ## maybe take off little nodes
    toRemoveS <- nodes %>% filter(value < eliminateBelow) %>% pull(id)
    nodes <- nodes %>% filter(!id %in% toRemoveS)
  }

  edges <- edges %>% filter(from %in% nodes$id,to %in% nodes$id)

  ## this in turn obviates some nodes... so remove nodes w/out edges again.
  nodes <- nodes %>% filter(id %in% (c(edges$from, edges$to) %>% unique))

  nodeCounts <- tibble(id = c(edges$from, edges$to))
  nodeCounts <- nodeCounts %>% count(id,name = "value")

  if (printResults) {
    print(nodeCounts %>% arrange(desc(value)) %>% head(10) %>%
            left_join(nodes %>% select(id, name)))

    par(mfrow=c(1,2))
    nodeCounts$value %>% sort %>% plot(main = "look for high n")
    grid()
    nodeCounts$value %>% hist(main="look for ones")
  }

  return(list(nodes = nodes, edges = edges, nodeCounts = nodeCounts))
}


## Network plotter
#' @title Create a prettier igraph chart
#' @description FUNCTION_DESCRIPTION
#' @param g graph from igraph
#' @param size What should govern node sizes ('D' for Degree, , 'B' for Betweenness,
#' 'A' for Authority, 'H' for Hub, 'C' for Eigenvector Centrality), Default: 'D'
#' @param color What should govern the node colors? Same options as size (D,B,A,H),
#' Default: NULL
#' @param layout What layout should be used ('FR' for Fruchterman Reingold, 'G' for
#' Graphopt, 'K' for Kamada Kawai), Default: 'F'
#' @param nodeScale how much should we exaggerate the scale for node sizes, Default: 2
#' @param as Arrow head size, Default: 0.1
#' @param keepBackbone What percent of edges should be shown? We are using the
#' `layout_as_backbone` function from `graphlayouts` to trim less meaningful edges
#' for aesthetic purposes, Default = 0.5
#' @importFrom graphlayouts layout_as_backbone
#' @importFrom igraph hub_score layout.fruchterman.reingold layout.graphopt
#' layout.kamada.kawai authority.score degree betweenness
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'
#'  }
#' }
#' @rdname igraphPlot
#' @export

igraphPlot <- function(g, size = "D", color = NULL, layout = "FR",
                       nodeScale = 2, as = 0.1, keepBackbone = 0.5){
  g <- simplify(g)
  #V(g)$grp <- as.character(rep(1:9,each=20))
  bb <- graphlayouts::layout_as_backbone(g,keep=keepBackbone)
  g <- subgraph.edges(g, eids = bb$backbone, delete.vertices = F)

  if (size == "H") vSize <- nodeScale * hub_score(g)$vector
  if (size == "A") vSize <- nodeScale * authority.score(g)$vector
  if (size == "D") vSize <- nodeScale * degree(g)
  if (size == "B") vSize <- nodeScale * igraph::betweenness(g)$vector

  if (layout == "FR") lay <- layout.fruchterman.reingold
  if (layout == "G") lay <- layout.graphopt
  if (layout == "K") lay <- layout.kamada.kawai

  plot(g,
       main = 'TITLE',
       vertex.color = rainbow(52),
       vertex.size = vSize,
       edge.arrow.size = as,
       layout=lay
  )
}

# Misc Helpers ------------------------------------------------------------

#' @title A helper function to remember all the steps I like when mapping a
#'   throwaway function on somethin
#' @description I always forget to set_names, to run safely, etc... so this is
#'   just to help save time. Assumes the tidyverse is loaded
#' @param objectName what is the name of the list IN QUOTES that you're going to
#'   be mapping? Eg "iris" not iris
#' @param parallel if your list contains many entries, set this to TRUE in order
#'   to use multiple cores. If setting to TRUE gives an error, install the
#'   package furrr
#' @return returns nothing, but will output some text to the console suitable to
#'   be copied into your script
#' @examples
#' \dontrun{
#' if(interactive()){
#'  goodstuff("iris")
#'  }
#' }
#' @rdname mapHelper
#' @export

mapHelper <- function(objectName, parallel = FALSE){

    if (!parallel){
      mapCommand <- paste0('mapResult <- ', objectName, ' %>% set_names %>% map(~safely(throwawayFunction1)(.))')
    } else {
      mapCommand <- paste0('mapResult <- ', objectName, ' %>% set_names %>% furrr::future_map(~safely(throwawayFunction1)(.))')
    }
  cat(paste0('
  ########### PASTE ME INTO YOUR SCRIPT AND WORK ON ME!! ##########

### map helper
## Example: pick one entry of your list and show what will happen
## Ex. ', objectName, '[[5]] %>% mutate(inArea = D_Name %in% D_Interested)


## write the throwaway function that will be mapped to each entry of ', objectName, '
throwawayFunction1 <- function(item){
  item %>%
  ## put your code below:
  ## Ex. mutate(inArea = D_Name %in% D_Interested)


}

## test function:
throwawayFunction1(', objectName, '[[5]])

## slow:
', objectName, ' %>% set_names %>% head(3) %>% map(~throwawayFunction1(.))

## apply the function to the list
', mapCommand, '

## check for errors
mapResult %>% map("error") %>% map(as.character) %>% enframe %>% unnest(value)

## keep results (rename goodStuff to whatever your desired outputname should be)
goodStuff <- mapResult %>% map("result")'))
}


# data --------------------------------------------------------------------

#' Some animal names
#'
#' Just a random list that is used w/ the toy dataset creator
#'
#' @format A character vector with 277 items
#' @source <don't remember>
"animalList"


# other stuff -------------------------------------------------------------



# Make a new function
# 1) write backbone including inputs
# 2) write Example/unit test covering main functionality and expectation
# 3) fill in function
# 4) (if first test, run usethis::usetestthat()
# 5) Copy Example into unit test file
