#' Build ID String
#'
#' This function will determine if this the request is for a user library
#' or a group library and will return the appropriate code to be used to
#' access the Zotero API. This is an internal function that should not be
#' called directly by the user.
#' @param user The user ID
#' @param group The group ID
#' @return Returns a character variable with the user or group prefix
build_id_string <- function(group = NA) {

  if (!is.na(group)) {
    ret_val <- paste0('groups/', as.character(group))
  } else {
    u <- Sys.getenv('ZOTERO_USER')
    if (u == '')
      stop('Zotero user ID not established as an environmental variable.')

    ret_val <- paste0('users/', u)
  }

  ret_val
}

#' Get Zotero Key
#'
#' This function retrieves the Zotero key or throws an error.
#' @return Character variable containing the Zotero API key.
get_zotero_key <- function() {
  key <- Sys.getenv('ZOTERO_KEY')
  if (key == '')
    stop('Zotero key not established as an environmental variable.')

  key
}

#' List Zotero Collections
#'
#' This function retrieves a list of all collections associated with a user
#' or group.
#' @param group Group ID (default = NA)
#' @return A named vector that with the text-based names of each collection and
#' the key value for that collection that is used to access the API.
#' @export
list_zotero_collections <- function(group = NA) {
  id_string <- build_id_string(group)

  my_url <-sprintf('https://api.zotero.org/%s/collections', id_string)
  request_val <- httr::GET(url = my_url,
                           config = httr::add_headers('Zotero-API-Key' = get_zotero_key()),
                           query = list(v = 3))

  httr::stop_for_status(request_val)
  #request_status <- httr::status_code(request_val)
  #if (request_status != 200)
  #  stop(sprintf('Could not contact Zotero.  HTTP status code:   %i',
  #               request_status))

  my_content <- httr::content(request_val)
  ret_val <- vapply(my_content, function(x) x$data$key, 'a')
  names(ret_val) <- vapply(my_content, function(x) x$data$name, 'a')

  ret_val
}

#' Validate Collections
#'
#' This function takes the collections provided by the user and validates that
#' they exist online. If none of them exist, an error will be thrown.  If some
#' of them do not exist, a warning will be issued.
#' @return A named character vector with the IDs of each collection found in the
#' online database
#' @importFrom magrittr "%>%"
validate_collections <- function(group = NA, collections) {
  if (missing(collections))
    stop('Must supply a vector of collection names to validate collections.')

  # Check if submitted collections are available
  if (length(collections) <= 1 & all(collections == ''))
    return(c(top = ''))

  online_collections <- list_zotero_collections(group)
  is_present <- (collections %in% names(online_collections))

  if (all(!is_present)) {
    stop('Collection(s) submitted to retrieve_zotero not found.')
  } else if (any(!is_present)) {
    # Produce warning if some (but not all) collections are not found
    paste0(collections[!is_present], collapse = ', ') %>%
      sprintf('Collections not found in online database:  %s', .) %>%
      warning(call. = FALSE)
    collections <- collections[is_present]
  }

  online_collections[names(online_collections) %in% collections]
}

verify_status <- function(request_val) {
  my_status <- httr::status_code()
  request_status <- httr::status_code(request_val)
  if (request_status != 200)
    stop(sprintf('Could not contact Zotero.  HTTP status code:   %i',
                 request_status))

}

#' Retrieve Zotero Files
retrieve_zotero <- function(collections = '', group = NA) {
  id_string <- build_id_string(group)

  if (collections != '') {
    # Check if submitted collections are available
    my_collections <- list_zotero_collections(group)
    is_present <- (collections %in% my_collections)

    if (all(!is_present)) {
      stop('Collection(s) submitted to retrieve_zotero not found.')
    } else if (any(!is_present)) {
      # Produce warning if some (but not all) collections are not found
      paste0(my_collections[!is_present], collapse = ', ') %>%
        sprintf('Collections not found:  %s', .) %>%
        warning()
      my_collections <- my_collections[is_present]
    }
  }
}

pull_single_collection <- function(collection_code, group = NA) {
  id_string <- build_id_string(group)

  my_url <-sprintf('https://api.zotero.org/%s/collections/%s/items',
                   id_string,
                   collection_code)
  request_val <- httr::GET(url = my_url,
                           config = httr::add_headers('Zotero-API-Key' = get_zotero_key(),
                                                      format = 'bibtex'),
                           query = list(v = 3,
                                        since = 349))

  # If a valid response is not returned, then throw an error
  request_status <- httr::status_code(request_val)
  if (request_status != 200)
    sprintf('Error retrieving collection %s:  %i',
            names(collection_code),
            request_status) %>%
    stop()

  my_content <- httr::content(request_val)
  if (length(my_content) == 100L) {}

}


