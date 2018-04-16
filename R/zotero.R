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


