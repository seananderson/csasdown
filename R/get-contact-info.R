#' Get regional CSAS contact information
#'
#' @description
#' Get regional CSAS contact information including email address and mailing
#' address for the last page in the section "This report is available from
#' the." Contact information for the national CSAS office is returned if
#' regional information is not found, with a warning.
#'
#' @param region Region in which the document is published; character vector.
#' (i.e., Pacific). Default is "National Capital Region."
#'
#' @export
#'
#' @return Email address and mailing address as list of character vectors
get_contact_info <- function(region = "National Capital Region") {

  if (fr()) {
    # Get index for region (row)
    ind <- which(region_info$RegionFr == region)
    if(!length(ind)){
      # Maybe the author used English for the region name
      ind <- which(region_info$Region == region)
    }
  } else {
    # Get index for region (row)
    ind <- which(region_info$Region == region)
    if(!length(ind)){
      # Maybe the author used French for the region name
      ind <- which(region_info$RegionFr == region)
    }
  }
  # If region not detected, use national contact info
  if (length(ind) == 0) {
    default_region <- "National Capital Region"
    email <- region_info$Email[region_info$Region == default_region]
    if(fr()){
      address <- region_info$AddressFr[region_info$Region == default_region]
    }else{
      address <- region_info$Address[region_info$Region == default_region]
    }
    alert("Region not detected; using national CSAS contact info")
  } else {
    # Get regional contact info
    email <- region_info$Email[ind]
    if (fr())
      address <- region_info$AddressFr[ind]
    else
      address <- region_info$Address[ind]
  }
  list(email = email, address = address)
}
