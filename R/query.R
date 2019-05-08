#' Query drugname using Daily Med API
#' @param query string to query
#' @param by search by drugnames or drug classes
#' @param ... other parameters
#' @import httr
#' @import rjson
#' @export
query_daily_med <- function(query, by="drugname", ...) {
  res <- list()
  if(by == "drugname") {
    r <- GET(paste0("https://dailymed.nlm.nih.gov/dailymed/services/v2/spls.json?drug_name=", query))
    if (status_code(r) == 200) {
      res <- rjson::fromJSON(content(r, "text"))[["data"]]
    }
    else {
      message(" did not find any drugs ")
    }
  }
  else if(by == "drugclass") {
    message("not yet implemented")
  }

  res
}

#' for a given record from daily med search, return drug properties
#' Note: Use query_daily_med first!
#' @param record drug record to search
#' @param namespaces xml namespace to use for parsing
#' @import httr
#' @import rjson
#' @import XML
#' @export
#'
get_drug_record <- function(record, namespaces = c(n="urn:hl7-org:v3")) {
 if ("setid" %in% names(record)) {
   r <- content(GET(paste0("https://dailymed.nlm.nih.gov/dailymed/services/v2/spls/", record[["setid"]], ".xml")), "text")
   data <- xmlParse(r)
   # components <- getNodeSet(data, "//n:component", namespaces = namespaces)
   # usage_sections <- getNodeSet(data, "//n:section[@ID='S1']", namespaces = namespaces)
   code <- getNodeSet(data, "//n:code[@code='34067-9']", namespaces = namespaces)
   usage <- xmlParent(code[[1]])
   return(xmlToDataFrame(usage))
 }
  else {
    message("no setid in record!")
  }
}
