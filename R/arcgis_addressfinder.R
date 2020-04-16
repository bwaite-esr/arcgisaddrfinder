#' Call Arcgis API "geocodeAddresses" operation for bulk address geocoding.
#'
#' @param address_table A table with addresses to be geocoded. Columns used in geocoding include required:"address" optional:"address2" "address3" "city" "postal" "countryCode"
#' @param url An URL with which to generate your Arcgis access token "https://www.arcgis.com/sharing/oauth2/token?client_id=<YOUR CLIENT ID>&grant_type=client_credentials&client_secret=<YOUR CLIENT SECRET>&f=pjson" see https://developers.arcgis.com/rest/geocode/api-reference/geocoding-authenticate-a-request.htm
#' @return A table of Geocoded results of matched addresses merged onto your origional columns.
addressFinder <- function(address_table, url) {
  address_table$OBJECTID <- as.numeric(row.names(address_table))
  address_table <-
    address_table[, c("OBJECTID", names(address_table)[-which(names(address_table) == "OBJECTID")])]
  address_table_cols <-
    address_table[, names(address_table)[which(
      names(address_table) %in% c(
        "address",
        "address2",
        "address3",
        "city",
        "postal",
        "countryCode",
        "OBJECTID"
      )
    )]]
  address_table_list <-
    list(records = lapply(
      1:nrow(address_table_cols),
      FUN = function(x) {
        list(attributes = as.list(address_table_cols[x, ]))
      }
    ))
  address_table_json <-
    jsonlite::toJSON(
      address_table_list,
      pretty = TRUE,
      dataframe = "rows",
      auto_unbox = TRUE
    )

  token <-
    postForm(url) # Get limited time access token
  if (is.raw(token)) {
    token <- rawToChar(token)
  }
  token <- jsonlite::fromJSON(token)$access_token
  res <-
    RCurl::postForm(
      uri = "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/geocodeAddresses?",
      addresses = address_table_json,
      token = token,
      f = "json"
    )
  if (is.raw(res)) {
    res <- rawToChar(res)
  }
  results <-
    jsonlite::fromJSON(res, simplifyDataFrame = TRUE)$locations$attributes
  return(merge(results, address_table, by.x = "ResultID", by.y = "OBJECTID"))
}
