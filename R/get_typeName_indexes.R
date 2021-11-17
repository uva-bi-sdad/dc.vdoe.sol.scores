get_typeName_indexes <- function(field_list = "metadata_template$datasetVersion$metadataBlocks$geospatial$fields") {
  field_num <- length(get(field_list))
  for (i in 1:field_num) {
    field <- paste0(field_list, "[[", i, "]]$typeName")
    typename <- get(field)
    typename_dt <- data.table::data.table(i, typename)
    if (!exists("typenames")) typenames <- typename_dt
    else typenames <- data.table::rbindlist(list(typenames, typename_dt))
  }
  typenames
}

unname(unlist(lapply(metadata_template$datasetVersion$metadataBlocks$geospatial$fields, function (x) x[c('typeName')])))

metadata_template$datasetVersion$metadataBlocks$geospatial$fields[[1]]['typeName'] == "geographicCoverage"


idx <- which(sapply(metadata_template$datasetVersion$metadataBlocks$geospatial$fields, function(y) "geographicUnit" %in% y$typeName))
metadata_template$datasetVersion$metadataBlocks$geospatial$fields[[idx]]$value <- list("Health District", "County")

yml_metadata$dataset$geospatial$geographicUnit

which(sapply(metadata_template$datasetVersion$metadataBlocks$citation$fields, function(y) "author" %in% y$typeName))

metadata_template$datasetVersion$metadataBlocks$citation$fields[[3]]$value


yml_metadata$dataset$citation$author



mytbl <-
  tibble::tibble(typeName = "geographicCoverage",
                 value = list(list(
                   country = list(typeName = "country",
                                  value = "United States"),
                   state = list(typeName = "state",
                                value = "Virginia")
                 )))

jsonlite::toJSON(mytbl, pretty = T)
#  multiple = c("geographicUnit", value = list("State Health District", "County")