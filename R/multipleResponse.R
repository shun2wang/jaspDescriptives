# dataset <- read.csv("./tests/mr.csv")
#
# multipleResponse(jaspResults, dataset, options)

multipleResponse <- function(jaspResults, dataset, options){

  ready <- length(options$multipleResponseVariables) > 0 #multiple response right?

  if (ready) {
    dataset <- .multipleResponseData(dataset, options)

    # .dataCheckErrors(dataset, options)
  }

  if (is.null(jaspResults[["multipleResponseTable"]])){
    .multipleResponseMain(jaspResults, dataset, options, ready)
  }

  # multipleResponseTable <- createJaspTable(title = "Multiple Response Table")
  # jaspResults[["multipleResponseTable"]] <- multipleResponseTable


  return()
}


.multipleResponseData <- function(dataset, options){

  if (!is.null(dataset))
    return(dataset)

  dataset <- .readDataSetToEnd(columns.as.factor = options$multipleResponseVariables)

  return(dataset)
}


.multipleResponseMain <- function(jaspResults, dataset, options, ready) {

  multipleResponseTable <- createJaspTable(title = "Multiple Response Table")

  multipleResponseTable$dependOn(c("multipleResponseVariables", "responseValue",
                           "assignGroupVariables","validOfResponse",
                           "missingOfResponse","percentOfResponses",
                           "percentOfCases", "total"))

  multipleResponseTable$addColumnInfo(name = "assignGroupVariables",   title = "Group",   type = "string", combine = TRUE)
  multipleResponseTable$addColumnInfo(name = "validOfResponse",        title = "Valid",     type = "integer")
  multipleResponseTable$addColumnInfo(name = "missingOfResponse",      title = "Missing",     type = "integer")
  multipleResponseTable$addColumnInfo(name = "percentOfResponses",     title = "Percent Of Responses", type = "number")
  multipleResponseTable$addColumnInfo(name = "total",                  title = "Total",      type = "integer")

  if (options$percentOfCases)
    multipleResponseTable$addColumnInfo(name = "percentOfCases",       title = "Percent Of Cases",          type = "number")

  multipleResponseTable$showSpecifiedColumnsOnly <- TRUE

  multipleResponseTable$addFootnote(
    paste0(gettext("The response value is specified as "), options$responseValue, "."))

  jaspResults[["multipleResponseTable"]] <- multipleResponseTable

  # if (!ready)
  #   return()

  .multipleResponseTableFillMain(multipleResponseTable, dataset, options)

  return()
}

.multipleResponseTableFillMain <- function(multipleResponseTable, dataset, options){

  numOfResponse <- sum(dataset == options$responseValue, na.rm=TRUE)
  responsePerCol <- colSums(dataset == options$responseValue, na.rm=TRUE)

  if( length(options$responseValue) == 1 ) {
    responsePerCol <- numOfResponse
    names(responsePerCol) <- options$responseValue[1]
    numOfCases <- sum(!is.na(dataset))
  } else {
    responsePerCol = colSums(dataset == options$responseValue, na.rm = TRUE)
    numOfCases = sum(!apply(apply(dataset, 1, is.na), 2, all))
  }

  # numOfCases <- sum(!apply(apply(dataset, 1, is.na), 2, all))
  totals <- as.numeric(c(responsePerCol, numOfResponse));

  multipleResponseTable[["validOfResponse"]] <- numOfResponse
  multipleResponseTable[["total"]] <- totals
  multipleResponseTable[["percentOfResponses"]]<- (totals/numOfResponse) * 100
  multipleResponseTable[["percentOfCases"]]<- (totals/numOfCases) * 100

  return()
}
