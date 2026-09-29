#
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.
#

# Maintainer: Shun Wang <shuonwang@gmail.com>

MultipleResponseAnalysisInternal <- function(jaspResults, dataset, options) {
  .mrMain(jaspResults, dataset, options)
}

# Null-coalescing helper (avoids a dependency on rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Option keys that every output depends on for the *definition* of the sets
.MR_SET_DEPS <- c("multipleResponseVariables", "assignGroupVariables",
                  "nameResponseGroups", "setCoding", "responseValue",
                  "missingValues")

.mrMain <- function(jaspResults, dataset, options) {

  # Apply the JASP colour palette globally (mirrors descriptives.R convention)
  jaspGraphs::setGraphOption("palette", options[["colorPalette"]])

  dataset <- .mrReadData(dataset, options)

  # Build every user-defined set. Returns an empty list when nothing is ready.
  sets  <- .mrBuildAllSets(dataset, options)
  ready <- length(sets) > 0L

  if (isTRUE(options[["frequencies"]]))
    .mrFrequencyTables(jaspResults, options, sets, ready)

  if (isTRUE(options[["crosstabs"]]))
    .mrCrosstabsOutput(jaspResults, dataset, options, sets, ready)

  if (ready)
    .mrChartsOutput(jaspResults, dataset, options, sets)

  invisible(NULL)
}

# ---------------
# Option helpers
# ---------------

.mrOptionValue <- function(x) {
  if (is.null(x))  return("")
  if (is.list(x))  return(as.character(x[["value"]] %||% ""))
  as.character(x)
}

.mrGroupDefinitions <- function(options) {
  raw <- options[["nameResponseGroups"]]
  if (is.null(raw) || length(raw) == 0L) return(list())

  out <- list()
  for (g in raw) {
    nm <- if (is.list(g)) as.character(g[["name"]] %||% "") else as.character(g)
    if (!nzchar(nm)) next
    lb <- if (is.list(g)) as.character(g[["label"]] %||% "") else ""
    out[[length(out) + 1L]] <- list(name = nm, label = if (nzchar(lb)) lb else nm)
  }
  out
}

.mrAssignments <- function(options) {
  raw <- options[["assignGroupVariables"]]
  if (is.null(raw) || length(raw) == 0L) return(list())

  out <- list()
  for (a in raw) {
    if (!is.list(a)) next
    v <- as.character(a[["variable"]] %||% "")
    g <- as.character(a[["group"]]    %||% "")
    if (!nzchar(v) || !nzchar(g)) next
    out[[length(out) + 1L]] <- list(variable = v, group = g)
  }
  out
}

# ---------------
# Data reading
# ---------------

.mrReadData <- function(dataset, options) {

  groupVarName <- .mrOptionValue(options[["crosstabGroupVar"]])

  allVars <- unique(c(
    unlist(options[["multipleResponseVariables"]]),
    if (nzchar(groupVarName)) groupVarName
  ))
  allVars <- allVars[nzchar(allVars)]

  # Columns are read as factors so that value *labels* survive. This is the key
  # to tolerant matching of the counted value: for a labelled column
  # as.character() yields the label, for a plain numeric column it yields the
  # number as text. Both are handled by .mrIsCounted().
  if (is.null(dataset)) {
    if (length(allVars) == 0L) return(data.frame())
    dataset <- .readDataSetToEnd(columns.as.factor = allVars)
  }

  if (options[["missingValues"]] == "excludeListwise" && length(allVars) > 0L) {
    present <- intersect(allVars, colnames(dataset))
    if (length(present) > 0L) {
      complete <- stats::complete.cases(dataset[, present, drop = FALSE])
      dataset  <- dataset[complete, , drop = FALSE]
    }
  }

  dataset
}

# ══════════════════════════════════════════════════════════════════════════════
# Tolerant matching of the "counted value"
#
# A response counts as selected when the entered text matches EITHER
#   * the value label / character representation of the cell, or
#   * the numeric interpretation of the cell,
# in both cases ignoring surrounding whitespace and letter case.
# This makes the analysis work for 0/1 numeric columns as well as for columns
# carrying value labels such as "Yes" / "No".
# ══════════════════════════════════════════════════════════════════════════════

.mrIsCounted <- function(column, countedValue) {

  n <- length(column)
  if (n == 0L) return(logical(0L))

  target <- trimws(as.character(countedValue))
  if (!nzchar(target)) return(rep(FALSE, n))

  cellsChr <- trimws(as.character(column))

  hit <- !is.na(cellsChr) & (tolower(cellsChr) == tolower(target))

  # Numeric fallback: makes "1" match a cell stored as 1.0, and vice versa.
  targetNum <- suppressWarnings(as.numeric(target))
  if (!is.na(targetNum)) {
    cellsNum <- suppressWarnings(as.numeric(cellsChr))
    hit <- hit | (!is.na(cellsNum) & cellsNum == targetNum)
  }

  hit[is.na(hit)] <- FALSE
  hit
}

# ══════════════════════════════════════════════════════════════════════════════
# Set construction
#
# Every set is a list with:
#   $name        set key (from the naming list)
#   $label       display label (falls back to the key)
#   $type        "dichotomous" | "category"
#   $vars        variables belonging to the set
#   $labels      response category labels
#   $matrix      logical nCases x nCategories presence matrix
#   $nCases      number of analysed cases
#   $nResponses  total number of selected responses
#   $warnings    character vector of non-fatal problems
# ══════════════════════════════════════════════════════════════════════════════

.mrBuildAllSets <- function(dataset, options) {

  groups      <- .mrGroupDefinitions(options)
  assignments <- .mrAssignments(options)

  if (length(groups) == 0L || length(assignments) == 0L) return(list())
  if (is.null(dataset) || ncol(dataset) == 0L)            return(list())

  sets <- list()
  for (g in groups) {
    vars <- vapply(assignments, function(a) {
      if (identical(a$group, g$name)) a$variable else NA_character_
    }, character(1L))
    vars <- vars[!is.na(vars)]
    vars <- intersect(vars, colnames(dataset))   # drop stale assignments
    if (length(vars) == 0L) next

    s <- .mrBuildSet(dataset, options, vars, g$name, g$label)
    if (!is.null(s)) sets[[length(sets) + 1L]] <- s
  }

  sets
}

.mrBuildSet <- function(dataset, options, vars, setName, setLabel) {

  setType  <- options[["setCoding"]]
  warnings <- character(0L)

  subData <- dataset[, vars, drop = FALSE]

  # Pairwise exclusion: keep cases with at least one non-missing value in the set
  if (options[["missingValues"]] == "excludeCasewise") {
    keep    <- rowSums(!is.na(subData)) > 0L
    subData <- subData[keep, , drop = FALSE]
  }

  nCases <- nrow(subData)
  if (nCases == 0L) return(NULL)

  if (identical(setType, "dichotomous")) {

    countedValue <- .mrOptionValue(options[["responseValue"]])

    hits <- lapply(vars, function(v) .mrIsCounted(subData[[v]], countedValue))

    # vapply/sapply collapse to a vector when nCases == 1, so build explicitly
    presenceMat <- matrix(unlist(hits, use.names = FALSE),
                          nrow = nCases, ncol = length(vars))
    colnames(presenceMat) <- vars

    # Non-fatal diagnostics instead of aborting the whole analysis
    neverHit <- vars[colSums(presenceMat) == 0L]
    if (length(neverHit) == length(vars)) {
      observed <- unique(unlist(lapply(vars, function(v)
        as.character(stats::na.omit(subData[[v]])))))
      observed <- sort(unique(observed))
      observed <- observed[seq_len(min(10L, length(observed)))]
      warnings <- c(warnings, gettextf(
        "The counted value '%s' does not occur in any variable of this set. Observed values are: %s. Adjust 'All response values are coded as:' to one of these.",
        countedValue, paste(observed, collapse = ", ")
      ))
    } else if (length(neverHit) > 0L) {
      warnings <- c(warnings, gettextf(
        "The counted value '%s' never occurs in: %s.",
        countedValue, paste(decodeColNames(neverHit), collapse = ", ")
      ))
    }

    list(
      name       = setName,
      label      = setLabel,
      type       = "dichotomous",
      vars       = vars,
      labels     = decodeColNames(vars),
      matrix     = presenceMat,
      nCases     = nCases,
      nResponses = sum(presenceMat),
      warnings   = warnings
    )

  } else {

    # Category coding: every distinct non-missing value is a response category
    allVals <- unique(unlist(lapply(vars, function(v) {
      x <- subData[[v]]
      as.character(x[!is.na(x)])
    }), use.names = FALSE))

    if (length(allVals) == 0L) return(NULL)

    # Preserve factor level order when available, otherwise sort
    lvlOrder <- unique(unlist(lapply(vars, function(v) {
      x <- subData[[v]]
      if (is.factor(x)) levels(x) else NULL
    }), use.names = FALSE))
    allVals <- if (length(lvlOrder) > 0L) lvlOrder[lvlOrder %in% allVals] else sort(allVals)

    hits <- lapply(allVals, function(val) {
      perVar <- lapply(vars, function(v) {
        cells <- trimws(as.character(subData[[v]]))
        !is.na(cells) & cells == val
      })
      m <- matrix(unlist(perVar, use.names = FALSE), nrow = nCases, ncol = length(vars))
      rowSums(m) > 0L
    })

    presenceMat <- matrix(unlist(hits, use.names = FALSE),
                          nrow = nCases, ncol = length(allVals))
    colnames(presenceMat) <- allVals

    list(
      name       = setName,
      label      = setLabel,
      type       = "category",
      vars       = vars,
      labels     = allVals,
      matrix     = presenceMat,
      nCases     = nCases,
      nResponses = sum(presenceMat),
      warnings   = warnings
    )
  }
}

.mrFindSet <- function(sets, name) {
  for (s in sets) if (identical(s$name, name)) return(s)
  NULL
}

.mrKey <- function(...) gsub("[^A-Za-z0-9]", "_", paste0(...))

# ---------------
# Frequency tables
# ---------------

.mrFrequencyTables <- function(jaspResults, options, sets, ready) {

  if (is.null(jaspResults[["freqContainer"]])) {
    container <- createJaspContainer(title = gettext("Multiple Response Frequencies"))
    container$dependOn(c("frequencies", "freqResponsePct", "freqCasePct", "freqTotal",
                         .MR_SET_DEPS))
    container$position <- 1L
    jaspResults[["freqContainer"]] <- container
  }
  container <- jaspResults[["freqContainer"]]

  # Not ready: show a single empty skeleton table, exactly like other analyses
  if (!ready) {
    if (is.null(container[["freqTablePlaceholder"]]))
      container[["freqTablePlaceholder"]] <- .mrBuildFreqTable(NULL, options)
    return(invisible(NULL))
  }

  for (s in sets) {
    keyName <- paste0("freqTable_", .mrKey(s$name))
    if (!is.null(container[[keyName]])) next
    container[[keyName]] <- .mrBuildFreqTable(s, options)
  }

  invisible(NULL)
}

.mrBuildFreqTable <- function(set, options) {

  title <- if (is.null(set)) gettext("Frequencies")
           else gettextf("Frequencies: %s", set$label)

  tbl <- createJaspTable(title = title)
  tbl$addColumnInfo(name = "response", title = gettext("Response"), type = "string")
  tbl$addColumnInfo(name = "n",        title = gettext("N"),        type = "integer")
  if (isTRUE(options[["freqResponsePct"]]))
    tbl$addColumnInfo(name = "pctResp", title = gettext("% of Responses"), type = "number", format = "dp:1")
  if (isTRUE(options[["freqCasePct"]]))
    tbl$addColumnInfo(name = "pctCase", title = gettext("% of Cases"),     type = "number", format = "dp:1")

  if (is.null(set)) {
    tbl$addFootnote(gettext(
      "Assign variables to 'Multiple response variables' and allocate them to a set to run the analysis."
    ))
    return(tbl)
  }

  colFreqs <- colSums(set$matrix)
  nCases   <- set$nCases
  nResp    <- set$nResponses

  rows <- vector("list", length(set$labels))
  for (i in seq_along(set$labels)) {
    row <- list(response = set$labels[i], n = colFreqs[i])
    if (isTRUE(options[["freqResponsePct"]]))
      row$pctResp <- if (nResp  > 0L) colFreqs[i] / nResp  * 100 else NA_real_
    if (isTRUE(options[["freqCasePct"]]))
      row$pctCase <- if (nCases > 0L) colFreqs[i] / nCases * 100 else NA_real_
    rows[[i]] <- row
  }

  if (isTRUE(options[["freqTotal"]])) {
    totalRow <- list(response = gettext("Total"), n = nResp)
    if (isTRUE(options[["freqResponsePct"]]))
      totalRow$pctResp <- if (nResp > 0L) 100 else NA_real_
    if (isTRUE(options[["freqCasePct"]]))
      totalRow$pctCase <- if (nCases > 0L) nResp / nCases * 100 else NA_real_
    rows[[length(rows) + 1L]] <- totalRow
  }

  tbl$addRows(rows)

  tbl$addFootnote(gettextf(
    "Valid cases: %d. Total responses: %d. Percentages of cases can exceed 100%% in total because a respondent may select several options.",
    nCases, nResp
  ))
  for (w in set$warnings) tbl$addFootnote(w)

  tbl
}

# ---------------
# Crosstabulation
# ---------------

.mrCrosstabsOutput <- function(jaspResults, dataset, options, sets, ready) {

  if (is.null(jaspResults[["crosstabContainer"]])) {
    container <- createJaspContainer(title = gettext("Multiple Response Crosstabulation"))
    container$dependOn(c("crosstabs", "crosstabRowSet", "crosstabColSet",
                         "crosstabColumnType", "crosstabRowPct", "crosstabColPct",
                         "crosstabTotalPct", "chiSquare", "crosstabGroupVar",
                         "freqTotal", .MR_SET_DEPS))
    container$position <- 2L
    jaspResults[["crosstabContainer"]] <- container
  }
  container <- jaspResults[["crosstabContainer"]]

  if (!ready) {
    if (is.null(container[["crosstabPlaceholder"]])) {
      ph <- createJaspTable(title = gettext("Crosstabulation"))
      ph$addColumnInfo(name = "rowLabel",  title = "", type = "string")
      ph$addColumnInfo(name = "statLabel", title = "", type = "string")
      ph$addFootnote(gettext("Assign variables to a multiple response set to run the analysis."))
      container[["crosstabPlaceholder"]] <- ph
    }
    return(invisible(NULL))
  }

  ct <- .mrComputeCrosstab(dataset, options, sets)
  if (is.null(ct)) {
    if (is.null(container[["crosstabNotConfigured"]])) {
      ph <- createJaspTable(title = gettext("Crosstabulation"))
      ph$addColumnInfo(name = "rowLabel",  title = "", type = "string")
      ph$addColumnInfo(name = "statLabel", title = "", type = "string")
      ph$addFootnote(gettext(
        "Select a row set and either a column set or a grouping variable to build the crosstabulation."
      ))
      container[["crosstabNotConfigured"]] <- ph
    }
    return(invisible(NULL))
  }

  keyName <- paste0("crosstab_", .mrKey(ct$rowLabelAxis, "_", ct$colLabelAxis))
  if (is.null(container[[keyName]])) {
    tbl <- createJaspTable(title = gettextf(
      "Crosstabulation: %s (rows) \u00d7 %s (columns)", ct$rowLabelAxis, ct$colLabelAxis))
    .mrFillCrosstabTable(tbl, options, ct)
    container[[keyName]] <- tbl
  }

  if (isTRUE(options[["chiSquare"]]))
    .mrAddChiSquareTable(container, ct)

  invisible(NULL)
}

# Computes the contingency matrix for the configured crosstabulation.
# Returns NULL when the configuration is incomplete.
.mrComputeCrosstab <- function(dataset, options, sets) {

  rowSet <- .mrFindSet(sets, .mrOptionValue(options[["crosstabRowSet"]]))
  if (is.null(rowSet)) rowSet <- sets[[1L]]
  if (is.null(rowSet)) return(NULL)

  useSet <- identical(options[["crosstabColumnType"]], "set")

  if (useSet) {
    colSet <- .mrFindSet(sets, .mrOptionValue(options[["crosstabColSet"]]))
    if (is.null(colSet)) {
      others <- Filter(function(s) !identical(s$name, rowSet$name), sets)
      if (length(others) == 0L) return(NULL)
      colSet <- others[[1L]]
    }

    nCommon <- min(nrow(rowSet$matrix), nrow(colSet$matrix))
    mat1    <- rowSet$matrix[seq_len(nCommon), , drop = FALSE]
    mat2    <- colSet$matrix[seq_len(nCommon), , drop = FALSE]

    countMat <- .mrCoOccurrence(mat1, mat2)
    colLabels <- colSet$labels
    colAxis   <- colSet$label
    indepNote <- TRUE

  } else {
    groupVarName <- .mrOptionValue(options[["crosstabGroupVar"]])
    if (!nzchar(groupVarName) || !groupVarName %in% colnames(dataset)) return(NULL)

    groupVar  <- as.factor(dataset[[groupVarName]])
    colLabels <- levels(groupVar)
    if (length(colLabels) == 0L) return(NULL)

    nCommon  <- min(nrow(rowSet$matrix), length(groupVar))
    mat1     <- rowSet$matrix[seq_len(nCommon), , drop = FALSE]
    groupSub <- groupVar[seq_len(nCommon)]

    groupMat <- matrix(
      unlist(lapply(colLabels, function(l) !is.na(groupSub) & groupSub == l),
             use.names = FALSE),
      nrow = nCommon, ncol = length(colLabels))

    countMat  <- .mrCoOccurrence(mat1, groupMat)
    colAxis   <- decodeColNames(groupVarName)
    indepNote <- FALSE
  }

  list(
    countMat     = countMat,
    rowLabels    = rowSet$labels,
    colLabels    = colLabels,
    rowTotals    = rowSums(countMat),
    colTotals    = colSums(countMat),
    grandTotal   = sum(countMat),
    rowLabelAxis = rowSet$label,
    colLabelAxis = colAxis,
    indepNote    = indepNote
  )
}

# Counts, for every pair of columns, how many rows are TRUE in both matrices.
# Implemented as a matrix cross-product, which is far faster than nested loops.
.mrCoOccurrence <- function(matA, matB) {
  res <- crossprod(matA * 1L, matB * 1L)
  matrix(as.integer(res), nrow = ncol(matA), ncol = ncol(matB))
}

.mrFillCrosstabTable <- function(tbl, options, ct) {

  showRowPct   <- isTRUE(options[["crosstabRowPct"]])
  showColPct   <- isTRUE(options[["crosstabColPct"]])
  showTotalPct <- isTRUE(options[["crosstabTotalPct"]])
  showTotals   <- isTRUE(options[["freqTotal"]])

  countMat   <- ct$countMat
  rowTotals  <- ct$rowTotals
  colTotals  <- ct$colTotals
  grandTotal <- ct$grandTotal

  nRow <- length(ct$rowLabels)
  nCol <- length(ct$colLabels)

  tbl$addColumnInfo(name = "rowLabel",  title = ct$rowLabelAxis, type = "string")
  tbl$addColumnInfo(name = "statLabel", title = "",              type = "string")
  for (j in seq_len(nCol))
    tbl$addColumnInfo(name      = paste0("col", j),
                      title     = as.character(ct$colLabels[j]),
                      type      = "number",
                      format    = "dp:1",
                      overtitle = ct$colLabelAxis)
  if (showTotals)
    tbl$addColumnInfo(name = "rowTotal", title = gettext("Total"), type = "number", format = "dp:1")

  allRows <- list()

  for (i in seq_len(nRow)) {

    countRow <- list(rowLabel = ct$rowLabels[i], statLabel = gettext("Count"),
                     .isNewGroup = TRUE)
    for (j in seq_len(nCol)) countRow[[paste0("col", j)]] <- countMat[i, j]
    if (showTotals) countRow$rowTotal <- rowTotals[i]
    allRows <- c(allRows, list(countRow))

    if (showRowPct) {
      r <- list(rowLabel = "", statLabel = gettext("Row %"))
      for (j in seq_len(nCol))
        r[[paste0("col", j)]] <- if (rowTotals[i] > 0L) countMat[i, j] / rowTotals[i] * 100 else NA_real_
      if (showTotals) r$rowTotal <- if (rowTotals[i] > 0L) 100 else NA_real_
      allRows <- c(allRows, list(r))
    }

    if (showColPct) {
      r <- list(rowLabel = "", statLabel = gettext("Col %"))
      for (j in seq_len(nCol))
        r[[paste0("col", j)]] <- if (colTotals[j] > 0L) countMat[i, j] / colTotals[j] * 100 else NA_real_
      if (showTotals) r$rowTotal <- if (grandTotal > 0L) rowTotals[i] / grandTotal * 100 else NA_real_
      allRows <- c(allRows, list(r))
    }

    if (showTotalPct) {
      r <- list(rowLabel = "", statLabel = gettext("Total %"))
      for (j in seq_len(nCol))
        r[[paste0("col", j)]] <- if (grandTotal > 0L) countMat[i, j] / grandTotal * 100 else NA_real_
      if (showTotals) r$rowTotal <- if (grandTotal > 0L) rowTotals[i] / grandTotal * 100 else NA_real_
      allRows <- c(allRows, list(r))
    }
  }

  if (showTotals) {
    totalRow <- list(rowLabel = gettext("Total"), statLabel = gettext("Count"),
                     rowTotal = grandTotal, .isNewGroup = TRUE)
    for (j in seq_len(nCol)) totalRow[[paste0("col", j)]] <- colTotals[j]
    allRows <- c(allRows, list(totalRow))
  }

  tbl$addRows(allRows)

  pctParts <- c(if (showRowPct)   gettext(", row %"),
                if (showColPct)   gettext(", column %"),
                if (showTotalPct) gettext(", total %"))
  tbl$addFootnote(gettextf("Cell entries are counts%s. Grand total of responses: %d.",
                           paste(pctParts, collapse = ""), grandTotal))

  invisible(NULL)
}

.mrAddChiSquareTable <- function(container, ct) {

  keyName <- paste0("chiSquare_", .mrKey(ct$rowLabelAxis, "_", ct$colLabelAxis))
  if (!is.null(container[[keyName]])) return(invisible(NULL))

  chiTbl <- createJaspTable(title = gettext("Chi-Square Test"))
  chiTbl$addColumnInfo(name = "stat", title = gettext("Chi-square"), type = "number", format = "dp:3")
  chiTbl$addColumnInfo(name = "df",   title = gettext("df"),         type = "integer")
  chiTbl$addColumnInfo(name = "p",    title = gettext("p"),          type = "pvalue")

  result <- tryCatch(
    suppressWarnings(stats::chisq.test(ct$countMat, correct = FALSE)),
    error = function(e) NULL
  )

  if (is.null(result)) {
    chiTbl$addFootnote(gettext("The chi-square test could not be computed for this table."))
  } else {
    chiTbl$addRows(list(list(stat = result$statistic,
                             df   = result$parameter,
                             p    = result$p.value)))
    if (isTRUE(ct$indepNote))
      chiTbl$addFootnote(gettext(
        "The test is applied to the cell counts only. Multiple response data produce non-independent observations, so this result should be interpreted with caution."
      ))
  }

  container[[keyName]] <- chiTbl
  invisible(NULL)
}

# ---------------
# Plots
# ---------------

.mrChartsOutput <- function(jaspResults, dataset, options, sets) {

  wanted <- c("barChart", "horizontalBarChart", "stackedBarChart", "pieChart",
              "donutChart", "lineChart", "paretoChart")
  if (!any(vapply(wanted, function(o) isTRUE(options[[o]]), logical(1L))))
    return(invisible(NULL))

  if (is.null(jaspResults[["chartContainer"]])) {
    container <- createJaspContainer(title = gettext("Charts"))
    container$dependOn(c(wanted, "colorPalette", "chartMetric",
                         "paretoCumulativeLine", "paretoReferenceLine",
                         "crosstabRowSet", "crosstabColSet", "crosstabColumnType",
                         "crosstabGroupVar", .MR_SET_DEPS))
    container$position <- 3L
    jaspResults[["chartContainer"]] <- container
  }
  container <- jaspResults[["chartContainer"]]

  for (s in sets) {

    df <- .mrChartData(s, options)

    if (isTRUE(options[["barChart"]]))
      .mrAddPlot(container, paste0("bar_", .mrKey(s$name)),
                 gettextf("Bar Chart: %s", s$label), 520, 380,
                 function() .mrMakeBarChart(df, s$label, options, horizontal = FALSE))

    if (isTRUE(options[["horizontalBarChart"]]))
      .mrAddPlot(container, paste0("hbar_", .mrKey(s$name)),
                 gettextf("Horizontal Bar Chart: %s", s$label), 520, 380,
                 function() .mrMakeBarChart(df, s$label, options, horizontal = TRUE))

    if (isTRUE(options[["pieChart"]]))
      .mrAddPlot(container, paste0("pie_", .mrKey(s$name)),
                 gettextf("Pie Chart: %s", s$label), 460, 400,
                 function() .mrMakePieChart(df, s$label, options))

    if (isTRUE(options[["donutChart"]]))
      .mrAddPlot(container, paste0("donut_", .mrKey(s$name)),
                 gettextf("Donut Chart: %s", s$label), 460, 400,
                 function() .mrMakeDonutChart(df, s$label, options))

    if (isTRUE(options[["lineChart"]]))
      .mrAddPlot(container, paste0("line_", .mrKey(s$name)),
                 gettextf("Line Chart: %s", s$label), 520, 380,
                 function() .mrMakeLineChart(df, s$label, options))

    if (isTRUE(options[["paretoChart"]]))
      .mrAddPlot(container, paste0("pareto_", .mrKey(s$name)),
                 gettextf("Pareto Chart: %s", s$label), 560, 400,
                 function() .mrMakeParetoChart(s, options))
  }

  # Stacked bar chart reuses the crosstabulation configuration
  if (isTRUE(options[["stackedBarChart"]])) {
    ct <- .mrComputeCrosstab(dataset, options, sets)
    keyName <- "stackedBar"
    if (is.null(container[[keyName]])) {
      if (is.null(ct)) {
        plt <- createJaspPlot(title = gettext("Stacked Bar Chart"), width = 560, height = 400)
        plt$setError(gettext(
          "The stacked bar chart needs a crosstabulation. Define a second response set or assign a grouping variable."
        ))
        container[[keyName]] <- plt
      } else {
        .mrAddPlot(container, keyName,
                   gettextf("Stacked Bar Chart: %s \u00d7 %s", ct$rowLabelAxis, ct$colLabelAxis),
                   580, 400,
                   function() .mrMakeStackedBarChart(ct, options))
      }
    }
  }

  invisible(NULL)
}

# Creates a JaspPlot and captures plotting failures as plot errors instead of
# letting them abort the whole analysis.
.mrAddPlot <- function(container, key, title, width, height, builder) {
  if (!is.null(container[[key]])) return(invisible(NULL))

  plt <- createJaspPlot(title = title, width = width, height = height)
  container[[key]] <- plt

  obj <- tryCatch(builder(), error = function(e) e)
  if (inherits(obj, "error"))
    plt$setError(gettextf("Plotting not possible: %s", conditionMessage(obj)))
  else
    plt$plotObject <- obj

  invisible(NULL)
}

.mrChartData <- function(set, options) {

  metric   <- options[["chartMetric"]]
  colFreqs <- colSums(set$matrix)
  nCases   <- set$nCases
  nResp    <- set$nResponses

  value <- switch(metric,
    "responsePct" = if (nResp  > 0L) colFreqs / nResp  * 100 else rep(NA_real_, length(colFreqs)),
    "casePct"     = if (nCases > 0L) colFreqs / nCases * 100 else rep(NA_real_, length(colFreqs)),
    as.numeric(colFreqs)
  )

  yLabel <- switch(metric,
    "responsePct" = gettext("% of Responses"),
    "casePct"     = gettext("% of Cases"),
    gettext("Count")
  )

  data.frame(
    response = factor(set$labels, levels = set$labels),
    value    = as.numeric(value),
    count    = as.numeric(colFreqs),
    yLabel   = yLabel,
    stringsAsFactors = FALSE
  )
}

# ── Bar chart (vertical or horizontal) ───────────────────────────────────────

.mrMakeBarChart <- function(df, setLabel, options, horizontal = FALSE) {

  p <- ggplot2::ggplot(df, ggplot2::aes(x = response, y = value, fill = response)) +
    ggplot2::geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]]) +
    ggplot2::xlab(setLabel) +
    ggplot2::ylab(df$yLabel[1L])

  if (horizontal) {
    p <- p + ggplot2::coord_flip() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(legend.position = "none")
  } else {
    p <- p + jaspGraphs::geom_rangeframe(sides = "l") +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(legend.position = "none",
                     axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1))
  }

  p
}

# ── Pie chart ────────────────────────────────────────────────────────────────
# plotPieChart(value, group, legendName, ...) — value must be numeric and
# group a character/factor vector.

.mrMakePieChart <- function(df, setLabel, options) {
  jaspGraphs::plotPieChart(
    value      = as.numeric(df$value),
    group      = as.character(df$response),
    legendName = setLabel,
    palette    = options[["colorPalette"]]
  )
}

# ── Donut chart ──────────────────────────────────────────────────────────────

.mrMakeDonutChart <- function(df, setLabel, options) {

  total <- sum(df$value, na.rm = TRUE)
  if (!is.finite(total) || total <= 0)
    stop(gettext("all values are zero or missing"))

  df$fraction <- df$value / total
  df$ymax     <- cumsum(df$fraction)
  df$ymin     <- c(0, df$ymax[-length(df$ymax)])

  colors <- jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(nrow(df))

  ggplot2::ggplot(df, ggplot2::aes(xmax = 4, xmin = 2.5,
                                   ymax = ymax, ymin = ymin, fill = response)) +
    ggplot2::geom_rect(colour = "white", linewidth = 0.5) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::xlim(c(0, 4)) +
    ggplot2::scale_fill_manual(values = colors, name = setLabel) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      text = ggplot2::element_text(family = jaspGraphs::getGraphOption("family"),
                                   size   = jaspGraphs::getGraphOption("fontsize"))
    )
}

# ── Line chart ───────────────────────────────────────────────────────────────

.mrMakeLineChart <- function(df, setLabel, options) {

  colors <- jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(1L)

  ggplot2::ggplot(df, ggplot2::aes(x = response, y = value, group = 1L)) +
    ggplot2::geom_line(colour = colors[1L], linewidth = 0.9) +
    ggplot2::geom_point(colour = colors[1L], size = 2.5, fill = "white",
                        shape = 21, stroke = 1.2) +
    ggplot2::xlab(setLabel) +
    ggplot2::ylab(df$yLabel[1L]) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# ── Stacked bar chart ────────────────────────────────────────────────────────

.mrMakeStackedBarChart <- function(ct, options) {

  metric   <- options[["chartMetric"]]
  countMat <- ct$countMat
  nRow     <- length(ct$rowLabels)
  nCol     <- length(ct$colLabels)

  plotDf <- data.frame(
    rowCat = rep(factor(ct$rowLabels, levels = ct$rowLabels), times = nCol),
    colCat = rep(factor(ct$colLabels, levels = ct$colLabels), each  = nRow),
    count  = as.numeric(countMat),
    stringsAsFactors = FALSE
  )

  if (metric == "frequency" || ct$grandTotal <= 0L) {
    plotDf$value <- plotDf$count
    yLabel       <- gettext("Count")
  } else {
    plotDf$value <- plotDf$count / ct$grandTotal * 100
    yLabel       <- gettext("% of Responses")
  }

  ggplot2::ggplot(plotDf, ggplot2::aes(x = rowCat, y = value, fill = colCat)) +
    ggplot2::geom_col(position = "stack", colour = "white", linewidth = 0.3) +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]]) +
    ggplot2::labs(x = ct$rowLabelAxis, y = yLabel, fill = ct$colLabelAxis) +
    jaspGraphs::geom_rangeframe(sides = "l") +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# ── Pareto chart ─────────────────────────────────────────────────────────────
# Bars sorted in descending order plus a cumulative percentage line on a
# secondary axis, following the classic Pareto (80/20) convention.

.mrMakeParetoChart <- function(set, options) {

  freqs <- colSums(set$matrix)
  ord   <- order(freqs, decreasing = TRUE)
  freqs <- freqs[ord]
  labs  <- set$labels[ord]

  total <- sum(freqs)
  if (total <= 0)
    stop(gettext("no responses were counted for this set"))

  df <- data.frame(
    response   = factor(labs, levels = labs),
    count      = as.numeric(freqs),
    cumPct     = cumsum(freqs) / total * 100,
    stringsAsFactors = FALSE
  )

  showLine <- isTRUE(options[["paretoCumulativeLine"]])
  showRef  <- isTRUE(options[["paretoReferenceLine"]])

  maxCount <- max(df$count)
  # Scale factor mapping the 0-100 cumulative axis onto the count axis
  scaleFac <- if (maxCount > 0) maxCount / 100 else 1

  colors   <- jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(2L)
  barCol   <- colors[1L]
  lineCol  <- colors[min(2L, length(colors))]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = response)) +
    ggplot2::geom_col(ggplot2::aes(y = count),
                      fill = barCol, colour = "white", linewidth = 0.3, width = 0.7)

  if (showRef)
    p <- p + ggplot2::geom_hline(yintercept = 80 * scaleFac,
                                 linetype = "dashed", colour = "grey40", linewidth = 0.5)

  if (showLine)
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = cumPct * scaleFac, group = 1L),
                         colour = lineCol, linewidth = 0.9) +
      ggplot2::geom_point(ggplot2::aes(y = cumPct * scaleFac),
                          colour = lineCol, fill = "white", shape = 21,
                          size = 2.5, stroke = 1.2)

  yAxis <- if (showLine || showRef)
    ggplot2::scale_y_continuous(
      name     = gettext("Count"),
      sec.axis = ggplot2::sec_axis(~ . / scaleFac, name = gettext("Cumulative %")))
  else
    ggplot2::scale_y_continuous(name = gettext("Count"))

  p + yAxis +
    ggplot2::xlab(set$label) +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}
