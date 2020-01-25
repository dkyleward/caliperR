#' Configures the environment for TransCAD
#'
#' Sets the error to dump.frames to prevent execution halt
#' Turns off command echo
#'
#' @keywords internal

setupTcCommandPrompt<- function() {
  options(error=dump.frames)
  # options(echo=FALSE)
}

#' Checks if filename is a valid .bin file
#'
#' Stops the program if the filename is not a valid .bin extension
#'
#' @param binFilename A data table with columns to convert
#' @keywords internal

checkIfValidBinFile <- function(binFilename) {
    binFilenameLength <- nchar(binFilename)
    binFilenameExtension <- substr(binFilename,binFilenameLength-3,binFilenameLength)
    if (!binFilenameExtension %in% c(".bin", ".BIN")) {
        stop("Filename does not have the .bin extension")
    }
}


#' Coerces data.frame columns to the specified classes
#'
#' Coerces data.frame columns to the specified classes
#'
#' @param dataTable A data table with columns to convert
#' @param colClasses A vector specifying the classes of each column
#' @keywords internal

convertColClasses <- function(dataTable, colClasses) {
    colClasses <- rep(colClasses, len=length(dataTable))
    dataTable[] <- lapply(seq_along(dataTable), function(i) switch(colClasses[i],
        Date=as.Date(dataTable[[i]], origin='1970-01-01'),
        POSIXct=as.POSIXct(dataTable[[i]], origin='1970-01-01'),
        as(dataTable[[i]], colClasses[i]) ))
}

#' Convert TransCAD type string to R datatype strings
#'
#' Convert TransCAD type string to R datatype strings
#'
#' @param typeChar String denoting the TC type
#' @return The correponding R type
#' @keywords internal

TcTypeToRType <- function(typeChar) {
    switch(as.character(typeChar),
    "C" = "character",
    "I" = "integer",
    "S" = "integer",
    "R" = "numeric",
    "F" = "numeric",
    "Date" = "Date",
    "Time" = "POSIXct",
    "DateTime" = "POSIXct",
    typeChar)
}

#' Convert R datatype strings to TransCAD type string
#'
#' Convert R datatype strings to TransCAD type string
#'
#' @param typeChar String denoting the R type
#' @return The correponding TC type
#' @keywords internal

RTypeToTcType <- function(typeChar) {
    switch(as.character(typeChar),
    "character" = "C",
    "integer" = "I",
    "numeric" = "R",
    "Date" = "Date",
    "POSIXct" = "DateTime",
    typeChar)
}

#' Convert TransCAD missing values to R NAs
#'
#' Convert TransCAD missing values to R NAs
#'
#' @param value Value of the data table entry to convert
#' @param typeChar String denoting the TC type
#' @return Either the value passed in or an NA
#' @keywords internal

TcMissToRNa <- function(value, typeChar) {
    switch(as.character(typeChar),
    "I" = {
        if (value == -2147483647)
            value <- NA
        value},
    "S" = {
        if (value == -32767)
            value <- NA
        value},
    "R" = {
        if (value == -1.7976931348623157e+308)
            value <- NA
        value},
    "F" = {
        if (value == -3.402823466e+38)
            value <- NA
        value},
    value)
}

#' Convert R NAs to TransCAD missing values
#'
#' Convert R NAs to TransCAD missing values
#'
#' @param value Value of the data table entry to convert
#' @param typeChar String denoting the TC type
#' @return Either the value passed in or a TC missing integer/real number
#' @keywords internal

RNaToTcMiss <- function(value, typeChar) {
    switch(as.character(typeChar),
    "I" = {
        if (is.na(value))
            value <- as.integer(-2147483647)
        value},
    "R" = {
        if (is.na(value))
            value <- -1.7976931348623157e+308
        value},
    value)
}

#' Return the byte length for each R data type
#'
#' Return the byte length for each R data type
#'
#' @param binDataCol The data table column for which data type to return
#' @return byte length for each R data type
#' @keywords internal

getByteLength <- function(binDataCol) {
    dataType <- class(binDataCol)
    # If the data frame has column labels/descriptions, remove that class
    dataType <- dataType[dataType != "labelled"]
    switch(as.character(dataType),
    "character" = max(sapply(binDataCol, nchar, type="bytes")),
    "integer" = 4,
    "numeric" = 8,
    "Date" = 4,
    "POSIXct" = 8)
}

#' Return the display length for each R data type
#'
#' Return the display length for each R data type
#'
#' @param binDataCol The data table column for which data type to return
#' @return A default character length for each field type
#' @keywords internal

getDisplayLength <- function(binDataCol) {
    dataType <- class(binDataCol)
    # If the data frame has column labels/descriptions, remove that class
    dataType <- dataType[dataType != "labelled"]
    switch(as.character(dataType),
    "character" = max(sapply(binDataCol, nchar, type="chars")),
    "integer" = 10,
    "numeric" = 10,
    "Date" = 12,
    "POSIXct" = 22)
}

#' Read the bin matrix into a data table
#'
#' Where all the heavy lifting is done
#' Reads data table column by column after the data is read
#' into a binary matrix
#'
#' @param binData
#' @param binMatrix
#' @param dcbKey
#' @param TcDataType
#' @param nRows
#' @param nCols
#' @return The modified binData data table
#' @import data.table
#' @keywords internal
readFfb <- function(binData, binMatrix, dcbKey, TcDataType, nRows, nCols) {
    for (i in 1:nCols) {
        startByte <- dcbKey[[i,"startByte"]]
        dataType <- dcbKey[[i,"dataType"]]
        byteLength <- dcbKey[[i,"byteLength"]]

        range <- seq(startByte, startByte+byteLength-1)
        if (dataType != "character") {
            binData[,i] <- readBin(binMatrix[range,], dataType, nRows, byteLength)
            binData[,(i) := sapply(binData[[i]], TcMissToRNa, TcDataType[i])]
        } else {
            binData[,i] <- readChar(binMatrix[range,], rep(byteLength,nRows), useBytes=TRUE)
        }
    }
    return(binData)
}

#' Read a bin file
#'
#' Read the data in binfilename and return a data table. Any field descriptions
#' will become column labels (use \code{View()} or \code{Hmisc::label()} to see
#' them).
#'
#' @param binFilename \code{string} The bin filename to read
#' @param returnDnames \code{bool} To return display names (if present in bin)
#' @return The data table read from the bin located at binFilename
#' @export
#' @import data.table
#' @import Hmisc
read_bin <- function(binFilename, returnDnames = FALSE) {
    checkIfValidBinFile(binFilename)

    # Find the corresponding .dcb file
    dcbFilename <- paste(substr(binFilename,1,nchar(binFilename)-3),"dcb", sep="")

    # Sometimes has words (i.e. "binary") after bytes per row
    nBytesPerRow <- as.numeric(read.csv(text=readLines(dcbFilename)[2], header=FALSE, sep=" ")[1])
    dcbFile <- fread(dcbFilename, skip=2, header = FALSE)
    dcbNames <- dcbFile[[1]]
    TcDataType <- dcbFile[[2]]
    fieldDescrs <- dcbFile[[10]]

    if (length(dcbFile) == 13){
      if (returnDnames) {
        display_names <- setNames(dcbFile[[13]], dcbNames)
        return(display_names)
      } else {
        # If not returning dnames, remove them if they exist
        dcbFile[, 13] <- NULL
      }
    } else {
      if (returnDnames) return(NA)
    }

    binFileSize <-file.info(binFilename)$size
    nRows <- ceiling(binFileSize / nBytesPerRow)
    nCols <- length(dcbNames)

    # Create a row key and order it by starting byte
    dcbKey <- data.table(
                index = c(1:nCols),
                startByte = dcbFile[[3]],
                dataType = sapply(dcbFile[[2]], TcTypeToRType),
                byteLength = dcbFile[[4]],
                descr = dcbFile[[10]]
              )
    setkey(dcbKey, "startByte")

    # Open binFilename before using nRows - produces clearer error message if file doesn't exist
    binFile <- file(binFilename, "rb")
    on.exit(close(binFile))

    binData <- data.table(matrix(ncol=nCols, nrow=1))
    colnames(binData) <- as.character(dcbNames)
    convertColClasses(binData, t(dcbKey[,"dataType"]))
    binData <- data.table(binData)[1:nRows]

    rawBinData <- readBin(binFile, what="raw", n=binFileSize)
    binMatrix <- matrix(rawBinData, nBytesPerRow, nRows)

    df <- readFfb(binData, binMatrix, dcbKey, TcDataType, nRows, nCols)
    df <- as.data.frame(df)

    # Create column labels from field descriptions
    descriptions <- setNames(fieldDescrs, dcbNames)
    Hmisc::label(df) <- as.list(descriptions)
    return(df)
}

#' Write data table's dcb file
#'
#' To write the data table into a TransCAD binary file (.bin),
#' we must first write a key in a .dcb file
#'
#' @param dcbKey The dcb data table to write in dcbFilename
#' @inheritParams write_bin
#' @import data.table
#' @keywords internal
writeDcbFile <- function(
  dcbKey, dcbFilename, description = "", dnames = NA) {
    dcbFile <- file(dcbFilename, "wb")
    on.exit(close(dcbFile))
    rowSize <- sum(unlist(dcbKey$byteLength))
    writeLines(c(description,as.character(rowSize)), dcbFile)

    nRows <- nrow(dcbKey)
    bytePosition <- 1
    for (i in 1:nRows) {
        decimalLength <- 0
        if (dcbKey[i, "dataType"] == "R"){
            decimalLength <- 4
        }

        line <- paste(
          paste('"',dcbKey[i, "colNames"],'"', sep=""),
          dcbKey[i, "dataType"],
          bytePosition,
          dcbKey[[i, "byteLength"]],
          0,
          dcbKey[[i, "displayLength"]],
          decimalLength,
          ',""',
          paste0("\"", dcbKey[i, "fieldDescrs"], "\""),
          ',"Copy",',
          sep = ","
        )

        # Add display names if they exist
        if(!(paste(dnames, collapse = "") == "NA")){
          colName <- dcbKey[[i, "colNames"]]
          display_name <- as.character(dnames[colName])
          if(!is.na(display_name)){
            if (display_name != "") {
              line <- paste0(line, "\"", display_name, "\"")
            }
          }
        }

        writeLines(line, dcbFile)
        bytePosition <- bytePosition + dcbKey[[i, "byteLength"]]
    }
}

#' Write data table into bin file
#'
#' Write the data table into a TransCAD binary file (.bin)
#' given the filename and description. If the table has
#' column labels, those will become field descriptions.
#'
#' @param binData \code{data.frame} The data table to write
#' @param binFilename \code{string} The file in which to write
#' @param description \code{string} A description of the binData
#' @param dnames \code{Named vector} of display names where each name is a
#'   column name and each value is the display name to use. For example:
#'   \code{c("hh" = "Households")} will assign the display name "Households" to
#'   column "hh".
#' @param n2i \code{bool} Whether to try to convert numerics to integers.
#'   This leads to integer columns in the bin (instead of real), which
#'   are smaller and easier to read. Defaults to \code{TRUE}. Conversion
#'   does not happen if the column contains decimal values.
#' @export
#' @import data.table
#' @import Hmisc
write_bin <- function(
  binData, binFilename, description='', dnames = NA, n2i = TRUE) {
  checkIfValidBinFile(binFilename)

  # Remove the 'labelled' class from columns if it exists
  for (i in 1:length(binData)) {
    class(binData[[i]]) <- setdiff(class(binData[[i]]), 'labelled')
  }

  # Convert numeric columns to integer
  if (n2i) {
    shrink_reals <- type.convert(binData, as.is = TRUE)
  } else {
    shrink_reals <- binData
  }

  # If any column in the data frame contains only NA values, the
  # column type is logical. Convert it to integer.
  convert_logics <- as.data.frame(lapply(shrink_reals, function(x){
    if (is.logical(x)) {
      return(as.integer(x))
    } else {
      return(x)
    }
  }), stringsAsFactors = FALSE)

  # Create the dcbKey and write the corresponding .dcb file
  dcbFilename <- paste(substr(binFilename,1,nchar(binFilename)-3),"dcb", sep="")
  dataType <- sapply(sapply(convert_logics, class), RTypeToTcType)
  dataType <- dataType[dataType != "labelled"]
  dcbKey <- data.table(
    colNames = colnames(binData),
    dataType = dataType,
    byteLength = sapply(convert_logics, getByteLength),
    displayLength = sapply(convert_logics, getDisplayLength),
    fieldDescrs = Hmisc::label(binData)
  )
  writeDcbFile(dcbKey, dcbFilename, description, dnames)

  # Open bin file
  binFile <- file(binFilename, "wb")
  on.exit(close(binFile))

  nRows <- nrow(convert_logics)
  nCols <- ncol(convert_logics)
  for (i in 1:nRows) {
    for (j in 1:nCols) {
      dataType <- dcbKey[[j,"dataType"]]
      byteLength <- dcbKey[[j,"byteLength"]]
      if (dataType != "C" ) {
        writeBin(RNaToTcMiss(convert_logics[[i,j]], dataType), binFile, byteLength)
      } else {
        # Use readChar() because readBin() doesn't read the correct number of bytes for characters
        writeChar(convert_logics[[i,j]], binFile, byteLength, eos=NULL)
      }
    }
  }
}
