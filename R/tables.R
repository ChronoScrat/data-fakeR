#' @title Create Tables
#'
#' @description A function that creates the tables provided in the configuration schema. Normally,
#'  it returns dataframes. This function can be used on its own, but it is better if it is called
#'  after `import_schema`.s
#'
#' @param list The list object created by the YAML file.
#'
#' @returns A list of dataframes specified in the schema list.
#'
#' @export
create_tables <- function(list){

  rtn_list <- list()

  # Get each table
  for(i in 1:length(list$tables)){

    name <- list$tables[[i]]$name
    rows <- list$tables[[i]]$rows
    columns <- list$tables[[i]]$columns

    ## Create a dataframe with nrows = rows and 0 columns. That data-frame

    tmpDF <- data.frame(
      matrix(,
             nrow = rows,
             ncol = 0)
    )

    ## Create columns

    for(j in 1:length(columns)){

      tmpDF <- create_column(columns[[j]],tmpDF)

    }

    rtn_list <- append(rtn_list, list(tmpDF))


  }

  return(rtn_list)

}


#' @title Create column
#'
#' @description This function creates a column in the 'frame' dataframe based on
#'  the specification provided in the 'clmnList' list.
#'
#' @param clmnList A list detailing the schema for the column
#' @param frame The dataframe where the column will be inserted into
#'
#' @returns A Dataframe
#'
#' @importFrom dplyr rowwise mutate
#' @importFrom lubridate as_date as_datetime
#'
#' @author Nathanael Rolim \email{Nathanael.Rolim@@gamail.com}
#'
create_column <- function(clmnList, frame){

  # Get column name and type

  clmn_name <- clmnList$name
  clmn_type <- clmnList$column_type

  frame <- frame

  # Match: Fisrt, we need to determine the type of column we have, and then apply
  # the necessary steps to create that column. For 'Fixed' and 'Expression' columns,
  # that should be relatively straightforward. But for 'Sequential' and 'Random',
  # it should take into consideration the data_type.

  # TODO Make Random columns of Logical data_type convert 0 and 1 to FALSE and TRUE.

  if(clmn_type == "Fixed"){

    frame[clmn_name] <- clmnList$value

  } else if(clmn_type == "Expression") {

    ## The expression
    exprs <- parse(text = clmnList$expression)

    frame <- frame |>
      dplyr::rowwise() |>
      dplyr::mutate( "{clmn_name}" := eval(exprs) )

  } else if(clmn_type == "Sequential"){

    start <- clmnList$start

    if(clmnList$data_type == "Integer"){
      start <- as.integer(start)
    } else if(clmnList$data_type == "Double"){
      start <- as.double(start)
    } else if(clmnList$data_type == "Date") {
      start <- lubridate::as_date(start)
    } else {
      start <- lubridate::as_datetime(start)
    }


    frame[clmn_name] <- seq(from = start,
                            by = clmnList$step,
                            length.out = nrow(frame))

  } else if(clmn_type == "Random") {

    min <- clmnList$min
    max <- clmnList$max

    if(clmnList$data_type == "Integer"){

      frame[clmn_name] <- as.integer( runif( nrow(frame), min = min, max = max ) )

    } else if(clmnList$data_type == "Double"){

      frame[clmn_name] <- runif( nrow(frame), min = min, max = max )

    } else if(clmnList$data_type == "Date"){

      min <- lubridate::as_date(min)
      max <- lubridate::as_date(max)

      frame[clmn_name] <- lubridate::as_date( runif( nrow(frame), min = min, max = max ) )

    } else if(clmnList$data_type == "Timestamp") {

      min <- lubridate::as_date(min)
      max <- lubridate::as_date(max)

      frame[clmn_name] <- lubridate::as_datetime( runif( nrow(frame), min = min, max = max ) )

    } else{ # Logical

      frame[clmn_name] <- as.integer( runif( nrow(frame), min = 0, max = 1.9 ) )

    }

  }


  return(frame)
}
