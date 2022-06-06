#' @title Import and Parse YAML file
#'
#' @description This function reads the YAML file.
#'
#' @importFrom yaml read_yaml
#'
#' @examples
#'  \dontrun{
#'  importParse("_faker.yaml")
#'  }
#'
#' @author Nathanael Rolim \email{Nathanael.Rolim@@gamail.com}
#'
#' @export
importParse <- function(file = ""){

  yaml::read_yaml(file)

}
