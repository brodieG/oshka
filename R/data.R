
nfl.state <- c(
  "Arizona", "California", "Colorado", "Florida", "Georgia", "Illinois",
  "Indiana", "Louisiana", "Maryland", "Massachusetts", "Michigan", "Minnesota",
  "Missouri", "New Jersey", "New York", "North Carolina", "Ohio",
  "Pennsylvania", "Tennessee", "Texas", "Washington", "Wisconsin"
)
#' State Data
#'
#' Selected statistics from the R built-in `state` data set, as well as an
#' additional column indicating whether the state has at least one NFL team or
#' not.
#'
#' @export
#' @seealso [state]
#' @docType data
#' @name state.data

state.data <- cbind(
  data.frame(
    State=state.name, Region=state.region,
    Population=unname(state.x77[, "Population"]),
    Income=unname(state.x77[, "Income"]),
    Illiteracy=unname(state.x77[, "Illiteracy"]),
    HasNfl=state.name %in% nfl.state
  )
)

