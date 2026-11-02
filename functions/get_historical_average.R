get_historical_average <- function(df, indice = 2) {
  n <- nrow(df)
  ha <- df[, 1:2] # Keep date and gold price columns
  y <- df[, indice]
  ha$ha <- cumsum(y) / seq_along(y)
  return(ha)
}