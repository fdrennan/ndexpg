#' postgres_connector
#' @importFrom RPostgres Postgres
#' @importFrom glue glue
#' @importFrom DBI dbConnect
#' @param host The hostame, Sys.getenv('POSTGRES_HOST')
#' @param port The port, Sys.getenv('POSTGRES_PORT')
#' @param user The port, Sys.getenv('POSTGRES_USER')
#' @param password The port, Sys.getenv('POSTGRES_PASSWORD')
#' @param dbname The dbname, Sys.getenv('POSTGRES_DB')
#' @param max_attempts Max retries if error
#' @param verbose Boolean, shut up or not
#' @export postgres_connector
postgres_connector <- function(host = Sys.getenv('POSTGRES_HOST'),
                               port = Sys.getenv('POSTGRES_PORT'),
                               user = Sys.getenv('POSTGRES_USER'),
                               password = Sys.getenv('POSTGRES_PASSWORD'),
                               dbname = Sys.getenv('POSTGRES_DB'),
                               max_attempts = 2,
                               verbose = TRUE) {
  n <- 1
  if (verbose) message('First attempt at connection')
  repeat {
    connection <-
      try({
        dbConnect(Postgres(),
                  host = host,
                  port = port,
                  user = user,
                  password = password,
                  dbname = dbname)
      })
    if (inherits(connection, 'try-error')) {
      if (n > max_attempts) {
        if (verbose) stop('Connection to DB failed')
      }
      n <- n + 1
      if (verbose) message(glue('Trying to connect: try {n}'))
    } else {
      if (verbose) message(glue('Connected at {host'))
      break
    }
  }

  connection

}
