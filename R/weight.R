#' weight function
#'
#' @param i the first movie
#' @param j the second movie
#' @param genre_matrix a matrix with all the genres and movies
#' @param key_matrix a matrix with all the keywords and movies
#'
#' @return a weight
#' @export
#'
#' @examples
#' \dontrun{
#' weight <-  function(i,j,genre_matrix = matrix_movies, key_matrix = matrix_movies2)
#' }
#'
weight <- function(i,j,genre_matrix, key_matrix) {
  if (i< j) {
    a <- score_key(i,j, key_matrix)
    b <- score_genre(i,j,genre_matrix)
    return(max(min(a-1, b-1, 1/(5*a+b)), 0))
    #at least 2 keywords and 1 genre, otherwise returns 0; + importance given to keywords than genres
  }
  else{
    return(0)
  }
}
