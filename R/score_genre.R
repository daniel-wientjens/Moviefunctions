#' score_genre
#'
#' @param i the first movie
#' @param j the second movie
#' @param genre_matrix the matrix with all the genres and the movies
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' score_genre <- function(i,j, genre_matrix)
#' }
score_genre <- function(i,j, genre_matrix) {
  vector_movies <- intersect(genre_matrix[i,], genre_matrix[j,])
  vector_movies <- vector_movies[!is.na(vector_movies)]
  return(length(vector_movies))
}
