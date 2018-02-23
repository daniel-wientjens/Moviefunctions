#' Score genre
#'
#' @param i integer, the first movie index
#' @param j integer, the second movie index
#' @param genre_matrix the matrix with all the genres and the movies
#'
#' @return an integer, the number of genres in common
#' @export
#'
#' @examples
#' \dontrun{
#' score_genre <- function(1,3, genre_matrix = matrix_movies)
#' }
score_genre <- function(i,j, genre_matrix) {
  vector_movies <- intersect(genre_matrix[i,], genre_matrix[j,])
  vector_movies <- vector_movies[!is.na(vector_movies)]
  return(length(vector_movies))
}
