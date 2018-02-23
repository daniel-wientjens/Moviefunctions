#' score_key
#'
#' @param i the first movie
#' @param j the second movie
#' @param keywords_matrix the keyword matrix with the movies
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' score_key(i,j, keywords_matrix = matrix_movies2)
#' }
#'
score_key <- function(i,j, keywords_matrix) {
  vector_movies <- intersect(keywords_matrix[i,], keywords_matrix[j,])
  vector_movies <- vector_movies[!is.na(vector_movies)]
  return(length(vector_movies))
}
