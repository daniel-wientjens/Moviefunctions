#' Score keywords
#'
#' @param i integer, the first movie index
#' @param j integer, the second movie index
#' @param keywords_matrix matrix, rows correspond to movies and entries are the keywords
#'
#' @return an integer, the number of keywords in common
#' @export
#'
#' @examples
#' \dontrun{
#' score_key(1,4, keywords_matrix = matrix_movies2)
#' }
#'
score_key <- function(i,j, keywords_matrix) {
  vector_movies <- intersect(keywords_matrix[i,], keywords_matrix[j,])
  vector_movies <- vector_movies[!is.na(vector_movies)]
  return(length(vector_movies))
}
