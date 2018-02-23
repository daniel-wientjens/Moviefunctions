#' similarities_key
#'
#' @param names a vector filled with two movies
#' @param movies_dataset a dataframe with all the movies (a column with the titles is used here called "original_title")
#' @param key_matrix a matrix filled with the keywords and the last column has the movie titles
#'
#' @return a list of the similar keywords between the two movies
#' @export
#'
#' @examples
#' \dontrun{
#' similarities_key(c("The Dark Knight Rises","Superman Returns"), tmdb, matrix_movies2)
#' }
#'
similarities_key <- function(names, movies_dataset, key_matrix) {
  i <- which(movies_dataset$original_title == names[1])
  j <- which(movies_dataset$original_title == names[2])
  keywords <- intersect(key_matrix[i,],key_matrix[j,])
  keywords <- keywords[!is.na(keywords)]
  return(keywords)
}
