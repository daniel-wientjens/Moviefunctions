#' similarities_genre
#'
#' @param names a vector filled with two movies
#' @param movies_dataset a dataframe with all the movies (a column with the titles is used here called "original_title")
#' @param genre_matrix a matrix filled with the genres and the last column has the movie titles
#'
#' @return a list of genres in common
#' @export
#'
#' @examples
#' \dontrun{
#' similarities_genre(c("The Dark Knight Rises","Superman Returns"),tmdb,matrix_movies)
#' }
#'
similarities_genre <- function(names, movies_dataset, genre_matrix) {
  i <- which(movies_dataset$original_title == names[1])
  j <- which(movies_dataset$original_title == names[2])
  genre <- intersect(genre_matrix[i,],genre_matrix[j,])
  genre <- genre[!is.na(genre)]
  return(genre)
}
