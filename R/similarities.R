#' Similarities function
#'
#' @param names a vector filled with only two movie names
#' @param movies_dataset a dataframe with all the movies including a column with the titles
#' @param sim_matrix a matrix with the genres or keywords and a column with the movie titles
#'
#' @return a list, of genres or keywords in common
#' @export
#'
#' @examples
#' \dontrun{
#' similarities(c("The Dark Knight Rises","Superman Returns"),tmdb,matrix_movies)
#' similarities(c("The Dark Knight Rises","Superman Returns"),tmdb,matrix_movies2)
#' }
#'
similarities <- function(names, movies_dataset, sim_matrix) {
  i <- which(movies_dataset$original_title == names[1])
  j <- which(movies_dataset$original_title == names[2])
  genre <- intersect(sim_matrix[i,],sim_matrix[j,])
  genre <- genre[!is.na(genre)]
  return(genre)
}
