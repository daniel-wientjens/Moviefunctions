#' Movie path
#'
#' @param names a vector filled with two movies
#' @param movies_dataset a dataframe with all the movies (a column with the titles is used here)
#' @param movies_graph a network showing all the edges between the nodes/movies
#'
#' @return a dataframe
#' @export
#' @importFrom igraph shortest_paths
#'
#' @examples
#' \dontrun{
#' movies_path(names, movies_dataset = tmdb, movies_graph = graph)
#' }
#'
movies_path <- function(names, movies_dataset, movies_graph) {
  i <- which(movies_dataset$original_title == names[1])
  j <- which(movies_dataset$original_title == names[2])
  l <- shortest_paths(movies_graph, i,j)
  l1 <- l[[1]][[1]]
  movies_list <- list()
  for (i in 1:length(l1)){
    movies_list[[i]] <- movies_dataset$original_title[l1[i]]
  }
  movies_list <- do.call(rbind.data.frame, movies_list)
  colnames(movies_list) <- c("Pathway from first movie to second movie")
  return(movies_list)

}
