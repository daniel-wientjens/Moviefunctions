#' Movies path
#'
#' @param names a vector filled with only two movie names
#' @param movies_dataset a dataframe with all the movies including a column with the titles
#' @param movies_graph a graph where vertices are movies and weighted edges between movies
#'
#' @return a dataframe with the shortest path between two movies
#' @export
#' @importFrom igraph shortest_paths
#'
#' @examples
#' \dontrun{
#' movies_path(c("Superman", "Batman"), movies_dataset = tmdb, movies_graph = graph)
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
