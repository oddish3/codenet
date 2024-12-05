#' Plot Network
#'
#' @param edges Data frame of edges representing function dependencies
#' @return A visNetwork object
#' @noRd
plot_network <- function(edges) {
  # Create nodes data frame
  nodes <- data.frame(
    id = unique(c(edges$from, edges$to)),
    label = unique(c(edges$from, edges$to))
  )

  # Create the network
  network <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visEdges(arrows = "from") %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    ) %>%
    visNetwork::visLayout(randomSeed = 123)

  return(network)
}

#' Plot Enhanced Network
#'
#' @param edges Edge relationships
#' @param object_types Types of objects
#' @noRd
plot_enhanced_network <- function(edges, object_types) {
  # Create nodes data frame with type information
  nodes <- data.frame(
    id = names(object_types),
    label = names(object_types),
    group = object_types,
    stringsAsFactors = FALSE
  )

  # Color scheme for different types
  colors <- list(
    "function" = "#97C2FC",    # blue
    "data.frame" = "#FB7E81",  # red
    "list" = "#7BE141",        # green
    "other" = "#FFA807"        # orange
  )

  # Ensure the edges data frame has required columns
  if (!all(c("from", "to") %in% colnames(edges))) {
    stop("The edges data frame must contain 'from' and 'to' columns.")
  }

  # Create the network visualisation
  network <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visGroups(
      groupname = "function",
      color = colors[["function"]],
      shape = "dot"
    ) %>%
    visNetwork::visGroups(
      groupname = "data.frame",
      color = colors[["data.frame"]],
      shape = "square"
    ) %>%
    visNetwork::visGroups(
      groupname = "list",
      color = colors[["list"]],
      shape = "triangle"
    ) %>%
    visNetwork::visGroups(
      groupname = "other",
      color = colors[["other"]],
      shape = "diamond"
    ) %>%
    visNetwork::visLegend(width = 0.1, position = "right") %>%
    visNetwork::visEdges(arrows = "from") %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    )

  return(network)
}
