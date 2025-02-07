library(graph)
library(Rgraphviz)

## =============================================================================
## plotAG function (slightly modified from the pcalg package)
## =============================================================================
#' Plot PAG (partial ancestral graph) for FCI and CCI algorithms
#'
#' @param amat adjacency matrix of the resulting estimated graph
#'
#' @details
#' "0": no edge; "1": circle; "2": arrow; "3": tail
#'
#' @return a PAG graph of graphNEL class
plotAG <- function(amat, title_text = NULL)
{
  # create a graph object
  g <- as(amat,"graphNEL")
  # extract node info
  nn <- nodes(g)
  # extract number of nodes
  p <- numNodes(g)
  # extract number of edges
  n.edges <- numEdges(g)
  ah.list <- at.list <- vector("list", n.edges)
  l.names <- character(n.edges)
  # assign a shape for each edge type
  amat[amat == 1] <- "odot"
  amat[amat == 2] <- "vee"
  amat[amat == 3] <- "none"
  amat[amat == 4] <- "tee"
  
  iE <- 0
  for (i in 1:(p-1)) {
    x <- nn[i]
    for (j in (i+1):p) {
      y <- nn[j]
      if (amat[x,y] != 0) {
        iE <- iE + 1
        ah.list[[iE]] <- amat[x,y]
        at.list[[iE]] <- amat[y,x]
        l.names[[iE]] <- paste0(x,"~",y)
      }
    }
  }
  names(ah.list) <- names(at.list) <- l.names
  
  edgeRenderInfo(g) <- list(arrowhead = ah.list, arrowtail = at.list)
  # global features
  graph.par(list(nodes=list(cex = 1.5)))
  # plot the PAG
  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))
  
  # If title_text is provided, add a title
  if (!is.null(title_text)) {
    title(main = title_text, cex.main = 2, line = 3.5)
  }
}



## =============================================================================
## plotPC function 
## =============================================================================
#' Plot CPDAG (completed partially directed acyclic graph) for PC algorithm
#'
#' @param amat adjacency matrix of the resulting estimated graph
#'
#'
#' @return a CPDAG graph of graphNEL class
plotPC <- function(amat, title_text = NULL) {
  # Convert adjacency matrix to a `graphNEL` object
  g <- as(amat, "graphNEL")

  # Extract node information
  nn <- nodes(g)
  p <- numNodes(g)
  n.edges <- numEdges(g)

  # Initialize edge attributes
  ah.list <- at.list <- vector("list", n.edges)
  l.names <- character(n.edges)

  # Assign edge types based on matrix values
  iE <- 0
  for (i in 1:p) {
    for (j in 1:p) {
      if (amat[i, j] == 1 || amat[j, i] == 1) {
        iE <- iE + 1
        if (amat[i, j] == 1 && amat[j, i] == 1) {
          # Bidirectional edge i <-> j
          ah.list[[iE]] <- "vee"  # Head: circle
          at.list[[iE]] <- "vee"  # Tail: circle
        } else if (amat[i, j] == 1 && amat[j, i] == 0) {
          # Directed edge i -> j
          ah.list[[iE]] <- "vee"  # Head: arrow
          at.list[[iE]] <- "none" # Tail: none
        } else if (amat[i, j] == 0 && amat[j, i] == 1) {
          # Directed edge j -> i
          ah.list[[iE]] <- "none" # Head: none
          at.list[[iE]] <- "vee"  # Tail: arrow
        }
        l.names[[iE]] <- paste0(nn[i], "~", nn[j])
      }
    }
  }

  # Assign edge attributes to the graph
  names(ah.list) <- names(at.list) <- l.names
  edgeRenderInfo(g) <- list(arrowhead = ah.list, arrowtail = at.list)

  # Global features for nodes
  graph.par(list(nodes = list(cex = 1.5)))

  # Render the graph
  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))

  # Add the title if provided
  if (!is.null(title_text)) {
    title(main = title_text, cex.main = 2, line = 3.5)
  }
}


