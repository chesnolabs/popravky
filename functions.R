
library(networkD3)
library(igraph)


# Function to filter the values ####
create_graph <- function(edges, vertices) {
  
  edges <- edges %>% 
    filter(names_mps_x %in% vertices$full_name,  # person_x
           names_mps_y %in% vertices$full_name)  # person_y
  
  v_in_edges <- edges %>% 
    gather(key = vert_type, value = vertice, 
           names_mps_x:names_mps_y) %>% 
    select(vertice) %>% 
    distinct() %>% 
    pull()
  
  vertices <- vertices %>% 
    filter(full_name %in% v_in_edges)
  
  g <- igraph::graph_from_data_frame(edges, vertices = vertices)
  g
}

# Function for an interactive graph ####
mps_force_network <- function(edges, vertices) {
  
  g <- create_graph(edges, vertices)
  
  network <- igraph_to_networkD3(g, group = vertex_attr(g, "factions", index = V(g)))
  
  forceNetwork(Links = network$links, 
               
               Nodes = network$nodes, 
               
               Source = "source", 
               Target = "target", 
               NodeID = "name", 
               Group = "group",
               #Nodesize = "size",
               legend = TRUE, 
               zoom=TRUE,
               opacity = 0.9, fontSize = 30, 
               bounded = TRUE, charge = -15)
}


