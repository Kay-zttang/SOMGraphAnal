#' Calculate graph properties
#'
#' This function calculate several graph properties for CONN graph. 
#'
#' @param conn.graph A CONN graph
#' @param param.table The key-value pair parameter table 
#' @param method  The choice for taking "allPEs": all PEs \ "activePEs": active PEs into account
#' @param save_to.excel  The choice for save all to an .xlsx file in the subfolder '/res'
#' @return A list with graph measurement results.
#' @export
calc.graph_property <- function(list, supervised = FALSE, save_to.excel = FALSE, method = "allPEs"){
  options(scipen=999)
  mainDir = box::file() 
  setwd(mainDir)
  
  if(method == 'allPEs'){
    gconn = list$conn.graph
  }else if(method == 'activePEs'){
    gconn = list$conn.graph
    Isolated = which(igraph::degree(gconn)==0)
    gconn = igraph::delete.vertices(gconn, Isolated)
  }else{
    stop("Wrong method selected.")
  }
  
  i = 0 # progress bar
  if (supervised){
    maxpb = 32
  }else{
    maxpb = 31
  }
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = maxpb, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 31,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  
  property = c()
  
  n = length(igraph::V(gconn)) # order (# vertices)
  deadPE = sum(igraph::degree(gconn)==0) #inactive prototypes
  m = length(igraph::E(gconn)) # size (# edges)
  add = c(n,deadPE,m)
  property = append(property,add)
  i = i + length(add)
  setTxtProgressBar(pb, i)
  
  shortdist = igraph::distances(gconn)
  shortdist[is.infinite(shortdist)] <- 0 
  # max(shortdist) ## checking max shortest path (diameter)
  diam = igraph::diameter(gconn) # diameter
  # get_diameter(gconn1) ## get an example path with the related diameter
  min_dis = min(shortdist) # min shortest path
  avg_dis = igraph::mean_distance(gconn, details = TRUE)
  # res: Average length of Shortest path 
  # unconnected: Unconnected pairs of nodes (count both)
  add  = c(diam,min_dis,avg_dis$res, avg_dis$unconnected)
  property = append(property,add)
  i = i + length(add)
  setTxtProgressBar(pb, i)
  
  vdegree = igraph::degree(gconn) # node degree vector
  min_deg = min(vdegree) # min degree
  max_deg = max(vdegree) # max dgeree
  avg_deg = mean(vdegree) #avg degree
  add = c(min_deg,max_deg,avg_deg)
  property = append(property,add)
  i = i + length(add)
  setTxtProgressBar(pb, i)
  
  cli_len = length(igraph::cliques(gconn)) # number of cliques
  cli_num = igraph::clique_num(gconn) # number of maximal cliques
  add = c(cli_len, cli_num)
  property = append(property, add)
  i = i + length(add)
  setTxtProgressBar(pb, i)
  
  c = igraph::transitivity(gconn,type = 'global') # Clustering Coefficient
  c_weight = igraph::transitivity(gconn, type = 'weighted',weights = igraph::E(gconn)$weight)  
  c_weight[is.na(c_weight)] = 0
  c_weighted = mean(c_weight) # Weighted Clustering Coefficient
  add = c(c,c_weighted)
  property = append(property,add)
  i = i + length(add)
  setTxtProgressBar(pb, i)
  
  eig = eigen(list$conn)
  eigen = eig$values # eigenvalues of CONN 
  max_eig = max(eigen) # max eigenvalue
  min_eig = min(eigen) # min eigenvalue
  avg_eig = mean(eigen) # avg eigenvalue
  dom_eig = max(abs(eigen)) # Dominant eigenvalue
  spec_gap = eigen[1]-eigen[2] # Spectral gap
  g_energy = sum(abs(eigen)) # graph energy
  add = c(max_eig, min_eig, avg_eig, dom_eig, spec_gap, g_energy)
  property = append(property, add)
  i = i + length(add)
  setTxtProgressBar(pb, i)
  
  #tmax: maximum theoretical graph level centralization score
  c_d = igraph::centr_degree(gconn)$centralization # degree centrality index
  c_d.tmax = igraph::centr_degree(gconn)$theoretical_max
  c_c = igraph::centr_clo(gconn)$centralization # closeness centrality index
  if (is.nan(c_c)){c_c = 'NaN'}
  c_c.tmax = igraph::centr_clo(gconn)$theoretical_max
  c_b = igraph::centr_betw(gconn)$centralization # betweenness centrality index
  c_b.tmax = igraph::centr_betw(gconn)$theoretical_max
  c_ev = igraph::centr_eigen(gconn)$centralization # eigenvector centrality index
  c_ev.tmax = igraph::centr_eigen(gconn)$theoretical_max
  c_h = igraph::centralize(igraph::harmonic_centrality(gconn), length(igraph::V(gconn))) ##harmonic centrality index in eq(9)
  c_sc = igraph::centralize(igraph::subgraph_centrality(gconn), length(igraph::V(gconn))) ##subgraph centrality
  # for subgraph centrality the function only calculated as binary adjacency Matrix as_adj(gconn)
  # To get an orthonormal eigenbasis of eigenvectors (Normalization; already)
  # typically dividing by its length sqrt(transpose(v)*v)
  # manually calculation as belows:
  # eig$vectors^2 %*% exp(eig$values)
  c_wsc = igraph::centralize(eig$vectors^2 %*% exp(eig$values), length(igraph::V(gconn)))
  
  add = c(c_d, c_d.tmax, c_c, c_c.tmax, c_b, c_b.tmax, c_ev, c_ev.tmax, c_h, c_sc, c_wsc)
  property = append(property, add)
  i = i + length(add)
  setTxtProgressBar(pb, i)
  
  property.name = c('Order (# vertices)','# Inactive prototypes (of 36)', 'Size (# edges)',
                    'Diameter (max shortest path)', 'Minimum shortest path length',
                    'Average shortest path length', '# Unconnected pairs of nodes',
                    'Minimum degree', 'Maximum degree', 'Average degree',
                    '# Cliques', '# Maximal cliques', 
                    'Clustering coefficient', 'Weighted clustering coefficient',
                    'Maximum eigenvalue', 'Minimum eigenvalue', 'Average eigenvalue',
                    'Dominant eigenvalue (max(abs(eigenvalue)))', 
                    'Spectral gap (eigval1-eigval2)','Graph energy (sum(abs(eigval)))',
                    'Degree centrality index', 'Degree centrality theoretical maximum',
                    'Closeness centrality index', 'Closeness centrality theorectical maximum',
                    'Betweenness centrality index', 'Betweenness centrality theoretical maximum',
                    'Eigenvector centrality index', 'Eigenvector centrality theoretical maximum',
                    'Harmonic centrality index', 
                    'Subgraph centrality index', 'Weighted Subgraph centrality index')
  
  if(supervised){
    q = igraph::modularity(gconn, igraph::V(gconn)$class, weights = igraph::E(gconn)$weight) # Modularity under class label
    add = c(q) 
    property = append(property, add)
    property.name = append(property.name, 'Modularity')
    i = i + length(add)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  property = data.frame(property)
  colnames(property) = c(paste0(format(list$steps, big.mark = ",", scientific = F), ' steps'))
  row.names(property) = property.name
  property.out <- tibble::rownames_to_column(property, " ") 
  
  vdegree = data.frame(vdegree)
  colnames(vdegree) = c(paste0(format(list$steps, big.mark = ",", scientific = F), ' steps'))
  vdegree.out <- tibble::rownames_to_column(vdegree, " ") 
  
  eigen = data.frame(eigen)
  colnames(eigen) = c(paste0(format(list$steps, big.mark = ",", scientific = F), ' steps'))
  eigen.out <- tibble::rownames_to_column(eigen, " ") 
  
  subDir <- "res"
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  dir = paste0('res/graph_measurements.',list$root,'.xlsx')
  
  if (save_to.excel) {
    if(method == 'allPEs'){
      savefile = list("Properties" = property.out, "Eigenvalues" = eigen.out, "Degrees" = vdegree.out)
    }else{
      savefile = list("Properties_active" = property.out, "Eigenvalues_active" = eigen.out, "Degrees_active" = vdegree.out)
    }
    openxlsx::write.xlsx(savefile, dir, colnames = T,rownames = T)
  }
  
  list(graph.property = property, graph.degree = vdegree, graph.eigenvalue = eigen)
}