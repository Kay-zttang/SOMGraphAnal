#' Load parameter table.
#'
#' This function search realated possible wwightcube .wgtcub/.wgt.xv file and 
#' the class label .nunr file for the input list of CONN .cadj file for each 
#' learning steps. And integrated as an output dataframe.
#' 
#' @param infile Path to the input .cadj file
#' @return A data frame with only one row showing the current learning steps and 
#' directories of related SOM files.
#' @export
load.parm_SOM <- function(infile){
  data.df = data.frame(matrix(ncol = 5, nrow = 0))
  colnames(data.df) <- c('steps', 'root_name', 'cadj_matrix', 'weight_cube', 'nunr')
  
  fname = sub('\\.cadj.viff$','',tail(strsplit(infile, "/")[[1]],n=1))
  step = suppressWarnings(as.numeric(strsplit(fname, "[.]")[[1]]))
  step = step[complete.cases(step)][1]
  if(file.exists(gsub('.cadj.viff', '.wgtcub', infile))){
    weightc = gsub('.cadj.viff', '.wgtcub', infile)
  }else{
    weightc = gsub('.cadj.viff', '.wgt.xv', infile) #for older wgt cube file
  }
  nunrf = gsub('.cadj.viff', '.nunr', infile)
  data.df = rbind(data.df, data.frame(steps = step, root_name = fname, 
            cadj_matrix = infile, weight_cube = weightc, nunr = nunrf))
  
  data.df
}



#' Load CONN matrix as graph object
#'
#' This function built a graph for the CONN matrix based on the related SOM products
#' accessing from directory link. 
#'
#' @param source.df The product data frame from func:load.parm_SOM.
#' @param method Set the graph type: "weighted": weighted graph/ "binary": binary graph
#' @returns A list consists of both the graph object and the related parameter data frame
#' @export
load.graph_SOM <- function(source.df, method = "weighted"){
  cadj = NeuroScopeIO::read_khoros_viff(filename = source.df$cadj_matrix, verbose = F)
  img_x = ncol(cadj); img_y = nrow(cadj)
  cadj = cadj[,,1]; conn = cadj + t(cadj)
  
  if(method == 'weighted'){
    gconn = igraph::graph_from_adjacency_matrix(conn, mode = "undirected", weighted = TRUE)
  }else if(method == 'binary'){
    conn = as.matrix((conn>0)+0)
    gconn = igraph::graph_from_adjacency_matrix(conn, mode = "undirected")
  }else{
    stop('Incorrect method.')
  }
  
  if(!is.null(source.df$weight_cube)){
    W = NeuroScopeIO::read_khoros_viff(filename = source.df$weight_cube, verbose = F)
    som_x = ncol(W); som_y = nrow(W)
    if(!is.null(source.df$nunr)){
      nunr = NeuroScopeIO::read_nunr(
        filename = source.df$nunr, som_x = som_x, som_y = som_y, img_x = img_x, img_y = img_y)
      classlabel = match(tolower(nunr$class), letters)
      classlabel[is.na(classlabel)] = dplyr::n_distinct(nunr$class) # NA class label will be assigned as the last number order
      igraph::V(gconn)$class = classlabel
    }
  }
  list(steps = source.df$steps, root = source.df$root_name, conn = conn, conn.graph = gconn)
}



