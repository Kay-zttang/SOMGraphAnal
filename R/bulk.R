#' Bulk load parameter table.
#'
#' This function search realated possible wwightcube .wgtcub/.wgt.xv file and 
#' the class label .nunr file for the input list of CONN .cadj file for each 
#' learning steps. And integrated as an output dataframe.
#' 
#' @param infile Path to the input .parm/.txt file with the directories of .cadj file
#' @return A data frame with each row showing the current learning steps and 
#' directories of related SOM files.
#' @export
bulk.parm_SOM <- function(infile){
  inputs = as.vector(read.table(infile)$V1)
  data.df = data.frame(matrix(ncol = 5, nrow = 0))
  colnames(data.df) <- c('steps', 'root_name', 'cadj_matrix', 'weight_cube', 'nunr')
  
  for (input in inputs){
    fname = sub('\\.cadj.viff$','',tail(strsplit(input, "/")[[1]],n=1))
    step = suppressWarnings(as.numeric(strsplit(fname, "[.]")[[1]]))
    step = step[complete.cases(step)][1]
    if(file.exists(gsub('.cadj.viff', '.wgtcub', input))){
      weightc = gsub('.cadj.viff', '.wgtcub', input)
    }else{
      weightc = gsub('.cadj.viff', '.wgt.xv', input) #for older wgt cube file
    }
    nunrf = gsub('.cadj.viff', '.nunr', input)
    data.df = rbind(data.df, data.frame(steps = step, root_name = fname, 
                                        cadj_matrix = input, weight_cube = weightc, nunr = nunrf))
  }
  data.df
}

#' Bulk load parameter table.
#'
#' This function search realated possible wwightcube .wgtcub/.wgt.xv file and 
#' the class label .nunr file for the input list of CONN .cadj file for each 
#' learning steps. And integrated as an output dataframe.
#' 
#' @param infile Path to the input .parm/.txt file with the directories of .cadj file
#' @return A data frame with each row showing the current learning steps and 
#' directories of related SOM files.
#' @export
bulk.calc_SOM <- function(source.df, supervised = FALSE, save_to.excel = FALSE, method = "allPEs"){
  wb <- openxlsx::createWorkbook()
  degree.df = data.frame()
  eigenvalue.df = data.frame()
  property.df = data.frame()
  
  for (i in 1:nrow(source.df)){
    cur.graph = SOMGraphAnal::load.graph_SOM(source.df[i,])
    cur.res = SOMGraphAnal::calc.graph_property(cur.graph, supervised = supervised, save_to.excel = F, method = method)
    if(nrow(property.df)==0){
      property.df = data.frame(matrix(nrow = nrow(cur.res$graph.property), ncol=0))
      property.df = cbind(property.df, cur.res$graph.property)
    }else{
      property.df = cbind(property.df, cur.res$graph.property)
    }
    if(nrow(degree.df)==0){
        degree.df = data.frame(matrix(nrow = nrow(cur.res$graph.degree), ncol=0))
        degree.df = cbind(degree.df, cur.res$graph.degree)
    }else{
        degree.df = cbind(degree.df, cur.res$graph.degree)
    }
    if(nrow(eigenvalue.df)==0){
      eigenvalue.df = data.frame(matrix(nrow = nrow(cur.res$graph.eigenvalue), ncol=0))
      eigenvalue.df = cbind(eigenvalue.df, cur.res$graph.eigenvalue)
    }else{
      eigenvalue.df = cbind(eigenvalue.df, cur.res$graph.eigenvalue)
    }

  }
  
  name = head(strsplit(source.df$root_name[1], "[.]")[[1]], 1)
  dir = paste0('graph_measurements.',name, ".", format(Sys.Date(), "%d%b%Y"),'.xlsx')
  
  property.out <- tibble::rownames_to_column(property.df, " ") 
  degree.out <- tibble::rownames_to_column(degree.df, " ") 
  eigen.out <- tibble::rownames_to_column(eigenvalue.df, " ") 
  
  if (save_to.excel) {
    if(method == 'allPEs'){
      savefile = list("Properties" = property.out, "Eigenvalues" = eigen.out, "Degrees" = degree.out)
    }else{
      savefile = list("Properties_active" = property.out, "Eigenvalues_active" = eigen.out, "Degrees_active" = degree.out)
    }
    openxlsx::write.xlsx(savefile, dir, colnames = T,rownames = T)
 }
  
  list(graph.property = property.df, graph.degree = degree.df, graph.eigenvalue = eigenvalue.df)
} 


