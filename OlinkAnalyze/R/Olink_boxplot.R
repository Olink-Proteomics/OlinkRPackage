#'Function which plots boxplots of selected variables
#'
#'Generates faceted boxplots of NPX vs. grouping variable(s) for a given list of proteins (OlinkIDs) using ggplot and ggplot2::geom_boxplot.
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID (unique), UniProt and at least one grouping variable.
#' @param variable  A character vector or character value indicating which column to use as the x-axis and fill grouping variable. 
#' The first or single value is used as x-axis, the second as fill. Further values in a vector are not plotted.
#' @param olinkid_list Character vector indicating which proteins (OlinkIDs) to plot.
#' @param number_of_proteins_per_plot Number of boxplots to include in the facet plot (default 6).
#' @param verbose Boolean. If the plots are shown as well as returned in the list (default is false).
#' @param ... coloroption passed to specify color order
#'
#' @return A list of objects of class “ggplot” (the actual ggplot object is entry 1 in the list).
#' @export
#' @examples
#' \donttest{
#' anova_results <- olink_anova(npx_data1, variable = "Site")
#' significant_assays <- anova_results %>%
#'     filter(Threshold == 'Significant') %>%
#'     pull(OlinkID)
#' olink_boxplot(npx_data1,
#'               variable = "Site",
#'               olinkid_list = significant_assays,
#'               verbose = TRUE,
#'               number_of_proteins_per_plot = 3)}

olink_boxplot <- function(df,
                          variable,
                          olinkid_list,
                          verbose = F,
                          number_of_proteins_per_plot = 6,
                          ...){

  #checking ellipsis
  if(length(list(...)) > 0){

    ellipsis_variables <- names(list(...))

    if(length(ellipsis_variables) == 1){

      if(!(ellipsis_variables == 'coloroption')){

        stop(paste0('The ... option only takes the coloroption argument. ... currently contains the variable ',
                    ellipsis_variables,
                    '.'))

      }

    }else{

      stop(paste0('The ... option only takes one argument. ... currently contains the variables ',
                  paste(ellipsis_variables, collapse = ', '),
                  '.'))
    }
  }


  #Filtering on valid OlinkID
  df <- df %>%
    filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))

  #Column setup
  columns_for_npx_data <- c("OlinkID","UniProt","Assay", "NPX", eval(variable))

  #Testing that needed columns are correct
  if(!(all(columns_for_npx_data %in% colnames(df)))){


    stop(paste0("Column(s) ",
                paste(setdiff(columns_for_npx_data,
                              colnames(df)),
                      collapse=", "),
                " not found in NPX data frame!"))

  }
  
  if (length(variable) > 2){
    warning(paste0("Variable(s) ", 
                   paste(setdiff(variable, variable[1:2]), collapse = ", "), 
                   " will not be used for plotting."))
  }
  
  #Setup
  x_variable <- rlang::syms(variable[1])
  if(length(variable) > 1){
    fill_variable <- rlang::syms(variable[2])
  }else{
    fill_variable <- x_variable
  }  

  topX <- length(olinkid_list)

  protein_index <- seq(from = 1,
                       to = topX,
                       by = number_of_proteins_per_plot)

  list_of_plots <- list()
  COUNTER <- 1

  for (i in c(1:length(protein_index))){

    #setting indeces

    from_protein <- protein_index[i]
    to_protein <- NULL

    if((protein_index[i] + number_of_proteins_per_plot) > topX){
      to_protein <- topX +1
    }else{
      to_protein <- protein_index[i+1]
    }

    assays_for_plotting <- olinkid_list[c(from_protein:(to_protein-1))]


    npx_for_plotting <- df %>%
      filter(OlinkID %in% assays_for_plotting) %>%
      mutate(OlinkID = factor(OlinkID, levels = assays_for_plotting)) %>%
      select(OlinkID, UniProt, Assay, NPX, eval(variable)) %>%
      with(., .[order(OlinkID),]) %>%
      unite(c(Assay, OlinkID), col = 'Name_OID', sep = ' ', remove = F) %>%
      mutate(Name_OID = as_factor(Name_OID))


    boxplot <- npx_for_plotting %>% 
      ggplot(aes(y = NPX, x = !!x_variable[[1]])) + 
      geom_boxplot(aes(fill = !!fill_variable[[1]])) +
      set_plot_theme() +
      olink_fill_discrete(...)+
      theme(axis.ticks.x = element_blank(),
            legend.text=element_text(size=13)) +
      facet_wrap(~Name_OID, scales = "free")
    
    if(length(variable) == 1){
      boxplot <- boxplot + 
        theme(axis.text.x = element_blank(), 
              legend.title = element_blank())
    }
    
    if(verbose){
      show(boxplot)
    }

    list_of_plots[[COUNTER]] <- boxplot
    COUNTER <- COUNTER + 1

  }

  return(invisible(list_of_plots))

}
