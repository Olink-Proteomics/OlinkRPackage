#'Function which plots boxplots of a selected variable
#'
#'Creates an array of boxplots with grouping variable on the x-axis and NPX values on the y-axis for a given list of proteins (by OlinkID).
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID (unique), UniProt and a grouping variable.
#' @param variable  A character value indiciating which column to use as the x-axis grouping variable
#' @param olinkid_list Character vector indicating for which OlinkID's to create figures
#' @param number_of_proteins_per_plot Number boxplots to include in the array of boxplots. (defaults 6 plots per figure)
#' @return A list of objects of class "ggplot"
#' @export
#' @examples
#' \donttest{anova_results <- anova_olink(df, variable = "Var1")
#' significant_assays <- anova_results %>%
#' filter(Threshold = 'Significant') %>%
#' pull(OlinkID)
#' olink_boxplot(df, variable = "Var1", olinkid_list = significant_assay, number_of_proteins_per_plot = 3)}

olink_boxplot <- function(df, variable, olinkid_list, number_of_proteins_per_plot = 6){

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


  #Setup
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
      ggplot(aes(y = NPX,
                 x = !!rlang::ensym(variable))) +
      geom_boxplot(aes(fill = !!rlang::ensym(variable))) +
      set_olink_theme() +
      theme(axis.text.x = element_blank(),
            legend.title = element_blank(),
            axis.ticks.x = element_blank(),
            legend.text=element_text(size=13)) +
      facet_wrap(~Name_OID, scales = "free")

    show(boxplot)


    list_of_plots[[COUNTER]] <- boxplot
    COUNTER <- COUNTER + 1

  }

  return(invisible(list_of_plots))

}




