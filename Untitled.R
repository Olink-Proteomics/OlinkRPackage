anova_results_oneway <- olink_anova(df = npx_data1,
                                   variable = 'Site')
# extracting the significant proteins
anova_results_oneway_significant <- anova_results_oneway %>%
 filter(Threshold == 'Significant') %>%
 pull(OlinkID)

anova_posthoc_oneway_results <- olink_anova_posthoc(df = npx_data1,
                                                   olinkid_list = anova_results_oneway_significant,
                                                   variable = 'Site',
                                                   effect = 'Site')


verbose = FALSE

number_of_proteins_per_plot = 6

olinkid_list = anova_results_oneway_significant

olink_boxplot <- function(df,
                          variable,
                          olinkid_list,
                          verbose = FALSE,
                          number_of_proteins_per_plot = 6,
                          posthoc_results,
                          ttest_results,
                          ...){
  
  myRound <- function(x){
    if(x >= .00009){
      return(as.character(round(x,4)))
    }else{
      out <- as.character(x)
      if(nchar(out)>8){
        out <- paste0(substring(out,1,4),substring(out,nchar(out)-3,nchar(out)))
      }
      return(return(out))
    }
  }
  
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
    dplyr::filter(stringr::str_detect(OlinkID,
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
      dplyr::filter(OlinkID %in% assays_for_plotting) %>%
      dplyr::mutate(OlinkID = factor(OlinkID, levels = assays_for_plotting)) %>%
      dplyr::select(OlinkID, UniProt, Assay, NPX, eval(variable)) %>%
      with(., .[order(OlinkID),]) %>%
      tidyr::unite(c(Assay, OlinkID), col = 'Name_OID', sep = ' ', remove = FALSE) %>%
      dplyr::mutate(Name_OID = forcats::as_factor(Name_OID))
    
    if(is.null(posthoc_results) && is.null(ttest_results)) {
      boxplot <- npx_for_plotting %>%
        ggplot2::ggplot(ggplot2::aes(y = NPX,
                                     !!x_variable[[1]])) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_variable[[1]])) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...)+
        ggplot2::theme(axis.ticks.x = element_blank(),
                       legend.text=element_text(size=13)) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
    } else if (!is.null(posthoc_results) && is.null(ttest_results)){
      star.info <- data.frame(x.vals = levels(npx_for_plotting %>% pull(eval(variable)) %>% addNA()),
                              id = 1:length(levels(npx_for_plotting %>% pull(eval(variable)) %>%addNA()))) %>%
        mutate(x.vals=replace(x.vals, is.na(x.vals), "NA"))
      
      posthoc.results_temp <- posthoc_results %>% filter(OlinkID %in% assays_for_plotting) %>% 
        tidyr::unite(c(Assay, OlinkID), col = 'Name_OID', sep = ' ', remove = FALSE) %>%
        dplyr::mutate(Name_OID = forcats::as_factor(Name_OID))
      
      scale_inf <- npx_for_plotting %>% group_by(Name_OID) %>% summarise(maxNPX = max(NPX), rangeNPX = diff(range(NPX))) %>% ungroup()
      
      line.data <- posthoc.results_temp %>% left_join(scale_inf, by = "Name_OID") %>% 
        mutate(C1=sapply(strsplit(contrast," - "),function(x) x[1]),
               C2=sapply(strsplit(contrast," - "),function(x) x[2])) %>%
        group_by(Name_OID, contrast) %>% 
        mutate(c.sort=min(C1,C2)) %>% 
        mutate(p.value=paste0(myRound(Adjusted_pval)," Contrast: ", contrast)) %>% 
        ungroup() %>% 
        group_by(Name_OID) %>% 
        arrange(c.sort) %>% 
        mutate(rowNum=n():1) %>% 
        ungroup() %>% 
        mutate(y.anchor = maxNPX + rowNum * rangeNPX *(.5)/max(rowNum)) %>%
        select(Name_OID,contrast,Adjusted_pval,C1,C2,p.value,Threshold,c.sort,y.anchor) %>% 
        pivot_longer(-c(Name_OID,Threshold,contrast,Adjusted_pval,p.value,c.sort,y.anchor),names_to="tmp",values_to = "x.vals") %>% 
        mutate(Star = case_when(Adjusted_pval <0.05 & Adjusted_pval > 0.01 ~ "*",
                                Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
                                Adjusted_pval <= 0.005  ~ "***",
                                Adjusted_pval >= 0.05 ~ NA_character_)) %>%
        left_join(star.info, by = "x.vals") %>% 
        group_by(contrast,Name_OID) %>% mutate(x.m = sum(id)/2) %>% ungroup() %>% 
        filter(Threshold=="Significant") 
      
      boxplot <- npx_for_plotting %>%
        ggplot(aes(y = NPX,
                   x = !!rlang::ensym(variable))) +
        geom_boxplot(aes(fill = !!rlang::ensym(variable))) +
        
        geom_line(data=line.data, aes(x=x.vals,y=y.anchor,group=p.value)) +
        geom_text(data=line.data %>% filter(tmp == "C1"),aes(group=p.value, x=x.m, y=y.anchor+0.1, label = Star)) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(axis.ticks.x = element_blank(),
                     legend.text=element_text(size=13)) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
    } else if (is.null(posthoc_results) && !is.null(ttest_results)){
      
    }
      
      
      
      
      
    
    star.info <- data.frame(x.vals = levels(npx_for_plotting %>% pull(eval(variable)) %>% addNA()),
                            id = 1:length(levels(npx_for_plotting %>% pull(eval(variable)) %>%addNA()))) %>%
      mutate(x.vals=replace(x.vals, is.na(x.vals), "NA"))
    
    posthoc.results_temp <- posthoc_results %>% filter(OlinkID %in% assays_for_plotting) %>% 
      tidyr::unite(c(Assay, OlinkID), col = 'Name_OID', sep = ' ', remove = FALSE) %>%
      dplyr::mutate(Name_OID = forcats::as_factor(Name_OID))
    
    scale_inf <- npx_for_plotting %>% group_by(Name_OID) %>% summarise(maxNPX = max(NPX), rangeNPX = diff(range(NPX))) %>% ungroup()
    
    line.data <- posthoc.results_temp %>% left_join(scale_inf, by = "Name_OID") %>% 
      mutate(C1=sapply(strsplit(contrast," - "),function(x) x[1]),
             C2=sapply(strsplit(contrast," - "),function(x) x[2])) %>%
      group_by(Name_OID, contrast) %>% 
      mutate(c.sort=min(C1,C2)) %>% 
      mutate(p.value=paste0(myRound(Adjusted_pval)," Contrast: ", contrast)) %>% 
      ungroup() %>% 
      group_by(Name_OID) %>% 
      arrange(c.sort) %>% 
      mutate(rowNum=n():1) %>% 
      ungroup() %>% 
      mutate(y.anchor = maxNPX + rowNum * rangeNPX *(.5)/max(rowNum)) %>%
      select(Name_OID,contrast,Adjusted_pval,C1,C2,p.value,Threshold,c.sort,y.anchor) %>% 
      pivot_longer(-c(Name_OID,Threshold,contrast,Adjusted_pval,p.value,c.sort,y.anchor),names_to="tmp",values_to = "x.vals") %>% 
      mutate(Star = case_when(Adjusted_pval <0.05 & Adjusted_pval > 0.01 ~ "*",
                              Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
                              Adjusted_pval <= 0.005  ~ "***",
                              Adjusted_pval >= 0.05 ~ NA_character_)) %>%
      left_join(star.info, by = "x.vals") %>% 
      group_by(contrast,Name_OID) %>% mutate(x.m = sum(id)/2) %>% ungroup() %>% 
      filter(Threshold=="Significant") 
    
    
    
    boxplot <- npx_for_plotting %>%
      ggplot(aes(y = NPX,
                 x = !!rlang::ensym(variable))) +
      geom_boxplot(aes(fill = !!rlang::ensym(variable))) +
      
      geom_line(data=line.data, aes(x=x.vals,y=y.anchor,group=p.value))+
      geom_text(data=line.data %>% filter(tmp == "C1"),aes(group=p.value, x=x.m, y=y.anchor+0.1, label = Star)) +
      OlinkAnalyze::set_plot_theme()+
      OlinkAnalyze::olink_fill_discrete(...)
      ggplot2::theme(axis.ticks.x = element_blank(),
                     legend.text=element_text(size=13)) +
      ggplot2::facet_wrap(~Name_OID, scales = "free")
    
    # boxplot <- npx_for_plotting %>%
    #   ggplot2::ggplot(ggplot2::aes(y = NPX,
    #                                !!x_variable[[1]])) +
    #   ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_variable[[1]])) +
    #   OlinkAnalyze::set_plot_theme() +
    #   OlinkAnalyze::olink_fill_discrete(...)+
    #   ggplot2::theme(axis.ticks.x = element_blank(),
    #                  legend.text=element_text(size=13)) +
    #   ggplot2::facet_wrap(~Name_OID, scales = "free")
    
    
    if(length(variable) == 1){
      boxplot <- boxplot +
        ggplot2::theme(axis.text.x = element_blank(),
                       legend.title = element_blank())
    }
    
    if(verbose){
      methods::show(boxplot)
    }
    
    list_of_plots[[COUNTER]] <- boxplot
    COUNTER <- COUNTER + 1
    
  }
  
  return(invisible(list_of_plots))
  
}


#' olink_boxplot_annova(npx_data1,
#'                     variable="Site",
#'                     olinkid_list=anova_results_oneway_significant,
#'                     posthoc.results= anova_posthoc_oneway_results,
#'                     verbose = F)}
re <- olink_boxplot(npx_data1,
                    variable="Site",
                    olinkid_list=anova_results_oneway_significant,
                    posthoc_results= anova_posthoc_oneway_results,
                    verbose = F)



