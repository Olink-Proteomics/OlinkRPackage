#'Function which performs a linear mixed model per protein
#'
#'Fits a linear mixed effects model for every protein (by OlinkID) using lmer. Model being fit will be NPX ~ var1 + var2 + var1:var2 + (1|ID)
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, 1-2 variables with at least 2 levels and subject ID.
#' @param var1 Character value indicating the name of the column to be used as the first variable in the LME model
#' @param var2 Character value indicating the name of the column to be used as the second variable in the LME model
#' @param ID Character value indicating the column of subject ID's. To be used as a random effect in the LME model.
#' @return A data frame containing the results of fitting the linear mixed effects model to every protein by OlinkID.
#' @export
#' @examples
#' \donttest{p_results <- olink_lmer_main(df, var1 = "Visit", var2 = "Treatment", ID = "SubjectID")}
#' @import dplyr stringr tidyr broom

olink_lmer_main <- function(df, var1, var2, ID) {

  withCallingHandlers({

    #Not testing assays that have all NA:s or all NA:s in one level
    all_nas <- df  %>%
      group_by(OlinkID) %>%
      summarise(n = n(), n_na = sum(is.na(NPX))) %>%
      ungroup() %>%
      filter(n == n_na) %>%
      pull(OlinkID)


    if(length(all_nas) > 0) {

      warning(paste0('The assays ',
                     paste(all_nas, collapse = ', '),
                     ' have only NA:s. They will not be tested.'),
              call. = F)

    }

    nas_in_var1 <- df  %>%
      filter(!(OlinkID %in% all_nas)) %>%
      group_by(OlinkID, !!rlang::ensym(var1)) %>%
      summarise(n = n(), n_na = sum(is.na(NPX))) %>%
      ungroup() %>%
      filter(n == n_na) %>%
      distinct(OlinkID) %>%
      pull(OlinkID)


    if(length(nas_in_var1) > 0) {

      warning(paste0('The assays ',
                     paste(nas_in_var1, collapse = ', '),
                     ' have only NA:s in atleast one level of ',
                     var1,
                     '. They will not be tested.'),
              call. = F)

    }



    if (missing(var2)) {
      ##make variable into factors
      df[[var1]] <- as.factor(df[[var1]])
      df[[ID]] <- as.factor(df[[ID]])

      ##make LMM
      lmer_model<-df %>%
        filter(!(OlinkID %in% all_nas)) %>%
        filter(!(OlinkID %in% nas_in_var1)) %>%
        group_by(Assay, OlinkID, UniProt, Panel) %>%
        do(tidy(anova(lmerTest::lmer(NPX~ !!rlang::ensym(var1) +(1|!!rlang::ensym(ID)),data=.,REML=FALSE)))) %>%
        ungroup() %>%
        mutate(Adjusted_pval = p.adjust(p.value,method="fdr")) %>%
        mutate(Threshold  = ifelse(Adjusted_pval<0.05,"Significant","Non-significant")) %>%
        arrange(p.value)

    } else {

      ##make variable into factors
      df[[var1]] <- as.factor(df[[var1]])
      df[[var2]] <- as.factor(df[[var2]])
      df[[ID]] <- as.factor(df[[ID]])


      nas_in_var2 <- df  %>%
        filter(!(OlinkID %in% all_nas)) %>%
        group_by(OlinkID, !!rlang::ensym(var2)) %>%
        summarise(n = n(), n_na = sum(is.na(NPX))) %>%
        ungroup() %>%
        filter(n == n_na) %>%
        distinct(OlinkID) %>%
        pull(OlinkID)


      if(length(nas_in_var2) > 0) {

        warning(paste0('The assays ',
                       paste(nas_in_var2, collapse = ', '),
                       ' have only NA:s in atleast one level of ',
                       var1,
                       '. They will not be tested.'),
                call. = F)

      }


      ##make LMM
      lmer_model<-df %>%
        filter(!(OlinkID %in% all_nas)) %>%
        filter(!(OlinkID %in% nas_in_var1)) %>%
        filter(!(OlinkID %in% nas_in_var2)) %>%
        group_by(Assay, OlinkID, UniProt, Panel) %>%
        do(tidy(anova(lmerTest::lmer(NPX~ !!rlang::ensym(var1) * !!rlang::ensym(var2) +(1|!!rlang::ensym(ID)),data=.,REML=FALSE)))) %>%
        ungroup() %>%
        mutate(Adjusted_pval = p.adjust(p.value,method="fdr")) %>%
        mutate(Threshold  = ifelse(Adjusted_pval<0.05,"Significant","Non-significant")) %>%
        arrange(p.value)
    }

    return(lmer_model)

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*not recognized or transformed: NumDF, DenDF*")))
      invokeRestart("muffleWarning")
  })
}

#'Function which perform a linear mixed model posthoc per significant protein
#'
#'Performs post-hoc analysis for proteins found to be statistically significant by the olink_lmer_main function. Differences in estimated marginal means are calculated for all pairwise levels of a given variable. Degrees of freedom estimated using Satterthwaite's approximation.
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, 1-2 variables with at least 2 levels and subject ID.
#' @param var1 Character value indicating the name of the column to be used as the first variable in the LME model
#' @param var2 Character value indicating the name of the column to be used as the second variable in the LME model
#' @param ID Character value indicating the column of subject ID's. To be used as a random effect in the LME model.
#' @param lmer_result Data frame containing the results of fitting the same model to the NPX data using the olink_lmer_main function.
#' @param variable Integer. Which variable on which the posthoc is performed. Must be either 1,2,3. 1 = var1, 2 = var2, 3 = interaction term.
#' @param mean.return Logical. Should the estimated marginal means for each group be returned (default) or the difference between them.
#' @return A data frame containing the results of the pairwise comparisons between given variable levels for proteins found to be statistically significant by olink_lmer_main.
#' @export
#' @examples
#' \donttest{lme_results <- olink_lmer_main(df=npx.data, var1 = "Visit", var2 = "Treatment", ID = "SubjectID")
#' results_lmer_posthoc <- olink_lmer_posthoc(df=npx.data, var1 = "Visit", var2 = "Treatment", ID = "SubjectID", lmer_result=lme_results, variable = 1, mean.return = T)}
#' @import dplyr stringr tidyr broom

olink_lmer_posthoc <- function(df, var1, var2, ID, lmer_result, variable = 3, mean.return=F) {

  if (missing(var2)) {
    df[[var1]] <- as.factor(df[[var1]])
    df[[ID]] <- as.factor(df[[ID]])

    sign_var <- lmer_result %>%
      filter(Threshold == "Significant" &
               term == var1) %>%
      pull(OlinkID)

    if(length(sign_var) == 0){
      stop('There are no significances. No posthoc will be performed.')
    }

    emmeans_models<-df %>%
      filter(OlinkID %in% sign_var) %>%
      group_by(Assay, OlinkID, UniProt, Panel) %>%
      do(model = emmeans::emmeans(lmerTest::lmer(NPX~ !!rlang::ensym(var1) +(1|!!rlang::ensym(ID)),
                                                 data=.,REML=F),pairwise ~ c(!!rlang::ensym(var1)),lmer.df="satterthwaite"))


    lm.means <- data_frame()
    lm.diff <- data_frame()

    for (i in 1:length(emmeans_models$OlinkID)) {
      in1 <- cbind(as.data.frame(emmeans_models[i,5]$model[[1]]$emmeans),
                   Assay=emmeans_models[i,1][[1]],
                   OlinkID=emmeans_models[i,2][[1]],
                   UniProt=emmeans_models[i,3][[1]],
                   Panel=emmeans_models[i,4][[1]])

      lm.means <- rbind(lm.means,in1)   ##Info for each group

      di1 <- cbind(as.data.frame(emmeans_models[i,5]$model[[1]]$contrasts),
                   Assay=emmeans_models[i,1][[1]],
                   OlinkID=emmeans_models[i,2][[1]],
                   UniProt=emmeans_models[i,3][[1]],
                   Panel=emmeans_models[i,4][[1]])

      lm.diff <- rbind(lm.diff,di1)     ##Info for each group comparison

    }
  } else{

    df[[var1]] <- as.factor(df[[var1]])
    df[[var2]] <- as.factor(df[[var2]])
    df[[ID]] <- as.factor(df[[ID]])


    if(variable == 1)  {
      sign_var <- lmer_result %>%
        filter(Threshold == "Significant" &
                 term == var1) %>%
        pull(OlinkID)

      if(length(sign_var) == 0){
        stop('There are no significances. No posthoc will be performed.')
      }

      emmeans_models<-df %>%
        filter(OlinkID %in% sign_var) %>%
        group_by(Assay, OlinkID, UniProt, Panel) %>%
        do(model = emmeans::emmeans(lmerTest::lmer(NPX~ !!rlang::ensym(var1) * !!rlang::ensym(var2) +(1|!!rlang::ensym(ID)),
                                                   data=.,REML=F),pairwise ~ c(!!rlang::ensym(var1)),lmer.df="satterthwaite"))

    }

    if(variable == 2)  {
      sign_var <- lmer_result %>%
        filter(Threshold == "Significant" &
                 term == var2) %>%
        pull(OlinkID)

      if(length(sign_var) == 0){
        stop('There are no significances. No posthoc will be performed.')
      }

      emmeans_models<-df %>%
        filter(OlinkID %in% sign_var) %>%
        group_by(Assay, OlinkID, UniProt, Panel) %>%
        do(model = emmeans::emmeans(lmerTest::lmer(NPX~ !!rlang::ensym(var1) * !!rlang::ensym(var2) +(1|!!rlang::ensym(ID)),
                                                   data=.,REML=F),pairwise ~ c(!!rlang::ensym(var2)),lmer.df="satterthwaite"))

    }

    if(variable == 3)  {
      sign_var <- lmer_result %>%
        filter(Threshold == "Significant" &
                 !(term == var1 | term == var2)) %>%
        pull(OlinkID)

      if(length(sign_var) == 0){
        stop('There are no significances. No posthoc will be performed.')
      }

      emmeans_models<-df %>%
        filter(OlinkID %in% sign_var) %>%
        group_by(Assay, OlinkID, UniProt, Panel) %>%
        do(model = emmeans::emmeans(lmerTest::lmer(NPX~ !!rlang::ensym(var1) * !!rlang::ensym(var2) +(1|!!rlang::ensym(ID)),
                                                   data=.,REML=F),pairwise ~ c(!!rlang::ensym(var1):!!rlang::ensym(var2)),lmer.df="satterthwaite"))

    }

    lm.means <- data_frame()
    lm.diff <- data_frame()

    for (i in 1:length(emmeans_models$OlinkID)) {
      in1 <- cbind(as.data.frame(emmeans_models[i,5]$model[[1]]$emmeans),
                   Assay=emmeans_models[i,1][[1]],
                   OlinkID=emmeans_models[i,2][[1]],
                   UniProt=emmeans_models[i,3][[1]],
                   Panel=emmeans_models[i,4][[1]])

      lm.means <- rbind(lm.means,in1)   ##Info for each group

      di1 <- cbind(as.data.frame(emmeans_models[i,5]$model[[1]]$contrasts),
                   Assay=emmeans_models[i,1][[1]],
                   OlinkID=emmeans_models[i,2][[1]],
                   UniProt=emmeans_models[i,3][[1]],
                   Panel=emmeans_models[i,4][[1]])

      lm.diff <- rbind(lm.diff,di1)     ##Info for each group comparison
    }
  }

  if(mean.return == T){

    return(lm.means%>%
             select(Assay, OlinkID, UniProt, Panel, everything()))

  } else {

    return(lm.diff %>%
             select(Assay, OlinkID, UniProt, Panel, everything()))
  }

}

#'Function which performs a linear mixed model plot per significant protein
#'
#'Creates an array of point-range plots for a given list of proteins (by OlinkID).
#'
#' @param df NPX data frame in long format with at least protein name, OlinkID, UniProt, 1-2 variables with at least 2 levels and subject ID.
#' @param var1 Character value indicating the name of the column to be used as the first variable in the LME model
#' @param var2 SCharacter value indicating the name of the column to be used as the second variable in the LME model
#' @param ID Character value indicating the column of subject ID's. To be used as a random effect in the LME model
#' @param olinkid_list Character vector indicating which proteins (by OlinkID) to create figures for
#' @param number_of_proteins_per_plot Number plots to include in the array of point-range plots. Defaults to 6 plots per figure
#' @return A list of objects of class "ggplot"
#' @export
#' @examples
#' \donttest{lme_results <- olink_lmer_main(df=npx.data, var1 = "Visit", var2 = "Treatment", ID = "SubjectID")
#'significant_proteins <- lme_results %>%
#'  filter(Threshold = 'Significant') %>%
#'  pull(OlinkID)
#' olink_lmer_plot(df=npx.data,  var1 = "Visit", var2 = "Treatment", ID = "Subject", olinkid_list = significant_proteins}
#' @import dplyr stringr tidyr broom

olink_lmer_plot <- function(df, var1, var2, ID, olinkid_list, number_of_proteins_per_plot = 6,...) {

  withCallingHandlers({

    df <- df %>%
      filter(OlinkID %in% olinkid_list)

    #Not testing assays that have all NA:s or all NA:s in one level
    all_nas <- df  %>%
      group_by(OlinkID) %>%
      summarise(n = n(), n_na = sum(is.na(NPX))) %>%
      ungroup() %>%
      filter(n == n_na) %>%
      pull(OlinkID)


    if(length(all_nas) > 0) {

      warning(paste0('The assays ',
                     paste(all_nas, collapse = ', '),
                     ' have only NA:s. They will not be tested.'),
              call. = F)

    }

    nas_in_var1 <- df  %>%
      filter(!(OlinkID %in% all_nas)) %>%
      group_by(OlinkID, !!rlang::ensym(var1)) %>%
      summarise(n = n(), n_na = sum(is.na(NPX))) %>%
      ungroup() %>%
      filter(n == n_na) %>%
      distinct(OlinkID) %>%
      pull(OlinkID)


    if(length(nas_in_var1) > 0) {

      warning(paste0('The assays ',
                     paste(nas_in_var1, collapse = ', '),
                     ' have only NA:s in atleast one level of ',
                     var1,
                     '. They will not be tested.'),
              call. = F)

    }




    if (missing(var2)) {

      df[[var1]] <- as.factor(df[[var1]])
      df[[ID]] <- as.factor(df[[ID]])

      assay_name_list<-df %>%
        filter(!(OlinkID %in% all_nas)) %>%
        filter(!(OlinkID %in% nas_in_var1)) %>%
        mutate(Name_Assay=paste0(Assay,"_",OlinkID)) %>%
        mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
        arrange(OlinkID) %>%
        pull(Name_Assay) %>% unique()

      emmeans_models<-df %>%
        filter(!(OlinkID %in% all_nas)) %>%
        filter(!(OlinkID %in% nas_in_var1)) %>%
        mutate(Name_Assay=paste0(Assay,"_",OlinkID)) %>%
        mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
        mutate(Name_Assay = factor(Name_Assay, levels = assay_name_list)) %>%
        group_by(Name_Assay) %>%
        do(model = emmeans::emmeans(lmerTest::lmer(NPX~ !!rlang::ensym(var1) +(1|!!rlang::ensym(ID)),
                                                   data=.,REML=F),pairwise ~ c(!!rlang::ensym(var1)),lmer.df="satterthwaite"))


      lm.means <- data_frame()

      for (i in 1:length(emmeans_models$Name_Assay)) {
        in1 <- cbind(as.data.frame(emmeans_models[i,2]$model[[1]]$emmeans),
                     Name_Assay=emmeans_models[i,1][[1]])

        lm.means <- rbind(lm.means,in1)   ##Info for each group
      }


      #Setup
      topX <- length(assay_name_list)

      protein_index <- seq(from = 1,
                           to = topX,
                           by = number_of_proteins_per_plot)

      list_of_plots <- list()
      COUNTER <- 1

      #loops
      for (i in c(1:length(protein_index))){


        from_protein <- protein_index[i]
        to_protein <- NULL

        if((protein_index[i] + number_of_proteins_per_plot) > topX){
          to_protein <- topX +1
        }else{
          to_protein <- protein_index[i+1]
        }

        assays_for_plotting <- assay_name_list[c(from_protein:(to_protein-1))]


        lmerplot<-lm.means %>%
          filter(Name_Assay %in% assays_for_plotting) %>%
          ggplot()+
          theme(axis.title.x=element_blank())+
          ylab("NPX")+
          theme(axis.text.x =  element_text(size = 10))+
          geom_pointrange(aes(x=!!rlang::ensym(var1),y=emmean,ymin=lower.CL,ymax=upper.CL,color = !!rlang::ensym(var1)),
                          position = position_dodge(width=0.4), size=0.8)+
          facet_wrap(~ Name_Assay,scales = "free_y")+
          theme(strip.text.x = element_text(size = 13))+
          theme_bw()+  ##White background
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ ##Remove grid
          theme(panel.border = element_blank(), axis.line = element_line(size = 0.5))+ #Remove border, add x and y line
          theme(strip.background  = element_rect(fill="white"),
                strip.text = element_text(size=8),
                strip.text.x = element_text(size = 13))

        show(lmerplot)

        list_of_plots[[COUNTER]] <- lmerplot
        COUNTER <- COUNTER + 1
      }

    } else{

      df[[var1]] <- as.factor(df[[var1]])
      df[[var2]] <- as.factor(df[[var2]])
      df[[ID]] <- as.factor(df[[ID]])

      nas_in_var2 <- df  %>%
        filter(!(OlinkID %in% all_nas)) %>%
        group_by(OlinkID, !!rlang::ensym(var2)) %>%
        summarise(n = n(), n_na = sum(is.na(NPX))) %>%
        ungroup() %>%
        filter(n == n_na) %>%
        distinct(OlinkID) %>%
        pull(OlinkID)


      if(length(nas_in_var2) > 0) {

        warning(paste0('The assays ',
                       paste(nas_in_var2, collapse = ', '),
                       ' have only NA:s in atleast one level of ',
                       var1,
                       '. They will not be tested.'),
                call. = F)

      }

      assay_name_list<-df %>%
        filter(!(OlinkID %in% all_nas)) %>%
        filter(!(OlinkID %in% nas_in_var1)) %>%
        filter(!(OlinkID %in% nas_in_var2)) %>%
        mutate(Name_Assay=paste0(Assay,"_",OlinkID)) %>%
        mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
        arrange(OlinkID) %>%
        pull(Name_Assay) %>% unique()


      emmeans_models<-df %>%
        filter(!(OlinkID %in% all_nas)) %>%
        filter(!(OlinkID %in% nas_in_var1)) %>%
        filter(!(OlinkID %in% nas_in_var2)) %>%
        mutate(Name_Assay=paste0(Assay,"_",OlinkID)) %>%
        mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
        mutate(Name_Assay = factor(Name_Assay, levels = assay_name_list)) %>%
        group_by(Name_Assay) %>%
        do(model = emmeans::emmeans(lmerTest::lmer(NPX~ !!rlang::ensym(var1) * !!rlang::ensym(var2) +(1|!!rlang::ensym(ID)),
                                                   data=.,REML=F),pairwise ~ c(!!rlang::ensym(var1):!!rlang::ensym(var2)),lmer.df="satterthwaite"))


      lm.means <- data_frame()

      for (i in 1:length(emmeans_models$Name_Assay)) {
        in1 <- cbind(as.data.frame(emmeans_models[i,2]$model[[1]]$emmeans),
                     Name_Assay=emmeans_models[i,1][[1]])

        lm.means <- rbind(lm.means,in1)   ##Info for each group
      }

      #Setup
      topX <- length(assay_name_list)

      protein_index <- seq(from = 1,
                           to = topX,
                           by = number_of_proteins_per_plot)

      list_of_plots <- list()
      COUNTER <- 1

      #loops
      for (i in c(1:length(protein_index))){


        from_protein <- protein_index[i]
        to_protein <- NULL

        if((protein_index[i] + number_of_proteins_per_plot) > topX){
          to_protein <- topX +1
        }else{
          to_protein <- protein_index[i+1]
        }

        assays_for_plotting <- assay_name_list[c(from_protein:(to_protein-1))]



        lmerplot<-lm.means %>%
          filter(Name_Assay %in% assays_for_plotting) %>%
          ggplot()+
          theme(axis.title.x=element_blank())+
          ylab("NPX")+
          theme(axis.text.x =  element_text(size = 10))+
          geom_pointrange(aes(x=!!rlang::ensym(var1),y=emmean,ymin=lower.CL,ymax=upper.CL, color = !!rlang::ensym(var2)),
                          position = position_dodge(width=0.4), size=0.8)+
          facet_wrap(~ Name_Assay,scales = "free_y")+
          theme(strip.text.x = element_text(size = 13))+
          theme_bw()+  ##White background
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ ##Remove grid
          theme(panel.border = element_blank(), axis.line = element_line(size = 0.5))+ #Remove border, add x and y line
          theme(strip.background  = element_rect(fill="white"),
                strip.text = element_text(size=8),
                strip.text.x = element_text(size = 13))

        show(lmerplot)

        list_of_plots[[COUNTER]] <- lmerplot
        COUNTER <- COUNTER + 1
      }
    }

    return(invisible(list_of_plots))

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*not recognized or transformed: NumDF, DenDF*")))
      invokeRestart("muffleWarning")
  })

}
