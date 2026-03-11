
#' Title
#'
#' @param df data frame with OSI data present
#' @param check_log check log from check NPX
#' @param osi_score OSI column to graph, one of OSISummary, 
#' OSITimeToCentrifugation, or OSIPreparationTemperature
#' @return distribution plot (histogram overlayed with density plot) of
#' osi values for corresponding osi_score column
#' @export
#'
#' @examples 
olink_osi_dist_plot <- function(df,
                                check_log = NULL,
                                osi_score = NULL){

  check_log <- run_check_npx(df, check_log = check_log)
  
  if(is.null(osi_score) || !(osi_score %in% c("OSISummary",
                        "OSITimeToCentrifugation",
                        "OSIPreparationTemperature"))){
    cli::cli_abort(paste0("`osi_score` must be one of OSISummary,",
                          " OSITimeToCentrifugation, ",
                          "or OSIPreparationTemperature."))
  }
  
  if (all(is.na(df[[osi_score]]))){
    cli::cli_abort(paste0(osi_score, " are all NA. ",
                          "Please check your data to confirm ",
                          "OSI data is present."))
  }
  if(any(is.na(df[[osi_score]]))){
    cli::cli_warn(paste("NA data detected in",
                        osi_score,
                        "Filtering out NA data."))
  }
  df1 <- df |> 
    dplyr::filter(!is.na(.data[[osi_score]]))
  
  
  p <- ggplot2::ggplot(df1, ggplot2::aes(x = .data[[osi_score]])) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)))+
    ggplot2::geom_density()
  
  return(p)
}