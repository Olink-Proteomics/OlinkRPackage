#' Plot all plates colored by a variable
#'
#' Displays each plate in a facet with cells colored by the given variable using ggplot and ggplot2::geom_tile.
#' @param data tibble/data frame in long format returned from the olink_plate_randomizer function.
#' @param fill.color Column name to be used as coloring variable for wells.
#' @param include.label Should the variable group be shown in the plot.
#' @keywords randomized plates, ggplot
#' @export
#' @examples
#' \donttest{randomized.manifest <- olink_plate_randomizer(manifest)}
#' \donttest{displayPlateLayout(data=randomized.manifest,fill.color="Site")}
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom ggplot2 ggplot geom_tile facet_wrap scale_fill_manual labs scale_x_discrete geom_text

displayPlateLayout <- function(data,fill.color,include.label=F){

  missing.spots <- expand.grid(plate=unique(data$plate),
                               row=LETTERS[1:8],
                               column=paste("Column",1:11),fill.color="Empty")
  missing.spots$unique.id <- paste(missing.spots$plate,missing.spots$row,missing.spots$column)
  missing.spots <- missing.spots %>%
    dplyr::filter(!unique.id %in% paste(data$plate,data$row,data$column)) %>%
    dplyr::select(-unique.id)

  if(missing(fill.color)) fill.color <- "plate"

  data$fill.color <- data[[fill.color]]
  data <- data %>%
    dplyr::select(fill.color,plate,row,column,fill.color) %>%
    rbind(missing.spots) %>%
    dplyr::mutate(row=factor(row,levels=LETTERS[8:1]),
           column=factor(column,levels=paste("Column",1:11)),
           fill.color=factor(fill.color))

  fill.levels <- levels(data$fill.color)
  if("Empty" %in% fill.levels){
    hld <- which(fill.levels=="Empty")
    fills <- rep(NA,length(fill.levels))
    fills[-hld] <- olink_pal()(length(fill.levels)-1)
    fills[hld] <- "#ffffff"
  }else{
    fills <- olink_pal()(length(fill.levels))
  }


  p <- ggplot2::ggplot(ggplot2::aes(x=column,y=row,fill=fill.color),data=data)+
    ggplot2::geom_tile(color="black")+
    ggplot2::facet_wrap(~plate,ncol=1,scales="fixed")+
    set_plot_theme()+
    ggplot2::scale_fill_manual(values=fills)+
    ggplot2::labs(x="",y="",fill=fill.color)+
    ggplot2::scale_x_discrete(labels=paste0("Col",1:11))

  if(include.label){
    return(p+ggplot2::geom_text(ggplot2::aes(label=fill.color),color="black"))
  }else{
    return(p)
  }

}


#' Plot distributions of a given variable for all plates
#'
#' Displays a bar chart for each plate representing the distribution of the given grouping variable on each plate using ggplot2::ggplot and ggplot2::geom_bar.
#' @param data tibble/data frame in long format returned from the olink_plate_randomizer function.
#' @param fill.color Column name to be used as coloring variable for wells.
#' @keywords randomized plates, ggplot
#' @export
#' @examples
#' \donttest{randomized.manifest <- olink_plate_randomizer(manifest)}
#' \donttest{displayPlateDistributions(data=randomized.manifest,fill.color="Site")}
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by tally ungroup mutate summarize as_tibble arrange
#' @importFrom ggplot2 ggplot aes theme labs


displayPlateDistributions <- function(data,fill.color){

  data$group.var <- data[[fill.color]]

  p1 <- data %>%
    dplyr::group_by(plate,group.var) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(plate) %>%
    dplyr::mutate(percent=100*n/sum(n)) %>%
    ggplot2::ggplot(ggplot2::aes(x=plate,y=percent,fill=group.var)) +
    ggplot2::geom_bar(stat="identity",color="gray")+
    olink_fill_discrete()+
    set_plot_theme() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust=0,vjust=0.5))+
    ggplot2::labs(fill=fill.color,x="Plate",y="Percent")+
    ggplot2::theme(legend.position = "bottom")

  return(p1)

}


assignSubject2Plate <- function(plateMap,manifest,SubjectID){

  samp.ids <- manifest$SampleID[manifest$SubjectID==SubjectID]

  spots.available <- plateMap %>%
    dplyr::group_by(plate) %>%
    dplyr::summarize(available=sum(is.na(SampleID)))

  if(any(spots.available$available>=length(samp.ids))){
    plate.assign <- sample(spots.available$plate[spots.available$available>=length(samp.ids)],1)
  } else{
    return("This Sample does not fit!")
  }

  placement <- which(plateMap$plate==plate.assign & is.na(plateMap$SampleID))[1:length(samp.ids)]
  plateMap$SampleID[placement] <- samp.ids
  return(dplyr::as_tibble(plateMap))

}

generatePlateHolder <- function(n.plates,n.spots,n.samples){
  if(n.plates!=length(n.spots)) stop("Vector of available spots must equal number of plates!")
  if(any(n.spots>88)) stop("Number of samples per plates cannot exceed 88!")
  if(sum(n.spots)<n.samples) stop("More samples than available spots! Double check your numbers!")
  full.row.col <- expand.grid(column=paste0("Column ",1:11),
                              row=LETTERS[1:8]) %>%
    dplyr::arrange(column,row)
  plates <- paste0("Plate ",1:n.plates)
  out <- data.frame(plate=NULL,column=NULL,row=NULL,stringsAsFactors = F)
  for(i in 1:n.plates){
    hld <- cbind(plate=rep(paste0("Plate ",i),n.spots[i]),
                 full.row.col[1:n.spots[i],])
    out <- rbind(out,hld)
  }
  return(out)
}



#' Randomly assign samples to plates
#'
#' Samples can be randomly assigned to plates using base::sample with an option to keep Subjects on the same plate.  DS no longer recommends forced balanced randomization considering other clinical variables. Contact DS with any questions.
#' @param Manifest tibble/data frame in long format containing all sample ID's. Sample ID column must be named SampleID.
#' @param SubjectColumn (Optional) Column name of the subject ID column. Cannot contain missings. If provided, subjects are kept on the same plate.
#' @param Groups (Optional) NO LONGER IMPLEMENTED. Vector of column names for variables to be balanced across plates.
#' @param iterations Number of iterations for fitting subjects on the same plate.
#' @param available.spots Default is 88. Number of wells available on each plate. Can also take a vector equal to the number of plates to be used indicating the number of wells available on each plate.
#' @param seed Seed to set. Highly recommend setting this for reproducibility.
#' @return Tibble including SampleID, SubjectID etc. assigned to well positions.
#' @keywords randomized plates
#' @export
#' @examples
#' \donttest{randomized.manifest <- olink_plate_randomizer(manifest, seed=12345)}
#' \donttest{randomized.manifest <- olink_plate_randomizer(manifest,SubjectColumn="SubjectID",
#'                                                         available.spots=c(88,88), seed=12345)}
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble mutate arrange left_join group_by ungroup select

#Main randomization function
olink_plate_randomizer <-function(Manifest, SubjectColumn, Groups, iterations=500, available.spots, seed){

  if(!"SampleID" %in% colnames(Manifest)) {
    stop("SampleID not found! Be sure the column of samples ID's is named 'SampleID'")
  }

  if(any(is.na(Manifest$SampleID))) {
    stop("No NA allowed in the SampleID column. Check that all the samples are named.")
  }

  if(!missing(seed)) {
    set.seed(seed)
  }

  if(!missing(SubjectColumn)){
    Manifest$SubjectID <- Manifest[[SubjectColumn]]

  }

  if(any(is.na(Manifest$SubjectID))) {

    stop("No NA allowed in the SubjectID column. Check that all the subjects are named.")
  }

  if(missing(available.spots)){
    PlatesNeeded <-ceiling(nrow(Manifest)/88)
    all.plates <- generatePlateHolder(PlatesNeeded,rep(88,times=PlatesNeeded),n.samples=length(Manifest$SampleID))
  } else{
    all.plates <- generatePlateHolder(length(available.spots),available.spots,n.samples=length(Manifest$SampleID))
  }

  if(!missing(Groups)){
    stop("Data science no longer recommends forcing plate balancing based on groups. Contact DS with any questions.")
  }

  #Complete Random if subjectID not given
  if(missing(SubjectColumn)){
    all.plates <- all.plates[sample(1:nrow(Manifest)),]
    out.manifest <- dplyr::as_tibble(cbind(Manifest,all.plates)) %>%
      dplyr::mutate(well=paste0(row,gsub("Column ","",as.character(column)))) %>%
      dplyr::mutate(well=factor(well,levels=paste0(rep(LETTERS[1:8],each=12),rep(1:12,times=8)))) %>%
      dplyr::arrange(plate, column, row)
    cat("Random assignment of SAMPLES to plates\n")
    class(out.manifest) <- c("randomizedManifest",class(out.manifest))
    return(out.manifest)
  }


  ##Keep subjects together
  if(!missing(SubjectColumn)){
    cat("Assigning subjects to plates\n")
    for(i in 1:iterations){
      cat(".")
      all.plates$SampleID <- NA
      rand.subjects <- sample(unique(Manifest$SubjectID))


      for(sub in rand.subjects){
        all.plates.tmp <- assignSubject2Plate(plateMap=all.plates,
                                              manifest=Manifest,
                                              SubjectID=sub)
        if(is_tibble(all.plates.tmp)){
          all.plates <- all.plates.tmp
        } else if(is.character(all.plates.tmp)){
          passed <- FALSE
          break}
        passed <- T
      }

      if(!passed) next

      if(passed){
        out.manifest <- Manifest %>%
          dplyr::left_join(all.plates,"SampleID") %>%
          dplyr::group_by(plate) %>%
          dplyr::mutate(scramble=sample(1:n())) %>%
          dplyr::mutate(row=row[scramble],
                 column=column[scramble]) %>%
          dplyr::ungroup() %>%
          dplyr::select(-scramble)

      }
      if(missing(Groups)) break

    }

    if(missing(Groups)){
      cat("Random assignment of SUBJECTS to plates\n")
      if(passed){

        out.manifest <- out.manifest %>%
          dplyr::mutate(well=paste0(row,gsub("Column ","",as.character(column)))) %>%
          dplyr::mutate(well=factor(well,levels=paste0(rep(LETTERS[1:8],each=12),rep(1:12,times=8)))) %>%
          dplyr::arrange(plate, column, row)

        class(out.manifest) <- c("randomizedManifest",class(out.manifest))
        return(out.manifest)
      } else{
        stop("Could not keep all subjects on the same plate! Try increasing the number of iterations.")
      }
    }

  }

}





