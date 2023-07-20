product_to_platesize <- function(Product){
  olink_products <-c("Target 96", "Target 48", "Explore 384","Explore 3072","Explore HT")
  if(!(Product %in% olink_products)){
    stop(paste0("Product must be one of the following: ", paste(olink_products, sep = " ", collapse = ", ")))
  }
  PlateSize<- ifelse(Product == "Target 48",
                     48,
                     96)
  return(PlateSize)
}

#' Plot all plates colored by a variable
#'
#' Displays each plate in a facet with cells colored by the given variable using ggplot and ggplot2::geom_tile.
#' @param data tibble/data frame in long format returned from the olink_plate_randomizer function.
#' @param fill.color Column name to be used as coloring variable for wells.
#' @param PlateSize Integer. Either 96 or 48. 96 is default.
#' @param Product String. Name of Olink product used to set PlateSize if not provided. Optional. 
#' @param num_ctrl Numeric. Number of controls on each plate (default = 8)
#' @param rand_ctrl Logical. Whether controls are added to be randomized across the plate (default = FALSE)
#' @param include.label Should the variable group be shown in the plot.
#' @keywords randomized plates ggplot
#' @return An object of class "ggplot" showing each plate in a facet with the cells colored by values in column fill.color in input \code{data}.
#' @export
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_plate_randomizer]{olink_plate_randomizer()}} for generating a plating scheme}
#' \item{\code{\link[OlinkAnalyze:olink_displayPlateDistributions]{olink_displayPlateDistributions()}} for validating that sites are properly randomized}
#' }
#'
#' @examples
#' \donttest{randomized.manifest <- olink_plate_randomizer(manifest)}
#' \donttest{olink_displayPlateLayout(data = randomized.manifest, fill.color="Site")}
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr n filter select mutate
#' @importFrom ggplot2 ggplot geom_tile facet_wrap scale_fill_manual labs scale_x_discrete geom_text

olink_displayPlateLayout <- function(data, fill.color, PlateSize = 96, num_ctrl = 8, rand_ctrl = FALSE, Product, include.label=FALSE){
  if(!missing(Product)){
    PlateSize <- product_to_platesize(Product)
  }

  if(!PlateSize %in% c(48,96)){
    stop('Plate size needs to be either 48 or 96.')
  }

  spots_per_plate <- PlateSize - num_ctrl*!rand_ctrl
  number_of_cols_per_plate <- PlateSize/8

  missing.spots <- expand.grid(plate=unique(data$plate),
                               row=LETTERS[1:8],
                               column=paste("Column",1:(number_of_cols_per_plate)),fill.color="Empty")
  missing.spots$unique.id <- paste(missing.spots$plate,missing.spots$row,missing.spots$column)
  missing.spots <- missing.spots %>%
    dplyr::filter(!unique.id %in% paste(data$plate,data$row,data$column)) %>%
    dplyr::select(-unique.id)

  if(missing(fill.color)) fill.color <- "plate"

  data$fill.color <- data[[fill.color]]
  data <- data %>% 
    dplyr::mutate(fill.color = ifelse(SampleID == "CONTROL_SAMPLE", "CONTROL", fill.color))
  data <- data %>%
    dplyr::select(fill.color,plate,row,column,fill.color) %>%
    rbind(missing.spots) %>%
    dplyr::mutate(row=factor(row,levels=LETTERS[8:1]),
                  column=factor(column,levels=paste("Column",1:(number_of_cols_per_plate))),
                  fill.color=factor(fill.color))

  fill.levels <- levels(data$fill.color)
  if("Empty" %in% fill.levels){
    hld <- which(fill.levels=="Empty")
    fills <- rep(NA,length(fill.levels))
    fills[-hld] <- olink_pal()(length(fill.levels)-1)
    fills[hld] <- "#ffffff"
  }else{
    fills <- OlinkAnalyze::olink_pal()(length(fill.levels))
  }


  p <- ggplot2::ggplot(ggplot2::aes(x=column,y=row,fill=fill.color),data=data)+
    ggplot2::geom_tile(color="black")+
    ggplot2::facet_wrap(~plate,ncol=1,scales="fixed")+
    OlinkAnalyze::set_plot_theme()+
    ggplot2::scale_fill_manual(values=fills)+
    ggplot2::labs(x="",y="",fill=fill.color)+
    ggplot2::scale_x_discrete(labels=paste0("Col",1:(number_of_cols_per_plate)))

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
#' @keywords randomized plates ggplot
#' @export
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_plate_randomizer]{olink_plate_randomizer()}} for generating a plating scheme}
#' \item{\code{\link[OlinkAnalyze:olink_displayPlateLayout]{olink_displayPlateLayout()}} for visualizing the generated plate layouts}
#' }
#'
#' @return An object of class "ggplot" showing the percent distribution of fill.color in each plate (x-axis)
#' @examples
#' \donttest{randomized.manifest <- olink_plate_randomizer(manifest)}
#' \donttest{olink_displayPlateDistributions(data=randomized.manifest,fill.color="Site")}
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by tally ungroup mutate summarize as_tibble arrange
#' @importFrom ggplot2 ggplot aes theme labs geom_bar element_text


olink_displayPlateDistributions <- function(data,fill.color){

  data$group.var <- data[[fill.color]]

  p1 <- data %>%
    dplyr::group_by(plate,group.var) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(plate) %>%
    dplyr::mutate(percent=100*n/sum(n)) %>%
    ggplot2::ggplot(ggplot2::aes(x=plate,y=percent,fill=group.var)) +
    ggplot2::geom_bar(stat="identity",color="gray")+
    OlinkAnalyze::olink_fill_discrete()+
    OlinkAnalyze::set_plot_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust=0,vjust=0.5))+
    ggplot2::labs(fill=fill.color,x="Plate",y="Percent")+
    ggplot2::theme(legend.position = "bottom")

  return(p1)

}


assignSubject2Plate <- function(plateMap,manifest,SubjectID){
  
  # Select the samples from that subject
  samp.ids <- manifest$SampleID[manifest$SubjectID==SubjectID]
  
  
  # Determine how many spots on each plate are still available
  spots.available <- plateMap %>%
    dplyr::group_by(plate) %>%
    dplyr::summarize(available=sum(is.na(SampleID)))
  
  # do any of the plates have enough space for the samples from this subject
  if(any(spots.available$available>=length(samp.ids))){
    # if so, choose a random plate where the samples will fit 
    plate.assign <- sample(spots.available$plate[spots.available$available>=length(samp.ids)],1)
  } else{
    # return a warning
    return("This Sample does not fit!")
  }
  
  # list the samples on the plate where they can fit, note this doesnt randomize them by sample, just subject per plate, this is where we can add in the control samples if we are randomizing by control
  placement <- which(plateMap$plate==plate.assign & is.na(plateMap$SampleID))[1:length(samp.ids)]
  plateMap$SampleID[placement] <- samp.ids
  return(dplyr::as_tibble(plateMap))
  
}


generatePlateHolder <- function(n.plates,n.spots,n.samples, PlateSize, num_ctrl, rand_ctrl){
  # Both these are calculated in main function, is this sub function called anywhere else
  # plate size - # of ctrls = spots per plate
  spots_per_plate <- PlateSize - num_ctrl*!rand_ctrl
  # cols given 8 row
  number_of_cols_per_plate <- PlateSize/8
  
  
  #check right number of plates and spots given samples and given plate capacity
  if(n.plates!=length(n.spots)) stop("Vector of available spots must equal number of plates!")
  if(any(n.spots>spots_per_plate)) stop("Number of samples per plates cannot exceed 40 for T48 and 88 for T96!")
  if(sum(n.spots)<n.samples) stop("More samples than available spots! Double check your numbers!")
  
  # Create a grid given number of columns - columns used by controls
  full.row.col <- expand.grid(column=paste0("Column ",1:(number_of_cols_per_plate)),
                              row=LETTERS[1:8],
                              stringsAsFactors = TRUE) %>% # String as factor needed so Column 10 doesnt come before column 2 in ordering
    dplyr::arrange(column, row) %>% #row is unnecessary here but its fine
    dplyr::slice_head(n = PlateSize - num_ctrl*!rand_ctrl)
  
  # Instead we could generate entire plate and then filter out the end of the plate (last column, end of rows, would need to make sure order is correct) for the controls if they are not randomized throughout the plate
  
  # Create name/number of plates
  plates <- paste0("Plate ",1:n.plates)
  
  # Create outline of data frame
  out <- data.frame(plate = NULL, column = NULL, row = NULL, stringsAsFactors = FALSE)
  
  # Create plate layout based on available spots
  # this chunk could be simplified or at least clarified better - maybe not using a for loop?
  for(i in 1:n.plates){
    hld <- cbind(plate=rep(paste0("Plate ",i),n.spots[i]),
                 full.row.col[1:n.spots[i],])
    out <- rbind(out,hld)
  }
  return(out)
}




#' Randomly assign samples to plates
#'
#' Generates a scheme for how to plate samples with an option to keep subjects on the same plate.
#'
#' Variables of interest should if possible be randomized across plates to avoid confounding with potential plate effects. In the case of multiple samples per subject (e.g. in longitudinal studies), Olink recommends keeping each subject on the same plate. This can be achieved using the SubjectColumn argument.
#' @param Manifest tibble/data frame in long format containing all sample ID's. Sample ID column must be named SampleID.
#' @param PlateSize Integer. Either 96 or 48. 96 is default.
#' @param Product String. Name of Olink product used to set PlateSize if not provided. Optional. 
#' @param SubjectColumn (Optional) Column name of the subject ID column. Cannot contain missings. If provided, subjects are kept on the same plate.
#' @param iterations Number of iterations for fitting subjects on the same plate.
#' @param available.spots Numeric. Number of wells available on each plate. Maximum 40 for T48 and 88 for T96. Takes a vector equal to the number of plates to be used indicating the number of wells available on each plate.
#' @param num_ctrl Numeric. Number of controls on each plate (default = 8)
#' @param rand_ctrl Logical. Whether controls are added to be randomized across the plate (default = FALSE)
#' @param seed Seed to set. Highly recommend setting this for reproducibility.
#' @return A "tibble" including SampleID, SubjectID etc. assigned to well positions.
#' Columns include same columns as Manifest with additional columns:
#' \itemize{
#'    \item{plate:} Plate number
#'    \item{column:} Column on the plate
#'    \item{row:} Row on the plate
#'    \item{well:} Well location on the plate
#' }
#' @keywords randomized plates
#' @export
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_displayPlateLayout]{olink_displayPlateLayout()}} for visualizing the generated plate layouts}
#' \item{\code{\link[OlinkAnalyze:olink_displayPlateDistributions]{olink_displayPlateDistributions()}} for validating that sites are properly randomized}
#' }
#'
#' @examples
#' \donttest{
#' #Generate randomization scheme using complete randomization
#' randomized.manifest_a <- olink_plate_randomizer(manifest, seed=12345)
#'
#' #Generate randomization scheme that keeps subjects on the same plate
#' randomized.manifest_b <- olink_plate_randomizer(manifest,SubjectColumn="SubjectID",
#'                                                         available.spots=c(88,88), seed=12345)
#'
#' #Visualize the generated plate layouts
#' olink_displayPlateLayout(randomized.manifest_a, fill.color = 'Site')
#' olink_displayPlateLayout(randomized.manifest_a, fill.color = 'SubjectID')
#' olink_displayPlateLayout(randomized.manifest_b, fill.color = 'Site')
#' olink_displayPlateLayout(randomized.manifest_b, fill.color = 'SubjectID')
#'
#' #Validate that sites are properly randomized
#' olink_displayPlateDistributions(randomized.manifest_a, fill.color = 'Site')
#' olink_displayPlateDistributions(randomized.manifest_b, fill.color = 'Site')
#' }
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble mutate arrange left_join group_by ungroup select
#' @importFrom tibble is_tibble

#Main randomization function
olink_plate_randomizer <- function(Manifest, PlateSize = 96, Product, SubjectColumn, iterations=500, available.spots, num_ctrl = 8, rand_ctrl = FALSE, seed){
  
  #Check if SampleID column is present in manifest
  if(!"SampleID" %in% colnames(Manifest)) {
    stop("SampleID not found! Be sure the column of samples ID's is named 'SampleID'")
  }
  
  if(!missing(Product)){
    PlateSize <- product_to_platesize(Product)
  }
  
  # Check if there are any duplicated Sample IDs in manifest
  if(any(which(duplicated(Manifest$SampleID)))){
    duplications <- Manifest$SampleID[which(duplicated(Manifest$SampleID))]
    warning(paste("Following SampleID(s) was/were duplicated:",paste(duplications,collapse = "\n"), sep = "\n"))
  }
  
  # Check if there are any NAs in SampleID column
  if(any(is.na(Manifest$SampleID))) {
    stop("No NA allowed in the SampleID column. Check that all the samples are named.")
  }
  
  # Check plate size is acceptable
  if(!PlateSize %in% c(48,96)){
    stop('Plate size needs to be either 48 or 96.')
  }
  
  # spots = platesize - num_ctrls
  # do we use this if available.spots is present?
  spots_per_plate <- PlateSize - num_ctrl*!rand_ctrl # subtract number of controls if not randomizing controls
  
  # cols = platesize/8 (b/c 8 rows on each plate)
  number_of_cols_per_plate <- PlateSize/8
  
  #if there is a seed, set the seed
  if(!missing(seed)) {
    set.seed(seed)
  }
  
  # Check that the subject column provided is present
  if(!missing(SubjectColumn)){
    if(!any(colnames(Manifest) == SubjectColumn)){
      stop("The user assigned SubjectColumn name was not found! Make sure the SubjectColumn is present in the dataset.")
    }
    Manifest$SubjectID <- Manifest[[SubjectColumn]]
  }
  
  # Check that the subjectID column does not have any NAs
  if(any(is.na(Manifest$SubjectID))) {
    stop("No NA allowed in the SubjectID column. Check that all the subjects are named.")
  }
  
  # Assuming all plates have same plate size
  if(missing(available.spots)){
    # plates needed calculated based on # of samples/# of spots per plate
    PlatesNeeded <-ceiling(nrow(Manifest)/spots_per_plate)
    
    # Add an additional plate if needed when randomizing the controls (if ctrls cause plate to be overfilled)
    if(rand_ctrl){
      PlatesNeeded <-ifelse(nrow(Manifest)%%spots_per_plate < PlatesNeeded*num_ctrl,
                            PlatesNeeded + ceiling(PlatesNeeded*num_ctrl/spots_per_plate),
                            PlatesNeeded)
      
    }
    
    # Create a plate map with rows and columns based on number of spots available on each plate, either using max that will fit on the plate - controls, or specified number per plate based on available spots    
    all.plates <- generatePlateHolder(PlatesNeeded,rep(spots_per_plate,times=PlatesNeeded),n.samples=length(Manifest$SampleID)+(num_ctrl*rand_ctrl*PlatesNeeded), PlateSize = PlateSize, num_ctrl = num_ctrl, rand_ctrl = rand_ctrl)
  } else{
    PlatesNeeded <- length(available.spots)
    
    all.plates <- generatePlateHolder(length(available.spots),available.spots,n.samples=length(Manifest$SampleID)+(num_ctrl*rand_ctrl*PlatesNeeded), PlateSize = PlateSize, num_ctrl = num_ctrl, rand_ctrl = rand_ctrl)
    
  }
  
  #Complete randomization if subjectID not given
  if(missing(SubjectColumn) & suppressWarnings(is.null(Manifest$study))){
    # randomly order the first X rows (x being number of samples) from plate layout to assign samples to
    all.plates <- all.plates[sample(1:(nrow(Manifest)+num_ctrl*rand_ctrl*PlatesNeeded)),]
    
    # bind manifest to randomized plate layout, add wellID, reorder back to standard plate
    # This doesnt work as well if # rows in manifest != # rows of all plates (could get duplicate samples) - which is what we filtered for above
    
    ctrl_locations <- all.plates %>% 
      dplyr::group_by(plate) %>% 
      dplyr::slice_sample(n = num_ctrl*rand_ctrl) %>%  # Select random locations from each plate when randomizing controls
      dplyr::mutate(ID = paste0(plate,column,row)) %>% 
      dplyr::mutate(SampleID = "CONTROL_SAMPLE")
    
    # Remove ctrl locations from list of possible locations
    all.plates <- all.plates %>% 
      dplyr::mutate(ID = paste0(plate,column,row)) %>% 
      filter(!(ID %in% ctrl_locations$ID))
    
    
    out.manifest <- dplyr::as_tibble(cbind(Manifest,all.plates)) %>%
      dplyr::bind_rows(ctrl_locations) %>% 
      dplyr::mutate(well=paste0(row,gsub("Column ","",as.character(column)))) %>%
      dplyr::mutate(well=factor(well,levels=paste0(rep(LETTERS[1:8],each=number_of_cols_per_plate),rep(1:number_of_cols_per_plate,times=8)))) %>% # This could use the row column in all.plates instead of regenerating it. 
      dplyr::arrange(plate, column, row) %>% 
      select(-ID)
    message("Random assignment of SAMPLES to plates\n")
    class(out.manifest) <- c("randomizedManifest",class(out.manifest))
    return(out.manifest)
    # Complete randomization does not require iteration
  }
  
  
  ##Keep subjects together
  if(!missing(SubjectColumn) & suppressWarnings(is.null(Manifest$study))){
    message("Assigning subjects to plates\n")
    for(i in 1:iterations){
      # prints ... for interations
      message(".")
      
      # Create new column sampleID
      all.plates$SampleID <- NA_character_
      ctrl_locations <- all.plates |> 
        dplyr::slice(0) |> 
        dplyr::mutate(ID = NA_character_)
      # When randomizing controls
      if(rand_ctrl){
         ctrl_locations <- all.plates %>% 
        dplyr::group_by(plate) %>% 
        dplyr::slice_sample(n = num_ctrl*rand_ctrl) %>%  # Select random locations from each plate when randomizing controls
        dplyr::mutate(ID = paste0(plate,column,row)) %>% 
        dplyr::mutate(SampleID = "CONTROL_SAMPLE")
      }
     
      
      # Remove ctrl locations from list of possible locations when randomizing controls
      all.plates <- all.plates %>% 
        dplyr::mutate(ID = paste0(plate,column,row)) %>% 
        filter(!(ID %in% ctrl_locations$ID))
      
      # randomize subject order
      rand.subjects <- sample(unique(Manifest$SubjectID))
      
      # for each subject in the randomized list of subjects
      for(sub in rand.subjects){
        
        all.plates.tmp <- assignSubject2Plate(plateMap=all.plates,
                                              manifest=Manifest,
                                              SubjectID=sub)
        # Check if assign subject 2 plate returned a df or a warning string (passed or didn't)
        # This could be written such that it didnt include a break statement
        if(tibble::is_tibble(all.plates.tmp)){
          # reassign platemap to now include assigned sample
          all.plates <- all.plates.tmp
        } else if(is.character(all.plates.tmp)){
          passed <- FALSE
          break}
        passed <- TRUE
      }
      
      # Now we have determined which samples fit on which plates, and we scramble the rows and columns within that plate
      if(passed){
        out.manifest <- Manifest %>%
          dplyr::left_join(all.plates,"SampleID") %>% # merge plate assignment of each sample
          dplyr::group_by(plate) %>% # group by plates
          dplyr::mutate(scramble=sample(1:dplyr::n())) %>%  # assign random number per sample in each plate
          
          dplyr::mutate(row=row[scramble],
                        column=column[scramble]) %>%  # Move the sample to the row of the scramble number, now each row and column has been moved to the location of its scramble number
          dplyr::ungroup() %>%
          dplyr::select(-scramble)
        break
      }
      
    }
    
    message("Random assignment of SUBJECTS to plates\n")
    if(passed){
      # Arrange updated manifest in order  
      out.manifest <- out.manifest %>%
        dplyr::bind_rows(ctrl_locations) %>% 
        dplyr::mutate(well=paste0(row,gsub("Column ","",as.character(column)))) %>%
        dplyr::mutate(well=factor(well,levels=paste0(rep(LETTERS[1:8],each=number_of_cols_per_plate),rep(1:number_of_cols_per_plate,times=8)))) %>%
        dplyr::arrange(plate, column, row) %>% 
        dplyr::select(-ID)
      
      class(out.manifest) <- c("randomizedManifest",class(out.manifest))
      return(out.manifest)
    } else{
      stop("Could not keep all subjects on the same plate! Try increasing the number of iterations.")
    }
    
  }
  
  ##Keep subjects together and keep studies together
  if(!missing(SubjectColumn) & suppressWarnings(!is.null(Manifest$study))){
    message("Assigning subjects to plates. 'study' column detected so keeping studies together during randomization. \n")
    # When randomizing controls
    all.plates$SampleID <- NA_character_
    ctrl_locations <- all.plates |> 
      dplyr::slice(0) |> 
      dplyr::mutate(ID = NA_character_)
    # When randomizing controls
    if(rand_ctrl){
      ctrl_locations <- all.plates %>% 
        dplyr::group_by(plate) %>% 
        dplyr::slice_sample(n = num_ctrl*rand_ctrl) %>%  # Select random locations from each plate when randomizing controls
        dplyr::mutate(ID = paste0(plate,column,row)) %>% 
        dplyr::mutate(SampleID = "CONTROL_SAMPLE")
    }
    
    
    # Remove ctrl locations from list of possible locations when randomizing controls
    all.plates <- all.plates %>% 
      dplyr::mutate(ID = paste0(plate,column,row)) %>% 
      filter(!(ID %in% ctrl_locations$ID))
    
    
    out.manifest <- matrix(nrow = 0,ncol = ncol(Manifest))
    
    #Randomize on SubjectID_study in case SubjectID is duplicated over studies
    Manifest <- Manifest %>% dplyr::mutate(
      SubjectID_old = SubjectID,
      SubjectID = paste0(SubjectID,"_",study))
    
    Manifest <- Manifest %>% dplyr::arrange(study) 
    j_tot <- 0 
    
    #Keep every study together
    for (studyNo in unique(Manifest$study)){
      passed <- FALSE
      rand.subjects <- sample(Manifest %>% 
                                dplyr::filter(study == studyNo) %>%
                                dplyr::select(SubjectID) %>%
                                unique() %>%
                                dplyr::pull())
      studyInterval <- which(Manifest$study == studyNo)
      sub.groups <- Manifest %>% 
        dplyr::filter(study == studyNo) %>%
        dplyr::select(SubjectID) %>% table()
      sub.groups.max <- as.numeric(max(sub.groups))
      
      #Append j for every well left empty
      for(j in 0:sub.groups.max){
        
        #Extend number of positions available on the last plate
        if(j>0){
          extendedStudyInterval <- unique(c(studyInterval+ j_tot,(max(studyInterval+j_tot):(max(studyInterval)+j_tot+j))))
          platesThisLap <- all.plates[extendedStudyInterval,"plate"] %>% unique()
          
          #Extend number of plates if needed
          if(missing(available.spots)){
            PlatesNeeded <-ceiling((length(Manifest$SampleID)+(num_ctrl*rand_ctrl*PlatesNeeded)+j_tot+j)/spots_per_plate)
            all.plates.New <- generatePlateHolder(PlatesNeeded,
                                                  rep(spots_per_plate,
                                                      times=PlatesNeeded),
                                                  n.samples=length(Manifest$SampleID)+(num_ctrl*rand_ctrl*PlatesNeeded)+j_tot+j,
                                                  PlateSize = PlateSize)
          }else{
            all.plates.New <- generatePlateHolder(length(available.spots),available.spots,n.samples=length(Manifest$SampleID)+j_tot+j, PlateSize = PlateSize)
          }
          all.plates.New$SampleID <- rep(NA,nrow(all.plates.New)) 
          all.plates <- all.plates.New
        }else{
          extendedStudyInterval <- unique(c(studyInterval+ j_tot,(max(studyInterval+j_tot):(max(studyInterval)+j_tot+j))))
          platesThisLap <- all.plates[extendedStudyInterval,"plate"] %>% unique()
        }
        message(paste0("Testing with ",j, " empty well(s) in the plate. \n"))
        ManifestStudy <- Manifest[studyInterval,]
        for(i in 1:iterations){
          message(".")
          for (sub in rand.subjects) {
            all.plates.tmp <- assignSubject2Plate(plateMap = all.plates[extendedStudyInterval,], 
                                                  manifest = Manifest, SubjectID = sub)
            if (tibble::is_tibble(all.plates.tmp)) {
              all.plates[extendedStudyInterval,] <- all.plates.tmp
            }
            else if (is.character(all.plates.tmp)) {
              passed <- FALSE
              break
            }
            passed <- TRUE
          }
          if(passed){
            out.manifestStudy <- ManifestStudy %>%
              tidyr::drop_na() %>% 
              dplyr::left_join(all.plates,"SampleID") %>%
              dplyr::arrange(plate) %>% 
              dplyr::group_by(plate) %>%
              dplyr::mutate(scramble=sample(1:dplyr::n())) %>%
              dplyr::mutate(row=row[scramble],
                            column=column[scramble]) %>%
              dplyr::arrange(plate) %>% 
              dplyr::ungroup() %>%
              dplyr::select(-scramble) %>% 
              dplyr::arrange(plate, column, row)
            message(paste(studyNo,"successful! \n"))
            out.manifest <- rbind(out.manifest, out.manifestStudy)
            ManifestStudy2 <- ManifestStudy %>%
              dplyr::left_join(all.plates,"SampleID") %>%
              dplyr::group_by(plate) %>%
              dplyr::mutate(scramble=sample(1:dplyr::n()))
            ManifestStudy2 %>% mutate(row = row[scramble], 
                                      column = column[scramble])
          }
          if(passed){
            j_tot <- j_tot + j
            break
          } 
        }
        if(passed) break
      }
    }
    message("Random assignment of SUBJECTS to plates\n")
    if(passed){
      message(paste("Totally included", j_tot, "empty well(s) in first and/or intermediate plate(s) to accomplish the randomization.\n"))
      message(paste("Please try another seed or increase the number of iterations if there are indications that another randomization might leave fewer empty wells.\n"))      
      out.manifest <- out.manifest %>%
        dplyr::bind_rows(ctrl_locations) %>% 
        dplyr::mutate(well=paste0(row,gsub("Column ","",as.character(column)))) %>%
        dplyr::mutate(well=factor(well,levels=paste0(rep(LETTERS[1:8],each=number_of_cols_per_plate),rep(1:number_of_cols_per_plate,times=8))),
                      SubjectID = SubjectID_old) %>%
        dplyr::select(-SubjectID_old) %>% 
        dplyr::arrange(plate, column, row)
      class(out.manifest) <- c("randomizedManifest",class(out.manifest))
      return(out.manifest)
    } else{
      stop("Could not keep all subjects on the same plate! Try increasing the number of iterations.")
    }
  }
  
  #Complete randomization within studies when subjectID is not given
  if(missing(SubjectColumn) & suppressWarnings(!is.null(Manifest$study))){
    message("Assigning subjects to plates. 'study' column detected so keeping studies together during randomization. \n")
    
    out.manifest <- matrix(nrow = 0,ncol = ncol(Manifest))
    
    all.plates$SampleID <- NA_character_
    ctrl_locations <- all.plates |> 
      dplyr::slice(0) |> 
      dplyr::mutate(ID = NA_character_)
    # When randomizing controls
    if(rand_ctrl){
      ctrl_locations <- all.plates %>% 
        dplyr::group_by(plate) %>% 
        dplyr::slice_sample(n = num_ctrl*rand_ctrl) %>%  # Select random locations from each plate when randomizing controls
        dplyr::mutate(ID = paste0(plate,column,row)) %>% 
        dplyr::mutate(SampleID = "CONTROL_SAMPLE")
    }
    
    
    # Remove ctrl locations from list of possible locations when randomizing controls
    all.plates <- all.plates %>% 
      dplyr::mutate(ID = paste0(plate,column,row)) %>% 
      filter(!(ID %in% ctrl_locations$ID))
    
    Manifest <- Manifest %>% dplyr::arrange(study) 
    for (studyNo in unique(Manifest$study)){
      studyInterval <- which(Manifest$study == studyNo)
      
      ManifestStudy <- Manifest[studyInterval,]
      all.plates_study <- all.plates[studyInterval,]
      
      all.plates_study <- all.plates_study[sample(1:nrow(ManifestStudy)),]
      
      out.manifestStudy <- dplyr::as_tibble(cbind(ManifestStudy,all.plates_study)) %>%
        dplyr::bind_rows(ctrl_locations) %>% 
        dplyr::mutate(well=paste0(row,gsub("Column ","",as.character(column)))) %>%
        dplyr::mutate(well=factor(well,levels=paste0(rep(LETTERS[1:8],
                                                         each=number_of_cols_per_plate),
                                                     rep(1:number_of_cols_per_plate,times=8)))) %>%
        dplyr::arrange(plate, column, row)
      out.manifest <- rbind(out.manifest, out.manifestStudy)
      
    }
    message("Random assignment of SAMPLES to plates by study\n")
    class(out.manifest) <- c("randomizedManifest",class(out.manifest))
    return(out.manifest)
  }
}





