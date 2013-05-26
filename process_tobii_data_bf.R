# -----------------------------------------------
#
# process_tobii_data.r
#
# This script processes a tab-delimited values file exported from Tobii using the "Data for Excel Templates"
# template.  With definitions from an AOI file and phase timing file, it will add columns to the original
# data file specifying *what* the subject was looking at throughout the experimental session and export
# as a comma-separated values file.
#
# Commented and extended by Brock Ferguson (2012).
#
# Improvements/Updates:
#   * No need to prepare each individual data file.  Script can deal with the
#     property values on any line, and the headers on any line.
#   * Subject numbers can be in any format, as long as it ends with the number
#     (e.g., "TNV_24", "24", "STUDY24")
#   * Now deals with new Tobii 3.x export files (Feb 13, 2012).
#   * Fixed find_looks() to work with new Tobii output and also track orders.
#   * Added auto_process_tobii() and updated concat_csv() to work with Tobii 3.x.
#
# USAGE
#
# Input: A tab-separated data file, tab-separated AOI file, tab-separated phase timing file.
# Output: A comma-separated values file including processed/new data.
#
# 1) Load this library:
#       source('process_tobii_data.R')
#
# 2) Call function with proper arguments:
#       process_tobii ('data_file_name.txt', 'aoi_file_name.txt', 'phase_timing_file_name.txt', 'Condition', 'Order', 'output_file_name.csv')
#
#       e.g., process_tobii ('Verb_A_14_Data_from_Tobii.txt', 'aoifile_tnv24.txt', 'phase_timing_tnv24.txt', 'Verb', 'A', 'processed_data14.csv')
#
# @author Winston Chang
# @date 2009
# @modified Brock Ferguson
# @modified_date May 8, 2012
#
# -----------------------------------------------

#
# auto_process_tobii
#
# process all of the raw Tobii 3.x data files in a folder
#
# minimum requirements:
#   auto_process_tobii(aoifile, phasefile);
#
# optional arguments:
#
#   Specify these additional parameters to process only a select group of the files,
#   based on condition or trial order.
#
#   auto_process_tobii(aoifile, phasefile, condition_to_include, trialorder_to_include)

auto_process_tobii <- function (aoifile, phasefile, only_condition = FALSE, only_trialorder = FALSE) {
  files <- list.files(path=getwd(), pattern=".tsv", all.files=T, full.names=F)
  
  for (file in files) {
    # get condition
    conditionLocation = regexpr("_[a-zA-Z0-9 \\_]+_", file)
    condition = substr(file, conditionLocation + 1, conditionLocation + attr(conditionLocation, 'match.length') - 2)
    
    if (only_condition != FALSE && only_condition != condition) {
      # skip this file
      next
    }
    
    # get trialorder
    trialorderLocation = regexpr("-[a-zA-Z0-9]+_", file)
    trialorder = substr(file, trialorderLocation + 1, trialorderLocation + attr(trialorderLocation, 'match.length') - 2)
    
    if (only_trialorder != FALSE && only_trialorder != trialorder) {
      # skip this file
      next
    }
    
    # set output filename
    participantNumberLocation = regexpr("[0-9]+.tsv", file)
    participantNumber = substr(file, participantNumberLocation, participantNumberLocation + attr(participantNumberLocation, 'match.length') - 5)
    
    processed_filename = paste('processed-',file,'.csv', sep="",collapse=NULL)
    
    # process file
    process_tobii(file, aoifile, phasefile, condition, trialorder, processed_filename)
  }
}

#
# process_tobii
#
# Main function to be called by user with proper arguments.
process_tobii <- function(tobiifile, aoifile, phasefile, condition, trialorder, outputfile=NULL) {
  
  # Retrieve start time for runtime calculation
  startTime1 <- proc.time()
  
  # commented out Feb 13, 2012 - should implement later so we can get a SubjectNum
  # field again
  # 
  # Extract the subject number from participant string (e.g., "TNV24_14" has subject number 14)
  # by replacing TNV24_ with nothing.
  
  # subjectNum  = as.numeric(gsub(".*_", "", participant))
  # subjectNumLocation = regexpr("[0-9]+$", participant)
  # subjectNum = substr(participant, subjectNumLocation, attr(subjectNumLocation, 'match.length'))
  
  # Verify that the subject number exists
  # if (is.na(subjectNum)) {
  #    warning(sprintf("No subject number extracted from Participant string \"%s\" from Tobii data file!", participant))
  #    cat("  No subject number found! The output file will probably have problems.\n")
  #    cat("  Make sure that the Participant line has the format XXXXX_YY, where YY is the subject number.\n")
  # }
  
  # READ THE DATA (BEGINNING WITH COLUMN HEADERS) INTO A DATA FRAME (e.g., TABLE OBJECT).
  #
  # read.table : reads a file into a data form
  #    file=tobiifile : read the file called tobiifile
  #    sep="\t" : the file is tab separated
  #    header=TRUE : get the column names from the first line    
  #    fill=TRUE : fill in empty fields with blank text fields
  #    skip=27 : skip the first 27 lines
  
  cat("Loading raw Tobii data file into data frame object...\n")
  
  tobii = read.table(file=tobiifile, sep="\t", header=TRUE, fill=TRUE, skip=0)
  # add in the data we read out of the header
  # tobii$SubjectNum  = subjectNum
  
  # these variables come from the function arguments
  tobii$Condition   = as.factor(condition)
  tobii$TrialOrder  = as.factor(trialorder)
  
  # rename a few columns to get rid of punctuation and funky formatting
  names(tobii)[names(tobii)=="GazePointLeftX..ADCSpx."] <- "GazePointLeftX"
  names(tobii)[names(tobii)=="GazePointRightX..ADCSpx."] <- "GazePointRightX"
  names(tobii)[names(tobii)=="GazePointLeftY..ADCSpx."] <- "GazePointLeftY"
  names(tobii)[names(tobii)=="GazePointRightY..ADCSpx."] <- "GazePointRightY"
  names(tobii)[names(tobii)=="GazePointY..ADCSpx."] <- "GazePointY"
  names(tobii)[names(tobii)=="GazePointX..ADCSpx."] <- "GazePointX"
  names(tobii)[names(tobii)=="X.StudioTestName"] <- "StudioTest"
  names(tobii)[names(tobii)=="X.Gender.Value"] <- "Gender"
  
  # Rename the "MediaName" column to "MovieName" to reduce ambiguity
  # (note from Brock: what ambiguity?)
  names(tobii)[names(tobii)=="MediaName"] <- "MovieName"
  
  # Replace "" in MovieName with NA
  tobii$MovieName[ tobii$MovieName=="" ]          <- NA
  
  # Re-factor to remove the unused levels
  tobii$MovieName <- factor(tobii$MovieName)
  
  # Drop the rows that say MovieStart or MovieEnd
  # because these rows do not contain any looking data
  tobii <- subset(tobii, StudioEvent!="MovieStart" & StudioEvent !="MovieEnd")
  
  cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime1)[3], digits=1)))
  
  # LOAD PHASE TIMING FILE AND MERGE WITH MAIN DATA FRAME
  #
  # This essentially matches the existing "MovieName" field containing the .AVI filename
  # with the trial and phase information from the phase timing file, and adds these
  # columns to the data frame.
  
  startTime <- proc.time()  # Get time for runtime measurement
  cat("Merging Trial and Phase columns based on MovieName...\n")
  
  # Load the phase timing file
  phaseTiming = read.table(file=phasefile, sep="\t", header=TRUE, fill=TRUE)
  
  # We do not need the subphase or timing columns from the phase timing file right now
  # so we will create a subset of the phase timing file that does not include those.
  # We will use that data later.
  phaseTimingSubset <- unique(phaseTiming[,c("Filename","Trial","Phase")])
  
  # Merge the phaseTimingSubset information with the tobii data file
  tobii <- merge(x=tobii, y=phaseTimingSubset, by.x="MovieName", by.y="Filename", all.x=TRUE)
  
  # The merge could have messed up the order of the data, so sort by timestamp.
  tobii <- tobii[order(tobii$RecordingTimestamp), ]
  
  cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime)[3], digits=1)))
  
  # FILL IN NA GAPS IN TRIAL COLUMN
  #
  # Some Trial rows may be NA because of timing issues.  So, we will take the
  # last non-NA value and set this is as the trial value.  This smooths over
  # the issue.
  
  startTime <- proc.time()  # Get time for runtime measurement
  
  cat("Filling in NA gaps in Trial column...\n")
  
  fillNAgaps <- function (x, firstBack=FALSE) {
    ## NA's in a vector or factor are replaced with last non-NA values
    ## If firstBack is TRUE, it will fill in leading NA's with the first
    ## non-NA value. If FALSE, it will not change leading NA's.
    
    # If it's a factor, store the level labels and convert to integer
    if (is.factor(x)) {
      lvls <- levels(x)
      x    <- as.integer(x)
    }
    
    goodIdx <- !is.na(x)
    
    # These are the non-NA values from x only
    # Add a leading NA or take the first good value, depending on firstBack   
    if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
    else             goodVals <- c(NA,            x[goodIdx])
    
    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx)+1
    
    x <- goodVals[fillIdx]
    
    # If it was originally a factor, convert it back
    if (exists("lvls"))   x <- factor(x, levels=1:length(lvls), labels=lvls)
    
    x
  }
  
  # Execute above function
  tobii$Trial <- fillNAgaps(tobii$Trial)
  
  cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime)[3], digits=1)))
  
  
  ### ADD TIMINGS FOR MOVIENAME, TRIAL, PHASE ###
  # The movie onset may align with the trial or phase, depending on the particular experiment.
  # This assumes there's a hierarchial structure to the data:
  # Subject -> Condition -> Trial -> Phase -> Subphase
  # For the purposes here, the MovieName column is considered on the same level as Trial
  # but isn't part of the full hierarchy.
  # Each combination of values for each of these variables is assumed to demarcate
  # a single, unique block, where the timings will run from 0, 1, etc.
  # If there is ever more than one block with the same combination of these values, the
  # timings for the subsequent blocks will be wrong.
  
  startTime <- proc.time()  # Get time for runtime measurement
  cat("Calculating frame count and timing for Movie and Phase...\n")
  
  # Create temporary vectors to store this stuff, and later store a copy of that vector as a new column
  # in the data frame.
  # This is much, much, much (maybe 1000x) faster than modifying a column in the data frame directly.    
  FramesFromMovieOnset    <- vector(mode="numeric", length=nrow(tobii))
  FramesFromPhaseOnset    <- vector(mode="numeric", length=nrow(tobii))
  # Start with count of zero for all
  FramesFromMovieOnset[1:nrow(tobii)]    <- 0    
  FramesFromPhaseOnset[1:nrow(tobii)]    <- 0    
  
  for (ParticipantName in unique(tobii$ParticipantName)) {
    # Pull out just the rows for this subject
    subjectrows <- tobii$ParticipantName==ParticipantName
    
    # Replace NA's with FALSE
    subjectrows[ is.na(subjectrows) ] <- FALSE
    
    for (condition in levels(tobii$Condition)) {
      # Now take the subset for this condition
      conditionrows <- subjectrows & tobii$Condition == condition
      conditionrows[ is.na(conditionrows) ] <- FALSE
      
      for (moviename in levels(tobii$MovieName)) {
        # Now take the subset for this moviename
        movienamerows <- conditionrows & tobii$MovieName == moviename
        movienamerows[ is.na(movienamerows) ] <- FALSE
        
        # Add the frame numbering
        FramesFromMovieOnset[movienamerows] <- 0:(sum(movienamerows)-1)
      }
      
      # MovieName is sort of separate from the Trial-Phase-Subphase hierarchy,
      #  so it is considered separately above. This improves performance a lot,
      #  compared to nesting it within the rest.
      
      for (trial in levels(tobii$Trial)) {
        # Take the subset for this trial
        trialrows <- conditionrows  &  tobii$Trial==trial
        trialrows[ is.na(trialrows) ] <- FALSE
        
        for (phase in levels(tobii$Phase)) {
          # Take the subset for this phase
          phaserows <- trialrows  &  tobii$Phase==phase
          phaserows[ is.na(phaserows) ] <- FALSE
          
          # Add the frame numbering
          FramesFromPhaseOnset[phaserows] <- 0:(sum(phaserows)-1)         
        }
      }
    }
  }
  
  # Store the vector as a new column in the data frame
  tobii$FramesFromMovieOnset    <- FramesFromMovieOnset
  tobii$FramesFromPhaseOnset    <- FramesFromPhaseOnset
  
  # Also calculate the time (in milliseconds) from frame. The data is collected at 60Hz
  tobii$TimeFromMovieOnset    <- round(FramesFromMovieOnset*1000/60)
  tobii$TimeFromPhaseOnset    <- round(FramesFromPhaseOnset*1000/60)
  
  cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime)[3], digits=1)))
  
  
  # FIND CURRENT SUBPHASE
  #
  # Now that we know which phase each frame is in, we can calculate which Subphase 
  # each frame is in. Unlike the MovieName, Trial, and Phase, Subphase is based on how long 
  # we've been in the current Phase. So we had to do the Trial and Phase before we could do this part.
  
  # Create the factor for the subphase information
  Subphase <- factor(vector(length=nrow(tobii)), levels=levels(phaseTiming$Subphase))
  
  startTime <- proc.time()  # Get time for runtime measurement
  cat("Calculating the subphase for each frame...\n")
  
  # Iterate over each Trial and Phase in the phase timing file
  for (trial in unique(phaseTiming$Trial)) {
    for (phase in unique(phaseTiming$Phase)) {
      
      # These are the rows in the Tobii data that have the current trial and phase
      tobiirows <- tobii$Trial==trial  &  tobii$Phase==phase
      
      # Pull the correct subphase timing rows -- these are ones where trial and phase match
      cur_phaseTiming <- phaseTiming[ phaseTiming$Trial==trial  &  phaseTiming$Phase==phase, ]
      
      # Sort in order of increasing starting time
      cur_phaseTiming <- cur_phaseTiming[order(cur_phaseTiming$Starting_time), ]
      
      # Iterate over each of the timings (e.g., 0, 4, and 7.85 seconds) and set the
      #  respective subphase for all rows of tobii data where the phase timing is greater or equal
      #  to these values. This loop works by overwriting the entries for larger times.
      for (i in 1:nrow(cur_phaseTiming)) {
        change_idx <- tobiirows  &  tobii$TimeFromPhaseOnset >= cur_phaseTiming$Starting_time[i]*1000
        Subphase[change_idx] <- cur_phaseTiming$Subphase[i]
      }
    }
  }
  
  tobii$Subphase <- Subphase
  
  cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime)[3], digits=1)))
  
  ### ADD SUBPHASE TIMINGS ###
  # Now that we know where the subphases are, we can add timings, in the same way we did
  #  MovieName, Trial, and Phase timings, above.
  
  startTime <- proc.time()  # Get time for runtime measurement
  cat("Calculating frame count and timing for Subphase...\n")
  
  # Create temporary vectors to store this stuff, and later store a copy of that vector as a new column
  # in the data frame.
  FramesFromSubphaseOnset <- vector(mode="numeric", length=nrow(tobii))
  # Start with count of zero for all
  FramesFromSubphaseOnset[1:nrow(tobii)] <- 0    
  
  for (ParticipantName in unique(tobii$ParticipantName)) {
    for (condition in levels(tobii$Condition)) {
      for (trial in levels(tobii$Trial)) {
        for (phase in levels(tobii$Phase)) {
          for (subphase in levels(tobii$Subphase)) {
            # Take just the subset for this subphase
            subphaserows <- tobii$ParticipantName==ParticipantName  & tobii$Condition==condition &
              tobii$Trial==trial            & tobii$Phase==phase  &
              tobii$Subphase==subphase
            subphaserows[ is.na(subphaserows) ] <- FALSE
            
            # Add the frame numbering
            FramesFromSubphaseOnset[subphaserows] <- 0:(sum(subphaserows)-1)
          }
        }
      }
    }
  }
  
  # Store the vector as a new column in the data frame
  tobii$FramesFromSubphaseOnset <- FramesFromSubphaseOnset
  
  # Also calculate the time (in milliseconds) from frame. The data is collected at 60Hz
  tobii$TimeFromSubphaseOnset <- round(FramesFromSubphaseOnset*1000/60)
  
  cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime)[3], digits=1)))
  
  ### LOAD AOI ###
  
  aoi = read.table(file=aoifile, sep="\t", header=TRUE, fill=TRUE)
  
  # Do regexp replacements to find the minimum and maximum X and Y values that mark the area of interest
  # The Bottom Left and Top Right columns are used; the Bottom Right and Top Left are ignored
  aoi$xmin <- as.numeric(sub(",.*$", "", as.character(aoi$Top.Left..x.y.)))
  aoi$ymin <- as.numeric(sub("^.*,", "", as.character(aoi$Top.Left..x.y.)))
  aoi$xmax <- as.numeric(sub(",.*$", "", as.character(aoi$Bottom.Right..x.y.)))
  aoi$ymax <- as.numeric(sub("^.*,", "", as.character(aoi$Bottom.Right..x.y.)))
  
  # Set values for GazeXAvg and GazeYAvg, based on the left and right eye values, and the validity for each.
  #
  # If ValidityLeft and ValidityRight are both either 0 or 1 (decent validity), take the average gaze of the eyes combined.
  # If only one of ValidityLeft and ValidityRight is either 0 or 1, use just that eye's data.
  # If neither of ValidityLeft and ValidityRight are 0 or 1, put in -999.
  
  bothValid      <-  (tobii$ValidityLeft %in% 0:1) &  (tobii$ValidityRight %in% 0:1)
  leftOnlyValid  <-  (tobii$ValidityLeft %in% 0:1) & !(tobii$ValidityRight %in% 0:1)
  rightOnlyValid <- !(tobii$ValidityLeft %in% 0:1) &  (tobii$ValidityRight %in% 0:1)
  neitherValid   <- !(tobii$ValidityLeft %in% 0:1) & !(tobii$ValidityRight %in% 0:1)
  
  # Verify that gaze points in AOI file are properly set
  # If improperly set, issue a warning.
  #
  # added by Brock (Jan 09, 2012)
  
  if (!is.vector(aoi$xmin)) {
    warning(sprintf("GazePointXLeft in AOI file is invalid (value: \"%s\").", aoi$xmin))
  }
  
  if (!is.vector(aoi$xmax)) {
    warning(sprintf("GazePointXRight in AOI file is invalid (value: \"%s\").", aoi$xmax))
  }
  
  if (!is.vector(aoi$ymin)) {
    warning(sprintf("GazePointYLeft in AOI file is invalid (value: \"%s\").", aoi$ymin))
  }
  
  if (!is.vector(aoi$ymax)) {
    warning(sprintf("GazePointYRight in AOI file is invalid (value: \"%s\").", aoi$ymax))
  }
  
  tobii$GazeXAvg[bothValid]      <- (tobii$GazePointLeftX[bothValid] + tobii$GazePointRightX[bothValid])/2
  tobii$GazeYAvg[bothValid]      <- (tobii$GazePointLeftY[bothValid] + tobii$GazePointRightY[bothValid])/2
  
  tobii$GazeXAvg[leftOnlyValid]  <- tobii$GazePointLeftX[leftOnlyValid]
  tobii$GazeYAvg[leftOnlyValid]  <- tobii$GazePointLeftY[leftOnlyValid]
  
  tobii$GazeXAvg[rightOnlyValid] <- tobii$GazePointRightX[rightOnlyValid]
  tobii$GazeYAvg[rightOnlyValid] <- tobii$GazePointRightY[rightOnlyValid]
  
  tobii$GazeXAvg[neitherValid]   <- -999
  tobii$GazeYAvg[neitherValid]   <- -999
  
  # Add a couple more columns which will later be stored in the tobii data frame. (They are kept separate now for speed)
  # Their values are taken from the AOI file.
  # They will be filled in below, when the specific AOI is found for each frame.
  
  SceneName <- factor(vector(length=nrow(tobii)), levels=c(levels(aoi$SceneName), "TrackLoss"))
  Position  <- factor(vector(length=nrow(tobii)), levels=c(levels(aoi$Position),  "TrackLoss"))
  SceneType <- factor(vector(length=nrow(tobii)), levels=c(levels(aoi$SceneType), "TrackLoss"))
  
  # Get time for runtime measurement
  
  startTime <- proc.time()
  cat("Finding which AOI the person is looking at for each frame...\n")
  
  # Iterate over the areas of interest for each kind of trial and find
  # which rows of data have eye gazes within each of the AOI's.
  
  for (trial in aoi$Trial) {
    # Get the areas of interest for each trial
    trial_aois <- aoi[aoi$Trial==trial,]
    
    # Now locate all rows from the tobii data that are in this trial
    trialrows <- tobii$Trial==trial
    trialrows[is.na(trialrows)] <- FALSE
    
    # Get the X and Y coordinates for these rows
    x <- tobii$GazeXAvg[trialrows]
    y <- tobii$GazeYAvg[trialrows]
    
    # Check for each AOI - are the X and Y coordinates within the AOI?
    for (i in 1:nrow(trial_aois)) {
      inAOIrows <- x >= trial_aois$xmin[i]  &  x <= trial_aois$xmax[i]  &
        y >= trial_aois$ymin[i]  &  y <= trial_aois$ymax[i]
      
      # This double-reverse mapping is a little tricky
      inTrialAndAOI <- trialrows
      inTrialAndAOI[ inTrialAndAOI==TRUE ] <- inAOIrows
      
      SceneName[inTrialAndAOI] <- trial_aois$SceneName[i]
      Position [inTrialAndAOI] <- trial_aois$Position [i]
      SceneType[inTrialAndAOI] <- trial_aois$SceneType[i]
    }  
  }
  cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime)[3], digits=1)))
  
  # Store the temporary factors back in the data frame.
  tobii$SceneName <- SceneName
  tobii$Position  <- Position
  tobii$SceneType <- SceneType
  
  # If both eyes were invalid, there was TrackLoss
  tobii$SceneName[neitherValid] <- "TrackLoss"
  tobii$Position [neitherValid] <- "TrackLoss"
  tobii$SceneType[neitherValid] <- "TrackLoss"
  
  # add trial numbers
  tobii$TrialNumber <- 0
  subjects <- unique(tobii$ParticipantName)
  for (subject in subjects) {
    trial_number <- 1
    trials <- unique(as.character(tobii[which(tobii$ParticipantName == subject), 'Trial']))
    
    for (trial in trials) {
      if (!is.na(trial) & trial != '') {
        tobii[which(tobii$Trial == trial & tobii$ParticipantName == subject), 'TrialNumber'] <- trial_number
        trial_number <- trial_number + 1
      }
    }
  }
  
  ### WRITE RESULTS ###
  if (!is.null(outputfile)) {
    startTime <- proc.time()  # Get time for runtime measurement
    cat(sprintf("Writing results to file %s...\n", outputfile))
    
    write.csv(tobii, outputfile, row.names=FALSE)
    
    cat(sprintf("Finished. (%.1f seconds)\n", round((proc.time()-startTime)[3], digits=1)))
  }
  
  cat(sprintf("Finished processing data. Total elapsed time: %.1f seconds.\n", round((proc.time()-startTime1)[3], digits=1)))
  
  tobii
}

#
# prepare_master()
#
# This file performs some necessary preparations on a master file.
#   1) Add new columns for each scenetype, with 1's, 0's, and NA's (ignores TrackLoss).
#   2) Merge subject information from CSV file (by ParticipantName)
#   3) Merge trial condition information from CSV file (by Trial)
prepare_master <- function (subjectfile = FALSE, trialfile = FALSE, inputfile='master.csv', outputfile='master-prepared.csv') {
  tobii <- read.csv(inputfile)
  
  # 1) create new columns based on types of SceneType
  scenetypes <- levels(tobii$SceneType)
  
  for (scenetype in scenetypes) {
    if (scenetype != "") {
      tobii[,scenetype] <- NA
      
      tobii[which(tobii$SceneType == scenetype),scenetype] <- 1
    }
  }
  
  # for all columns except TrackLoss, we want to set a 0 for all in-line other columns
  for (scenetype in scenetypes) {
    for (scenetype2 in scenetypes) {
      if (scenetype2 != "TrackLoss" && scenetype != "TrackLoss" && scenetype != "" && scenetype2 != "" && scenetype != scenetype2) {
        tobii[which(tobii$SceneType == scenetype), scenetype2] <- 0
      }
    }
  }
  
  # 2) Merge subject information
  if (subjectfile != FALSE) {
    subjects <- read.csv(subjectfile)
    
    # does ParticipantName column exist?
    if (is.na(match("ParticipantName", names(subjects)))) {
      warning("We need a column called ParticipantName in order to merge subject information into the file.")
    }
    else {
      tobii <- merge(tobii, subjects, by=c("ParticipantName"), all.x = TRUE)
    }
  }
  
  # 3) Merge trial condition information
  if (trialfile != FALSE) {
    trials <- read.csv(trialfile)
    
    # does ParticipantName column exist?
    if (is.na(match("Trial", names(trials)))) {
      warning("We need a column called Trial in order to merge trial condition information into the file.")
    }
    else {
      tobii <- merge(tobii, trials, by=c("Trial"), all.x = TRUE)
    }
  }
  
  # sort results
  tobii <- tobii[order(tobii$ParticipantName, tobii$RecordingTimestamp), ]
  
  write.csv(tobii, outputfile, row.names=FALSE)
}

find_looks <- function(inputfile, outputfile=NULL, subjectnums=NULL, conditions=NULL, trials=NULL, phases=NULL) {
  processedData <- read.csv(inputfile)
  
  # If the particular instances of these variables aren't specified, use all of them
  if (is.null(subjectnums)) {  subjectnums = unique(processedData$ParticipantName)  }
  if (is.null(conditions))  {  conditions  = levels(processedData$Condition)   }
  if (is.null(trials))      {  trials      = levels(processedData$Trial)       }
  if (is.null(phases))      {  phases      = levels(processedData$Phase)       }
  
  # For the purposes of this function, TrackLoss is the same as NA, so we'll just replace it here
  processedData$SceneType[ processedData$SceneType=="TrackLoss" ] <- NA
  processedData[which(processedData$TrackLoss == 1), 'SceneType'] <- NA
  
  # ================== Smooth the data within each phase =================
  
  cat("Smoothing over one-frame gaps in SceneType...\n")
  
  # Iterate over each of the following
  for (subjectnum in subjectnums) {
    for (condition in conditions) {
      for (trial in trials) {
        for (phase in phases) {
          # Find which rows of data are in this phase
          phaserows <- processedData$ParticipantName==subjectnum  & processedData$Condition==condition &
            processedData$Trial==trial            & processedData$Phase==phase
          # Replace the NA's with FALSE
          phaserows[ is.na(phaserows) ] <- FALSE
          
          # If there's no data to look at, skip the rest of this (otherwise will crash)
          if (sum(phaserows) == 0) next();
          
          # Get all the rows of data that are in this particular phase
          phaseData <- processedData[phaserows,]
          
          # Find all the NA's for SceneType in this phase
          pdNA <- is.na(phaseData$SceneType)
          
          # Wherever there's an NA, look at the SceneType of the previous and next frame.
          # If they're the same (and not NA or TrackLoss), then fill in the gap
          # with that SceneType.
          
          # Shift the SceneType one to the left or right
          SceneTypePrev <- c(phaseData$SceneType[2:length(phaseData$SceneType)], NA)
          SceneTypeNext <- c(NA, phaseData$SceneType[1:(length(phaseData$SceneType)-1)])
          
          # These are the rows that need to be filled
          gaps <- pdNA & SceneTypePrev == SceneTypeNext
          gaps[is.na(gaps)] <- FALSE
          
          cat(sprintf("Subject %s | Condition %s | Trial %s | Phase %s | Gaps found: %d\n",
                      subjectnum, condition, trial, phase, sum(gaps)))
          
          # Now find all the rows that are just before each of the gaps. This is where
          # we'll get the SceneType (to fill the gaps) from
          gapsPrev <- c(gaps[2:length(gaps)], FALSE)
          
          # fill in the gaps with the entry from the previous frame
          phaseData$SceneType[gaps] <- phaseData$SceneType[gapsPrev]
          
          # Move it out to the main data frame
          processedData$SceneType[phaserows] <- phaseData$SceneType
          
        }
      }
    }
  }
  
  # ================== Look for runs within each subphase =================
  
  cat("\n\nLooking for runs (looks) in each subphase...\n")
  
  # This will be used to store all the runs
  Runs <- NULL
  
  # Iterate over each of the following
  for (subjectnum in subjectnums) {
    for (condition in conditions) {
      for (trial in trials) {
        for (phase in phases) {
          # Find which rows of data are in this phase
          phaseRows <- with(processedData,
                            ParticipantName==subjectnum & Condition==condition & Trial==trial & Phase==phase)
          # Replace the NA's with FALSE
          phaseRows[is.na(phaseRows)] <- FALSE
          
          # Get all the rows of data that are in this particular phase
          phaseData <- processedData[phaseRows,]
          
          # Make sure it's sorted by frame count
          phaseData <- phaseData[order(phaseData$FramesFromPhaseOnset),]
          
          for (subphase in levels(phaseData$Subphase)) {
            cat(sprintf("Subject %s | Condition %s | Trial %s | Phase %s | Subphase \"%s\"\n",
                        subjectnum, condition, trial, phase, subphase))
            
            # This block of data is a single subphase within a phase, within a trial
            spRows <- phaseData$Trial==trial & phaseData$Subphase==subphase
            spRows[is.na(spRows)] <- FALSE
            
            # If there's no data for this particular combination of trial/phase/subphase,
            #  or if all the AOI entries are NA or TrackLoss (which means there were no looks),
            #  skip the rest of this inner loop
            if (sum(spRows) == 0  ||  sum(!is.na(phaseData$SceneType[spRows]))==0 ) { next() }
            
            # Check to make sure there's no carryover from the previous subphase.
            carryover <- FALSE
            # Get the suphase for the first frame
            idx1 <- min(which(spRows))  # The index of the first sample in this subphase
            idx0 <- idx1-1              # The index of the last sample in the previous subphase
            if (idx0 != 0  && 
              !is.na(phaseData$SceneType[idx1])  &&  !is.na(phaseData$SceneType[idx0]) &&
              phaseData$SceneType[idx1] == phaseData$SceneType[idx0])
            {
              carryover <- TRUE
              cat("    Look carried over from previous subphase\n")
            }
            
            # Pull out just the rows from the phase data for this subphase
            spData <- phaseData[spRows, ]    
            
            # These vectors will keep track of each look in the subphase
            # - The SceneType of the area of interest (AOI) being looked at
            SceneType       <- factor(levels=levels(spData$SceneType))
            # - The start and ending frame numbers for each look
            startFrame <- vector(mode="numeric")
            endFrame   <- vector(mode="numeric")
            
            # Now iterate over spData and search for runs of SceneType
            prevSceneType <- NA
            curSceneType  <- NA
            for (i in 1:nrow(spData)) {
              prevSceneType <- curSceneType
              curSceneType  <- spData$SceneType[i]
              
              if (is.na(curSceneType)) {
                if (!is.na(prevSceneType)) {
                  # If the previous wasn't NA or TrackLoss, then end the run
                  endFrame <- append(endFrame, i-1)
                  cat(sprintf("%d\n", i-1))
                }
              } else {
                if (is.na(prevSceneType)) {
                  # If the previous SceneType is NA or TrackLoss, this is the start of a new run
                  # Have to use this fancy stuff instead of append() because append only works with vectors
                  SceneType [length(SceneType) +1] <- curSceneType
                  startFrame[length(startFrame)+1] <- i
                  cat(sprintf("    Look found. %s | Frame %d-", curSceneType, i))
                  
                } else if (prevSceneType != curSceneType) {
                  # If the previous SceneType isn't NA, but it is different from
                  #  the current one, end the old run and start a new one
                  endFrame  [length(endFrame)  +1] <- i-1
                  SceneType [length(SceneType) +1] <- curSceneType
                  startFrame[length(startFrame)+1] <- i
                  
                  cat(sprintf("%d\n", i-1))
                  cat(sprintf("    Look found. %s | Frame %d-", curSceneType, i))
                  
                }
              }
            }
            
            # If we were in the middle of an SceneType run, end it.
            if ( !is.na(curSceneType)) {
              endFrame[length(endFrame)+1]     <- i-1
              cat(sprintf("%d\n", i-1))
            }
            
            # Make a data frame of all the runs/looks in this subphase
            RunsSP <- data.frame(ParticipantName=subjectnum, Condition=condition, 
                                 Trial=trial, TrialNumber=phaseData[1,'TrialNumber'], Phase=phase, Subphase=subphase, 
                                 SceneType=SceneType, StartFrame=startFrame, EndFrame=endFrame)
            
            RunsSP$LengthFrames <- RunsSP$EndFrame - RunsSP$StartFrame + 1
            RunsSP$StartTime    <- round(RunsSP$StartFrame   * 1000/60)
            RunsSP$EndTime      <- round(RunsSP$EndFrame     * 1000/60)
            RunsSP$LengthTime   <- round(RunsSP$LengthFrames * 1000/60)
            RunsSP$FixationNum  <- 1:nrow(RunsSP)
            
            # ====================== Rank Look Length =======================
            
            # Rank the length within each subphase, across all scenetypes (e.g., Action/Object)
            # Start these at a maximum value -- they will be replaced later
            RunsSP$RankLengthSubphase             <- 9999
            RunsSP$RankLengthSubphase_NoCarryover <- 9999
            
            RunsSP$RankLengthSubphase <- rank(-RunsSP$LengthFrames, ties.method="first")
            if (carryover) {
              RunsSP$RankLengthSubphase_NoCarryover[RunsSP$StartFrame!=1] <- rank(-RunsSP$LengthFrames[RunsSP$StartFrame!=1], ties.method="first")
            } else {
              RunsSP$RankLengthSubphase_NoCarryover <- RunsSP$RankLengthSubphase
            }
            
            
            
            # Rank the length within each scene type (e.g., Action/Object) within each subphase
            RunsSP$RankLengthScenetype             <- 9999
            RunsSP$RankLengthScenetype_NoCarryover <- 9999
            
            for (scenetype in levels(RunsSP$SceneType)) {
              
              # Rank the runs by length
              # These are the looks to this SceneType
              SceneTypelooks <- RunsSP$SceneType==scenetype
              RunsSP$RankLengthScenetype[SceneTypelooks] <-
                rank(-RunsSP$LengthFrames[SceneTypelooks], ties.method="first")
              
              # Rank the runs by length -- but only if initiated WITHIN this subphase --
              #  that is, if there was carryover AND StartFrame is 1, ignore it.
              # These are the looks to this SceneType, excluding startframe=1 cases
              if (carryover) {
                SceneTypelooks <- RunsSP$SceneType==scenetype & RunsSP$StartFrame!=1
                RunsSP$RankLengthScenetype_NoCarryover[SceneTypelooks] <-
                  rank(-RunsSP$LengthFrames[SceneTypelooks], ties.method="first")
              } else {
                RunsSP$RankLengthScenetype_NoCarryover <- RunsSP$RankLengthScenetype
              }
              
            }
            
            
            
            # Sort by SceneType and RankLengthScenetype
            RunsSP <- RunsSP[order(RunsSP$SceneType, RunsSP$RankLengthScenetype),]
            
            Runs <- rbind(Runs, RunsSP)
            
            
          }
        }
      }
    }
  }
  
  
  if (!is.null(outputfile)) {
    write.csv(Runs, outputfile, row.names=FALSE)
  }
  
  Runs
}

first_looks <- function(looks, subjectfile = FALSE, trialfile = FALSE) {
  # clean up the looks dataframe
  
  # aggregate by participants>trials>scenetypes and take the
  # MIN(frames) values for each scenetype
  looks <- aggregate(data.frame(looks$StartTime,looks$EndTime), by = list(looks$ParticipantName, looks$Condition, looks$Trial, looks$TrialNumber, looks$Phase, looks$Subphase, looks$SceneType), FUN = min)
  colnames(looks) <- c('ParticipantName','Condition','Trial','TrialNumber','Phase','Subphase','SceneType','StartTime','EndTime')
  
  # re-sort
  looks <- looks[order(looks$ParticipantName, looks$Condition, looks$Trial, looks$TrialNumber, looks$Phase, looks$Subphase, looks$StartTime, looks$EndTime), ]
  
  # add the FixationNum column
  looks$FixationNum <- 0
  
  max_fixations <- length(unique(looks$SceneType))
  fixation_vector <- c(1:max_fixations)
  
  subjectnums <- unique(looks$ParticipantName)
  conditions  <- unique(looks$Condition)
  trials      <- unique(looks$Trial)
  phases      <- unique(looks$Phase)
  subphases   <- unique(looks$Subphase)
  scenetypes  <- unique(looks$SceneType)
  
  for (subjectnum in subjectnums) {
    for (condition in conditions) {
      for (trial in trials) {
        for (phase in phases) {
          for (subphase in subphases) {
            rows <- length(looks[which(looks$ParticipantName == subjectnum & looks$Condition == condition & looks$Trial == trial & looks$Phase == phase & looks$Subphase == subphase), 'FixationNum'])
            looks[which(looks$ParticipantName == subjectnum & looks$Condition == condition & looks$Trial == trial & looks$Phase == phase & looks$Subphase == subphase), 'FixationNum'] <- fixation_vector[0:rows]
          }
        }
      }
    }
  }
  
  # get all first looks
  first_looks <- looks[which(looks$FixationNum == 1), 1:8]
  colnames(first_looks)[7] <- 'FirstSceneType'
  colnames(first_looks)[8] <- 'FirstStartTime'
  
  # get all second looks
  second_looks <- looks[which(looks$FixationNum == 2), 1:9]
  colnames(second_looks)[7] <- 'SecondSceneType'
  colnames(second_looks)[8] <- 'SecondStartTime'
  colnames(second_looks)[9] <- 'SecondEndTime'
  
  # merge first and second looks, getting rid of non-switch trials
  merged <- merge(first_looks, second_looks, by = c(colnames(first_looks)[1:6]))
  
  # calculate switch time
  merged$SwitchTime <- merged$SecondStartTime - merged$FirstStartTime
  
  # merge subject information?
  if (subjectfile != FALSE) {
    subjects <- read.csv(subjectfile)
    
    # does ParticipantName column exist?
    if (is.na(match("ParticipantName", names(subjects)))) {
      warning("We need a column called ParticipantName in order to merge subject information into the file.")
    }
    else {
      merged <- merge(merged, subjects, by=c("ParticipantName"), all.x = TRUE)
    }
  }
  
  # merge trial information?
  if (trialfile != FALSE) {
    trials <- read.csv(trialfile)
    
    # does ParticipantName column exist?
    if (is.na(match("Trial", names(trials)))) {
      warning("We need a column called Trial in order to merge trial condition information into the file.")
    }
    else {
      merged <- merge(merged, trials, by=c("Trial"), all.x = TRUE)
    }
  }
  
  merged
}

#
# concat_csv()
#
# This function is used to concatenate data files. The headers are taken from the first file only.
# It takes as input ALL 'processed-XXX.csv' files in the working directory.
# It does not check if the subsequent files have the same headers, or even if they have the same number
# of columns.
#
# Example usage:
#   concat_csv('master.csv')
concat_csv <- function(outputfile, verbose = TRUE, na.rm = TRUE) {
  inputfiles <- list.files(path=getwd(), pattern="processed-", all.files=T, full.names=F)
  
  if (na.rm == TRUE) {
    nas <- ""
  }
  else {
    nas <- "NA"
  }
  
  # Load and write each inputfile
  # Use write.table instead of write.csv, because write.csv doesn't allow you to supress headers.
  for (i in 1:length(inputfiles)) {
    if (i==1) {
      if (verbose) cat(sprintf("%s: writing with headers.\n", inputfiles[[i]]))
      write.table( read.csv(inputfiles[[i]]), outputfile, sep=",", na=nas, row.names=FALSE)
    }
    else {
      if (verbose) cat(sprintf("%s: appending without headers.\n", inputfiles[[i]]))
      write.table( read.csv(inputfiles[[i]]), outputfile, sep=",", na=nas, append=TRUE, col.names=FALSE, row.names=FALSE)
    }
  }
}

#
# convert_tobii()
#
# Converts an old Tobii dataset to a new Tobii dataset
# Place all files in a subfolder of our normal working directory (e.g., /old_files)
# and call like so:
# 
# convert_files('old_files')
#
convert_files <- function (sub_folder, condition_regex = 'M[a-zA-Z]+_[A-Z]', filename_prefix = 'TNV24M_') {
  files <- list.files(path=as.character(paste(getwd(),'/',sub_folder,sep="")), pattern=".tsv", all.files=T, full.names=F)
  
  for (file in files) {
    cat("Extracting participant and session variables...\n")
    
    # read in Tobii file with all properties in header
    tobii <- read.table(file=paste(sub_folder,'/',file,sep=""), sep="\t", fill=TRUE, row.names = NULL)
    
    # initiate variables with empty values
    gender = NA;
    participant = NA;
    date = NA;
    
    for (i in 1:nrow(tobii)) {
      property <- tobii[i, "V1"]
      value <- tobii[i, "V2"]
      
      if (property == 'Gender:') {
        gender = gsub(" ","", value)
      }
      else if (property == 'Recording date:') {
        date = gsub(" ","", value)
      }
      else if (property == 'Recording name:') {
        recording_name = gsub(" ","", value)
      }
      else if (property == 'Participant:') {
        participant = gsub(" ","", value)
      }
      else if (property == 'Timestamp') {
        # we have reached the column headers, so let's record this place (so we
        # know where to import from) and stop parsing the file
        import_skip_headers <- as.integer(i - 1)
        break
      }
      
      if (i > 100) {
        # something's gone horribly wrong - this should all be sorted well before this
        stop("Failure to read headers properly.")
      }
    }
    
    # Extract the subject number from participant string (e.g., "TNV24_14" has subject number 14)
    # by replacing TNV24_ with nothing.
    
    subjectNum <- participant
    
    # Verify that the subject number exists
    if (is.na(subjectNum)) {
      stop(sprintf("No subject number extracted from Participant string \"%s\" from Tobii data file!", participant))
      cat("  No subject number found! The output file will probably have problems.\n")
      cat("  Make sure that the Participant line has the format XXXXX_YY, where YY is the subject number.\n")
    }
    
    # remove initially read file imported above for properties
    rm(tobii)
    
    cat("Loading raw Tobii data file into data frame object...\n")
    
    tobii <- read.table(file=paste(sub_folder,'/',file,sep=""), sep="\t", header=TRUE, fill=TRUE, skip=import_skip_headers)
    
    # now make appropriate changes
    
    condition <- regexpr(condition_regex, file)
    condition <- substr(file, condition, attr(condition, 'match.length'))
    
    if (is.na(condition)) {
      stop("Unable to find condition in filename using REGEX!")
    }
    
    tobii$StudioTestName <- condition
    tobii$ParticipantName <- as.character(subjectNum)
    tobii$Gender <- gender
    tobii$RecordingName <- as.character(recording_name)
    tobii$RecordingDate <- as.character(date)
    tobii$RecordingDuration <- max(tobii$Timestamp)
    
    # create new dataframe
    empty <- matrix(ncol = 1, nrow = nrow(tobii))
    
    tobii_new <- data.frame(tobii$StudioTestName, tobii$ParticipantName, tobii$Gender, tobii$RecordingName, tobii$RecordingDate, tobii$RecordingDuration, tobii$StimuliName, tobii$Timestamp, tobii$DateTimeStamp, tobii$Event, empty, empty, empty, empty, empty, empty, empty, empty, empty, tobii$GazePointXLeft, tobii$GazePointYLeft, tobii$GazePointXRight, tobii$GazePointYRight, tobii$GazePointX, tobii$GazePointY, empty, empty, tobii$ValidityLeft,tobii$ValidityRight)
    colnames(tobii_new) <- c('StudioTest','ParticipantName','Gender','RecordingName','RecordingDate','RecordingDuration','MediaName','RecordingTimestamp','LocalTimeStamp','StudioEvent','FixationIndex','SaccadeIndex','GazeEventType','GazeEventDuration','FixationPointX','FixationPointY','AOI[Left AOI]Hit','AOI[Right AOI]Hit','GazePointIndex','GazePointLeftX','GazePointLeftY','GazePointRightX','GazePointRightY','GazePointX','GazePointY','PupilLeft','PupilRight','ValidityLeft','ValidityRight')
    
    # write new file
    participantNumberLocation = regexpr("[0-9]", file)
    participantNumber = substr(file, participantNumberLocation, participantNumberLocation + attr(participantNumberLocation, 'match.length') - 1)
    
    filename <- paste(filename_prefix, condition, '_', participantNumber,'.tsv',sep="")
    write.table(tobii_new, filename, sep = "\t")
  }
}