#'loadAndProcessDataset.r
#'
#'(C) 2017 - Imad Khoury
#'
#'Rscript command prompt method
#'loadAndProcessDataset(c("10","KAGGLE","dummy_mandatory")); #other options: LUNA, LUNAEXTRA
#**************************************************#
library(jsonlite);
settings_url <- "D:/Kaggle/Bowl2017/SETTINGS.json";#
settings_paths <- fromJSON(txt=settings_url);      #
#**************************************************#

setwd(settings_paths$R_code_paths$working_directory);
library(stringr)
library(methods)

par(ask=F);#turn-off plot asking
proj <- NULL;

PRINT_BATCH_SIZE <- 5000;

#function - load & process
loadAndProcessDataset <- function(args){
  if (length(args)<3) {
    stop("At least two arguments must be supplied (a session id then a list of dataset names)", call.=FALSE);
  } 
  if(is.na(as.numeric(args[1]))){
    stop("The session id must be numeric!");
  }    
  countNAS <- 0;
  countNAS_test <- 0;
  
  #=============================================TESTSET========================
  proj$dataset_KAGGLE_TEST_SET <- NULL;
  TOTAL_DATASET_LEN_TESTSET <- 0;
  #Load TEST SET BLOBS
  h<-NULL; dframe<-NULL; blobs_list<-NULL;
  results_dir<-NULL; results_dir <- "RESULTS";
  meta_csv_dir<-NULL; meta_csv_dir <- settings_paths$R_code_paths$meta_csv_final_directory;
  meta_csv_file<-NULL; meta_csv_file <- paste(meta_csv_dir, "/", "FINAL_FINAL.csv", sep="");
  meta_frame <-read.csv(meta_csv_file, na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
  res_path<-NULL; res_path<-settings_paths$test_paths$stage1_images_dir;
  dcm_list<-NULL; dcm_list <- list.dirs(path = res_path, recursive = FALSE);
  TOTAL_DATASET_LEN_TESTSET <- TOTAL_DATASET_LEN_TESTSET + length(dcm_list);
  for(h in 1:length(dcm_list)){
    if(!is.na(dcm_list[h])){
      dcm_dir <- dcm_list[h];
      proj$results_path<-NULL; proj$results_path <-paste(dcm_dir,"\\",results_dir, sep="");
      blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
      for(j in 1:1){
        if(length(blobs_list)!=0){
          dframe<-NULL;
          info<-NULL; info = file.info(paste(proj$results_path, "\\",blobs_list[j], sep=""));
          info <- info$size;
          if(!is.na(info)){
            if(as.numeric(info) > 3){
              if(dir.exists(proj$results_path)){
                dframe <- read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                dframe <-dframe[which(dframe$Area.equivalent.circle.diameter >= 3 & dframe$Area.equivalent.circle.diameter <= 30),];
                dframe <-dframe[which(dframe$Elong. <= 0.6),];
                if(nrow(dframe)==0){
                  dframe <- read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                  dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
                }
                dframe$ct_file_name <- str_split(str_split(dcm_dir,"/")[[1]][5],"\\/")[[1]][2];
                #dframe$total_slices <- meta_frame$num_slices[which(meta_frame$ct_file_name == dframe$ct_file_name)];
              #  dframe$total_blobs <- meta_frame$num_blobs[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                #  dframe$mean_bits_stored <- meta_frame$ctmeta_bits_stored_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                #  dframe$mean_slice_location <- meta_frame$ctmeta_slice_location_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
  
                proj$dataset_KAGGLE_TEST_SET <- rbind(proj$dataset_KAGGLE_TEST_SET, dframe);
                if(h%%50 ==0){print(paste("Loading & processing KAGGLE_TEST_BLOBS_SET ", h, ".../", nrow(dframe), "..."))};
              }
            }else{
              countNAS_test <- countNAS_test +1;
              heading <- c("X.1", "Frame", "Label", "X", "Y", "Area", "Area.Conv..Hull", 
                           "Peri.", "Peri..Conv..Hull", "Feret", "Min..Feret", "Maximum.inscriped.circle.diameter", 
                           "Area.equivalent.circle.diameter", "Long.Side.Length.MBR", "Short.Side.Length.MBR", 
                           "Aspect.Ratio", "Area.Peri.", "Circ.", "Elong.", "Convexity", 
                           "Solidity", "Num..of.Holes", "Thinnes.Rt.", "Contour.Temp.", 
                           "Orientation", "Fract..Dim.", "Fract..Dim..Goodness", "ct_file_name"
              );
              notavs<- rep(0, length(heading));
              notavs[length(heading)] <- str_split(str_split(dcm_dir,"\\\\")[[1]][5],"\\/")[[1]][2];
              dframe <- NULL;
              dframe <-data.frame(t(notavs));
              colnames(dframe)<-heading;
              proj$dataset_KAGGLE_TEST_SET <- rbind(proj$dataset_KAGGLE_TEST_SET, dframe);
            }
          }
        }
      }
    }
  }
  
  DATASET_LEN_TESTSET <- NULL;
  proj$dataset_testset <- NULL;
  proj$dataset_testset <- rbind(proj$dataset_testset, proj$dataset_KAGGLE_TEST_SET);
  DATASET_LEN_TESTSET<- nrow(proj$dataset_testset);
  print(paste("TESTSET DF_LEN:", DATASET_LEN_TESTSET, " from ", TOTAL_DATASET_LEN_TESTSET));
  
  #=============================================CANCER========================
  proj$blobs_features <-NULL;
  proj$dataset_LUNA_CANCER <- NULL;
  proj$dataset_LUNA_EXTRA_CANCER <- NULL;
  proj$dataset_KAGGLE_CANCER <- NULL;
  
  TOTAL_DATASET_LEN_CANCER <- 0;

  if(sum(grepl("LUNA", args)) == 1){
    
    #Load CANCER Blobs
    h<-NULL; dframe<-NULL; blobs_list<-NULL;
    results_dir<-NULL; results_dir <- "RESULTS_ANNOTATIONS/";
    proj$results_path<-NULL; proj$results_path <-paste(settings_paths$R_code_paths$LUNA_root_directory,"IMAGE_OUTPUTS/", results_dir, sep="");
    proj$results_path <- str_replace(proj$results_path, "/Kaggle/Bowl2017","");
        blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
    TOTAL_DATASET_LEN_CANCER <- TOTAL_DATASET_LEN_CANCER + length(blobs_list);
    for(j in (1:length(blobs_list))){#length(blobs_list)){
      dframe<-NULL;
      dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
      dframe <- dframe[which(dframe$Area.equivalent.circle.diameter == max(dframe$Area.equivalent.circle.diameter)),];
      if(nrow(dframe)==0){
        dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
        dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
      }
      #Adjust pixels dimensions due to ImageJ BioFormatExporter:
      col_adj<-colnames(dframe)[c(4:15,17)];
      dframe[col_adj] = dframe[col_adj]*10^4;
      dframe$ct_file_name <- "dummy";
      #dframe$total_slices <- -1;
      #dframe$total_blobs <- 1;
      #dframe$mean_bits_stored <- -1;
      #dframe$mean_slice_location <- -1;
      #TODO metablobs metastats metaresults
      dframe$cancer <- 1;
      proj$dataset_LUNA_CANCER <- rbind(proj$dataset_LUNA_CANCER, dframe);
      if(j%%PRINT_BATCH_SIZE ==0){print(paste("Loading & processing LUNA_CANCER_BLOBS_SET ",j, ".../", nrow(dframe), "..."));}
    }
  }
  
  if(sum(grepl("LUNAEXTRA", args)) >= 1){
    #EXTRA2
    results_dir<-NULL; results_dir <- "RESULTS_EXTRA2_ANNOTATIONS";
    proj$results_path<-NULL; proj$results_path <-paste(settings_paths$R_code_paths$LUNA_root_directory,"IMAGE_OUTPUTS/", results_dir, sep="");
    blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
    TOTAL_DATASET_LEN_CANCER <- TOTAL_DATASET_LEN_CANCER + length(blobs_list);
    for(j in (1:length(blobs_list))){#length()){
      dframe<-NULL;
      dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
      dframe <- dframe[which(dframe$Area.equivalent.circle.diameter == max(dframe$Area.equivalent.circle.diameter)),];
      if(nrow(dframe)==0){
        dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
        dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
      }
      #Adjust pixels dimensions due to ImageJ BioFormatExporter:
      col_adj<-colnames(dframe)[c(4:15,17)];
      dframe[col_adj] = dframe[col_adj]*10^4;
      dframe$ct_file_name <- "dummy";
      # dframe$total_slices <- -1;
      #dframe$total_blobs <- 1;
      #dframe$mean_bits_stored <- -1;
      #dframe$mean_slice_location <- -1;
      #TODO metablobs metastats metaresults
      dframe$cancer <- 1;
      proj$dataset_LUNA_EXTRA_CANCER <- rbind(proj$dataset_LUNA_EXTRA_CANCER, dframe);
      if(j%%PRINT_BATCH_SIZE ==0){print(paste("Loading & processing LUNA_EXTRA2_CANCER_BLOBS_SET ", j, ".../", nrow(dframe), "..."));}
    }
  }
  
  if(sum(grepl("KAGGLE", args)) >= 1){
    
    
    #KAGGLESET
    results_dir<-NULL; results_dir <- "RESULTS";
    meta_csv_dir<-NULL; meta_csv_dir <- settings_paths$R_code_paths$meta_csv_final_directory;
    meta_csv_file<-NULL; meta_csv_file <- paste(meta_csv_dir, "/", "FINAL_FINAL.csv", sep="");
    meta_frame <-read.csv(meta_csv_file, na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
    res_path<-NULL; res_path<-settings_paths$train_paths$stage1_images_dir;
    dcm_list<-NULL; dcm_list <- list.dirs(path = res_path, recursive = FALSE);
    dcm_list <- dcm_list[which("CANCER"==as.vector(str_match(dcm_list, "CANCER")))]
    TOTAL_DATASET_LEN_CANCER <- TOTAL_DATASET_LEN_CANCER + length(dcm_list);
    for(h in (1:length(dcm_list))){#length(dcm_list)){
      if(!is.na(dcm_list[h])){
        dcm_dir <- dcm_list[h];
        proj$results_path<-NULL; proj$results_path <-paste(dcm_dir,"\\",results_dir, sep="");
        blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
        for(j in 1:1){
          if(length(blobs_list)!=0){
            dframe<-NULL;
            info<-NULL; info = file.info(paste(proj$results_path, "\\",blobs_list[j], sep=""));
            info <- info$size;
            if(!is.na(info)){
              if(as.numeric(info) > 3){
                if(dir.exists(proj$results_path)){
                  dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                  dframe <- dframe[which(dframe$Area.equivalent.circle.diameter >= 3 & dframe$Area.equivalent.circle.diameter <= 30),];
                  dframe <-dframe[which(dframe$Elong. <= 0.6),];
                  if(nrow(dframe)==0){
                    dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                    dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
                  }
                  dframe$ct_file_name <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
                  #dframe$total_slices <- meta_frame$num_slices[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                  #  dframe$total_blobs <- meta_frame$num_blobs[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                  #dframe$mean_bits_stored <- meta_frame$ctmeta_bits_stored_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                  #  dframe$mean_slice_location <- meta_frame$ctmeta_slice_location_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
    
                  dframe$cancer <- 1;
                  proj$dataset_KAGGLE_CANCER <- rbind(proj$dataset_KAGGLE_CANCER, dframe);
                  if(h%%PRINT_BATCH_SIZE >=0 && h%%PRINT_BATCH_SIZE <=5){print(paste("Loading & processing KAGGLE_CANCER_BLOBS_SET ", h, ".../", nrow(dframe), "..."));}
                }else{
                  countNAS <- countNAS +1;
                  heading <- c("X.1", "Frame", "Label", "X", "Y", "Area", "Area.Conv..Hull",
                               "Peri.", "Peri..Conv..Hull", "Feret", "Min..Feret", "Maximum.inscriped.circle.diameter",
                               "Area.equivalent.circle.diameter", "Long.Side.Length.MBR", "Short.Side.Length.MBR",
                               "Aspect.Ratio", "Area.Peri.", "Circ.", "Elong.", "Convexity",
                               "Solidity", "Num..of.Holes", "Thinnes.Rt.", "Contour.Temp.",
                               "Orientation", "Fract..Dim.", "Fract..Dim..Goodness", "ct_file_name","cancer"
                  );
                  notavs<- rep(NA, length(heading));
                  notavs[length(heading)-1] <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
                  notavs[length(heading)] <- 1;
                  dframe <-NULL;
                  dframe <-data.frame(t(notavs));
                  colnames(dframe)<-heading;
                  proj$dataset_KAGGLE_CANCER <- rbind(proj$dataset_KAGGLE_CANCER, dframe);
                }
              }
            }
          }
        }
      }
    }
    
    DATASET_LEN_CANCER <- NULL;
    proj$dataset_cancer <- NULL;
    proj$dataset_cancer <- rbind(proj$dataset_cancer, proj$dataset_LUNA_CANCER);
    proj$dataset_cancer <- rbind(proj$dataset_cancer, proj$dataset_KAGGLE_CANCER);
    DATASET_LEN_CANCER <- nrow(proj$dataset_cancer);
    
    print(paste("CANCER DF_LEN:", DATASET_LEN_CANCER, " from ", TOTAL_DATASET_LEN_CANCER));
    
    
    
    # #=============================================NORMAL========================
    proj$dataset_KAGGLE_NORMAL <- NULL;
    proj$dataset_LUNA_NORMAL <- NULL;
    TOTAL_DATASET_LEN_NORMAL <- 0;
    h<-NULL; dframe<-NULL; blobs_list<-NULL;
    #CADIDATES
    results_dir<-NULL; results_dir <- "RESULTS";
    meta_csv_dir<-NULL; meta_csv_dir <- settings_paths$R_code_paths$meta_csv_final_directory;
    meta_csv_file<-NULL; meta_csv_file <- paste(meta_csv_dir, "/", "FINAL_FINAL.csv", sep="");
    meta_frame <-read.csv(meta_csv_file, na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
    res_path<-NULL; res_path<-settings_paths$train_paths$stage1_images_dir;
    dcm_list<-NULL; dcm_list <- list.dirs(path = res_path, recursive = FALSE);
    dcm_list <- dcm_list[which("NORMAL"==as.vector(str_match(dcm_list, "NORMAL")))];
    TOTAL_DATASET_LEN_NORMAL <- TOTAL_DATASET_LEN_NORMAL + length(dcm_list);
    for(h in (1):(length(dcm_list))){
      if(!is.na(dcm_list[h])){
        dcm_dir <- dcm_list[h];
        proj$results_path<-NULL; proj$results_path <-paste(dcm_dir,"\\",results_dir, sep="");
        blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
        for(j in 1:1){
          if(length(blobs_list)!=0){
            dframe<-NULL;
            info<-NULL; info = file.info(paste(proj$results_path, "\\",blobs_list[j], sep=""));
            info <- info$size;
            if(!is.na(info)){
              if(as.numeric(info) > 3){
                if(dir.exists(proj$results_path)){
                  dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                  dframe <- dframe[which(dframe$Area.equivalent.circle.diameter >= 3 & dframe$Area.equivalent.circle.diameter <= 30),];
                  dframe <-dframe[which(dframe$Elong. <= 0.6),];
                  if(nrow(dframe)==0){
                    dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                    dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
                  }
                  dframe$ct_file_name <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
                 # dframe$total_slices <- meta_frame$num_slices[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                  # dframe$total_blobs <- meta_frame$num_blobs[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                  #dframe$mean_bits_stored <- meta_frame$ctmeta_bits_stored_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                  #dframe$mean_slice_location <- meta_frame$ctmeta_slice_location_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
    
                  dframe$cancer <- 0;
                  proj$dataset_KAGGLE_NORMAL <- rbind(proj$dataset_KAGGLE_NORMAL, dframe);
                  if(h%%PRINT_BATCH_SIZE >=0 && h%%PRINT_BATCH_SIZE <=5){print(paste("Loading & processing KAGGLE_NORMAL_BLOBS_SET ", h, ".../", nrow(dframe), "..."));}
                }
              }else{
                # countNAS <- countNAS +1;
                # heading <- c("X.1", "Frame", "Label", "X", "Y", "Area", "Area.Conv..Hull",
                #              "Peri.", "Peri..Conv..Hull", "Feret", "Min..Feret", "Maximum.inscriped.circle.diameter",
                #              "Area.equivalent.circle.diameter", "Long.Side.Length.MBR", "Short.Side.Length.MBR",
                #              "Aspect.Ratio", "Area.Peri.", "Circ.", "Elong.", "Convexity",
                #              "Solidity", "Num..of.Holes", "Thinnes.Rt.", "Contour.Temp.",
                #              "Orientation", "Fract..Dim.", "Fract..Dim..Goodness", "ct_file_name", "cancer"
                # );
                # notavs<- rep(NA, length(heading));
                # notavs[length(heading)-1] <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
                # notavs[length(heading)] <- 0;
                # dframe <-NULL;
                # dframe <-data.frame(t(notavs));
                # colnames(dframe)<-heading;
                # proj$dataset_KAGGLE_NORMAL <- rbind(proj$dataset_KAGGLE_NORMAL, dframe);
              }
            }
          }
        }
      }
    }
    
    DATASET_LEN_NORMAL <- NULL;
    proj$dataset_normal <- NULL;
    #proj$dataset_normal <- rbind(proj$dataset_normal, proj$dataset_LUNA_NORMAL);
    proj$dataset_normal <- rbind(proj$dataset_normal, proj$dataset_KAGGLE_NORMAL);
    DATASET_LEN_NORMAL <- nrow(proj$dataset_normal);
    print(paste("NORMAL DF_LEN:", DATASET_LEN_NORMAL, " from ", TOTAL_DATASET_LEN_NORMAL));
    print(paste("COUNT_NAS:", countNAS, " & for test: ", countNAS_test));
  }  
  
  proj$dataset <- rbind(proj$dataset_cancer, proj$dataset_normal);
  proj$testset <- proj$dataset_testset;
  
  SESSION = as.numeric(args[1]);
  
  #-------temp dir inits------
  temp_dir<- settings_paths$R_code_paths$temp_directory;
  temp_files <- list.files(path = temp_dir, recursive = FALSE);
  #temp_dirs <- list.dirs(path = temp_dir, recursive = FALSE);
  temp_files <- setdiff(temp_files, temp_files[which(grepl(".zip", temp_files))]);
  temp_files <- setdiff(temp_files, temp_files[which(grepl("try", temp_files))]);
  #temp_files <- c(temp_files, temp_dirs);
  temp_session_dir <- paste("try",(SESSION-1),sep="");
  dir.create(paste(temp_dir,temp_session_dir, sep=""));
  my.file.rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    file.rename(from = from,  to = to)
  }
  for(m in 1: length(temp_files)){
    my.file.rename(from = paste(temp_dir, temp_files[m], sep=""), to = paste(temp_dir, temp_session_dir, "\\",temp_files[m], sep=""));
  }
 
  backup_dir <- settings_paths$R_code_paths$backup_directory; 
  temp_dir<- settings_paths$R_code_paths$temp_directory;
  con<-file(paste(temp_dir, "KAGGLE_TEST_SET.csv", sep=""), encoding="utf8");
  write.csv(proj$dataset_KAGGLE_TEST_SET, file=con, row.names=FALSE);
  
  if(!is.null(proj$dataset_LUNA_CANCER)){
    temp_dir<- settings_paths$R_code_paths$temp_directory;
    con<-file(paste(temp_dir, "LUNA_CANCER.csv", sep=""), encoding="utf8");
    write.csv(proj$dataset_LUNA_CANCER, file=con, row.names=FALSE);
  }
  if(!is.null(proj$dataset_LUNA_EXTRA_CANCER)){
    temp_dir<- settings_paths$R_code_paths$temp_directory;
    con<-file(paste(temp_dir, "LUNA_EXTRA_CANCER.csv", sep=""), encoding="utf8");
    write.csv(proj$dataset_LUNA_EXTRA_CANCER, file=con, row.names=FALSE);
  }
  if(!is.null(proj$dataset_KAGGLE_CANCER)){
    temp_dir<- settings_paths$R_code_paths$temp_directory;
    con<-file(paste(temp_dir, "KAGGLE_CANCER.csv", sep=""), encoding="utf8");
    write.csv(proj$dataset_KAGGLE_CANCER, file=con, row.names=FALSE);
  }
  if(!is.null(proj$dataset_KAGGLE_NORMAL)){
    temp_dir<- settings_paths$R_code_paths$temp_directory;
    con<-file(paste(temp_dir, "KAGGLE_NORMAL.csv", sep=""), encoding="utf8");
    write.csv(proj$dataset_KAGGLE_NORMAL, file=con, row.names=FALSE);
  }
  
  con<-file(paste(backup_dir, "train_set.csv", sep=""), encoding="utf8");
  write.csv(proj$dataset, file=con, row.names=FALSE);
  con<-file(paste(backup_dir, "test_set.csv", sep=""), encoding="utf8");
  write.csv(proj$testset, file=con, row.names=FALSE);
  
  print("DONE!...")
}


args <- commandArgs(TRUE);
loadAndProcessDataset(c(as.character(args), "dummy"));

#------------------------------------------END------------------------------------------------------------
