#'generateMetadataCSVDataset.r
#'
#'(C) 2017 - Imad Khoury
#'
#**************************************************#
library(jsonlite);
settings_url <- "D:/Kaggle/Bowl2017/SETTINGS.json";#
settings_paths <- fromJSON(txt=settings_url);      #
#**************************************************#

setwd(settings_paths$R_code_paths$working_directory);
library(Hmisc, quietly=TRUE)
library(stringr)
library(robustHD)
library(methods)
par(ask=F);#turn-off plot asking

#function - normalize per data frame column
normalizePerColumn <- function(dframe){
  for(j in 1:length(dframe)){
    dframe[j] <-(dframe[j]-min(dframe[j]))/(max(dframe[j])-min(dframe[j]));
  }
  dframe
}


proj <- NULL;


generateMetadataCSVDataset <- function(){
  
#types <- c("CANCER", "NORMAL");
#types <- c("NORMAL","CANCER");
# if (length(args)<2) {
#   stop("At least one argument must be supplied (a type (CANCER, or NORMAL))", call.=FALSE);
# }  
# if(args[1] %in% c("NORMAL", "CANCER")){
# }else{
#   stop("The type must be either NORMAL, or, CANCER");
# }    
  
  types <- c("CANCER", "NORMAL");
  proj$final_1_dataset <- NULL;
  proj$final_2_dataset <- NULL;
  proj$dataset <- NULL; proj$dataset2 <- NULL;
    
  for(types_counter in 1: length(types)){
    
    
    print("");
    print("Copying CT METADATA CSV files from main dir to the two respective sub dirs...");
    proj$main_path <- paste(settings_paths$R_code_paths$meta_csv_directory, sep="");
    proj$main_path <- substring(proj$main_path, 1, nchar(proj$main_path)-1);
    main_list <- list.files(path = proj$main_path, pattern = "meta.csv", recursive = FALSE);
    ok=file.copy(paste(proj$main_path,"/",main_list[grepl("CANCER", main_list)],sep=""), paste(settings_paths$R_code_paths$meta_csv_directory,"CANCER/", sep=""));
    ok=file.copy(paste(proj$main_path,"/",main_list[grepl("NORMAL", main_list)],sep=""), paste(settings_paths$R_code_paths$meta_csv_directory,"NORMAL/", sep=""));
    
    type <- types[types_counter];
    proj$path <- paste(settings_paths$R_code_paths$meta_csv_directory,type, sep="");
    proj$files <- list.files(path = proj$path, pattern = ".csv", recursive = FALSE);
    proj$ordered_path <- file.path(proj$path, "ORDERED\\");
    dir.create(proj$ordered_path);
    proj$filtered_path <- file.path(proj$path, "FILTERED\\");
    dir.create(proj$filtered_path);
    proj$filtered_final_path <- file.path(proj$path, "FILTERED_FINAL\\");
    dir.create(proj$filtered_final_path);
    
    proj$filtered_image_features_path <- file.path(proj$path, "FILTERED_IMAGE_FEATURES\\");
    #dir.create(proj$filtered_image_features_path);
    proj$files_image_features <-list.files(path = proj$filtered_image_features_path, pattern = ".csv", recursive = FALSE);
    proj$final_path <- file.path(proj$path, "FINAL\\");
    dir.create(proj$final_path);
    
    numslices <- NULL;
    filenames <- NULL;
    numblobs <- NULL;
    blob_features_medians_cumul <- NULL;
    blob_features_means_cumul <- NULL;
    blob_features_thirdQ_cumul <- NULL;
    blob_features_firstQ_cumul <- NULL;
    blob_features_min_cumul <- NULL;
    blob_features_max_cumul <- NULL;
    
    blob_slices_counts_medians_cumul <- NULL;
    blob_slices_counts_means_cumul <- NULL;
    blob_slices_counts_thirdQ_cumul <- NULL;
    blob_slices_counts_firstQ_cumul <- NULL;
    blob_slices_counts_min_cumul <- NULL;
    blob_slices_counts_max_cumul <- NULL;
    
    blob_calculated_volumes_medians_cumul <- NULL;
    blob_calculated_volumes_means_cumul <- NULL;
    blob_calculated_volumes_thirdQ_cumul <- NULL;
    blob_calculated_volumes_firstQ_cumul <- NULL;
    blob_calculated_volumes_min_cumul <- NULL;
    blob_calculated_volumes_max_cumul <- NULL;
    
    half_stack_lobes_features_medians_cumul <- NULL;
    half_stack_lobes_features_means_cumul <- NULL;
    half_stack_lobes_features_thirdQ_cumul <- NULL;
    half_stack_lobes_features_firstQ_cumul <- NULL;
    half_stack_lobes_features_min_cumul <- NULL;
    half_stack_lobes_features_max_cumul <- NULL;
    
    statistics_features_medians_cumul <- NULL;
    statistics_features_means_cumul <- NULL;
    statistics_features_thirdQ_cumul <- NULL;
    statistics_features_firstQ_cumul <- NULL;
    statistics_features_min_cumul <- NULL;
    statistics_features_max_cumul <- NULL;
    
    results_features_medians_cumul <- NULL;
    results_features_means_cumul <- NULL;
    results_features_thirdQ_cumul <- NULL;
    results_features_firstQ_cumul <- NULL;
    results_features_min_cumul <- NULL;
    results_features_max_cumul <- NULL;
    
    ctmeta_features_medians_cumul <- NULL;
    ctmeta_features_means_cumul <- NULL;
    ctmeta_features_thirdQ_cumul <- NULL;
    ctmeta_features_firstQ_cumul <- NULL;
    ctmeta_features_min_cumul <- NULL;
    ctmeta_features_max_cumul <- NULL;
    
    proj$dataset_no_constants <- NULL;
    proj$dataset_no_constants_final_features <- NULL;
    empty_file<-NULL;
    empty_blobs<-NULL;
    empty_stats<-NULL;
    empty_half_stack<-NULL;
    empty_results<-NULL;
    info<-NULL;
    processed_counts <- 0;
    for(i in 1:length(proj$files)){
      info = file.info(paste(proj$path, proj$files[i], sep="\\"));
      empty_file = rownames(info[info$size < 4, ]);
      start <- proc.time();
      if((length(empty_file)==0 & !is.null(empty_file)) || !(length(empty_file)==1 & empty_file == "NA")){
        print(paste("Processing ",type," ",i," of ", length(proj$files), " ", proj$files[i]),sep="");
        
        proj$dataset <- NULL;
        proj$dataset <- read.csv(paste(proj$path, proj$files[i], sep="/"), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
        num <- nrow(proj$dataset);
        if(num>0){
          proj$dataset <- proj$dataset[order(proj$dataset$Instance.Number),];
          
          blob_features_medians <- NULL;
          blob_features_means <- NULL;
          blob_features_thirdQ <- NULL;
          blob_features_firstQ <- NULL;
          blob_features_min <- NULL;
          blob_features_max <- NULL;
          
          blob_slices_counts_medians <- NULL;
          blob_slices_counts_means <- NULL;
          blob_slices_counts_thirdQ <- NULL;
          blob_slices_counts_firstQ <- NULL;
          blob_slices_counts_min <- NULL;
          blob_slices_counts_max <- NULL;
          
          blob_calculated_volumes_medians <- NULL;
          blob_calculated_volumes_means <- NULL;
          blob_calculated_volumes_thirdQ <- NULL;
          blob_calculated_volumes_firstQ <- NULL;
          blob_calculated_volumes_min <- NULL;
          blob_calculated_volumes_max <- NULL;
          
          half_stack_lobes_features_medians <- NULL;
          half_stack_lobes_features_means <- NULL;
          half_stack_lobes_features_thirdQ <- NULL;
          half_stack_lobes_features_firstQ <- NULL;
          half_stack_lobes_features_min <- NULL;
          half_stack_lobes_features_max <- NULL;
          
          statistics_features_medians <- NULL;
          statistics_features_means <- NULL;
          statistics_features_thirdQ <- NULL;
          statistics_features_firstQ <- NULL;
          statistics_features_min <- NULL;
          statistics_features_max <- NULL;
          
          results_features_medians <- NULL;
          results_features_means <- NULL;
          results_features_thirdQ <- NULL;
          results_features_firstQ <- NULL;
          results_features_min <- NULL;
          results_features_max <- NULL;
          
          ctmeta_features_medians <- NULL;
          ctmeta_features_means <- NULL;
          ctmeta_features_thirdQ <- NULL;
          ctmeta_features_firstQ <- NULL;
          ctmeta_features_min <- NULL;
          ctmeta_features_max <- NULL;
          
          filename <- paste(paste(proj$ordered_path, proj$files[i], sep="/"), "_Ordered.csv", sep="");
          filename <- str_replace(filename, "\\\\\\\\", "\\\\");
          filename <- str_replace(filename, "/", "\\\\");
          con<-file(filename, encoding="utf8")
          write.csv(proj$dataset, file=con, row.names=FALSE)
          
          proj$dataset_no_constants <- proj$dataset[sapply(proj$dataset, function(x) length(unique(na.omit(x)))) > 1];
          proj$dataset_no_constants_final_features <- proj$dataset_no_constants[,c("Filename","Instance.Number", "Slice.Location")];
          new <- str_split_fixed(proj$dataset_no_constants$Image.Position.Patient, "\\\\", 3);
          new_features <- c("Image.Orientation.Patient","Image", "Position");
          colnames(new) <- new_features;
          
          proj$dataset_no_constants_final_features[c("Image.Orientation.Patient","Image", "Position")] <- new[,c(1:3)];
          
          handpicked <- proj$dataset[c("Samples.Per.Pixel","Rows", "Columns","Pixel.Spacing","Bits.Allocated","Bits.Stored","Window.Center","Window.Width","Rescale.Intercept","Rescale.Slope")]
          str_split_fixed(proj$dataset$Pixel.Spacing, "\\\\", 2)
          handpicked$Pixel.Spacing.Row <- str_split_fixed(proj$dataset$Pixel.Spacing, "\\\\", 2)[1]
          handpicked$Pixel.Spacing.Col <- str_split_fixed(proj$dataset$Pixel.Spacing, "\\\\", 2)[2]
          handpicked$Pixel.Spacing <- NULL
          
          proj$dataset_no_constants_final_features <- cbind(proj$dataset_no_constants_final_features, handpicked);
          
          filename <- paste(paste(proj$filtered_path, proj$files[i], sep="/"), "_Filtered.csv", sep="");
          filename <- str_replace(filename, "\\\\\\\\", "\\\\");
          filename <- str_replace(filename, "/", "\\\\");
          con<-file(filename, encoding="utf8")
          write.csv(proj$dataset_no_constants, file=con, row.names=FALSE)
          
          filename <- paste(paste(proj$filtered_final_path, proj$files[i], sep="/"), "_Filtered_final.csv", sep="");
          filename <- str_replace(filename, "\\\\\\\\", "\\\\");
          filename <- str_replace(filename, "/", "\\\\");
          con<-file(filename, encoding="utf8")
          write.csv(proj$dataset_no_constants_final_features, file=con, row.names=FALSE)
          
          proj$dataset <- NULL;
          thelist <- sapply(strsplit(proj$files_image_features, " of "), "[[",1);
          blobs_list <- proj$files_image_features[which(thelist == "Blobs")];
          half_stack_lobes_list <- proj$files_image_features[which(thelist == "Half-Stack Lobes")];
          results_list <- proj$files_image_features[which(thelist == "Results")];
          statistics_list <- proj$files_image_features[which(thelist == "Statistics")];
          
          #blobs
          info = file.info(paste(proj$filtered_image_features_path, blobs_list[i], sep="\\"));
          empty_blobs = rownames(info[info$size < 4, ]);
          if((length(empty_blobs)==0 & !is.null(empty_blobs)) || (!(length(empty_blobs)==1 & empty_blobs=="NA"))){
            proj$dataset <- read.csv(paste(proj$filtered_image_features_path, blobs_list[i], sep="/"), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
            filename <- paste(paste(proj$filtered_image_features_path, proj$files[i], sep="/"), "_Filtered_final_Blobs.csv", sep="");
            filename <- str_replace(filename, "\\\\\\\\", "\\\\");
            filename <- str_replace(filename, "/", "\\\\");
            con<-file(filename, encoding="utf8");
            proj$dataset$X.1 <-NULL;
            for(k in 5:26){
              proj$dataset[k] <- proj$dataset[k] / max(proj$dataset[k]); 
              #can also try standardize(...) for gaussian normalization TODO: log(x+1)
              ##ADJUST THIS ONE -- do log(x+1)
              proj$dataset[k] <- standardize(proj$dataset[k]); 
            }
            proj$dataset[, colSums(is.na(proj$dataset)) != nrow(proj$dataset)]
            proj$dataset<-proj$dataset[with(proj$dataset, order(Label)), ];
            
            intm <- aggregate(Frame ~ Label, data=proj$dataset, FUN=table)
            ff <- as.data.frame(sapply(intm$Frame, length))
            colnames(ff) <- c("counts")
            intm <- intm$Label
            
            intm2 <- aggregate(Area ~ Label, data=proj$dataset, FUN=sum)
            ff2 <- intm2
            colnames(ff2) <- c("volumes")
            ff2 <- ff2$volumes;
            
            write.csv(proj$dataset, file=con, row.names=FALSE);
            
            aa=contents(proj$dataset);
            bb=summary(proj$dataset);
            cc=describe(proj$dataset);
            
            dd=summary(ff$counts);
            dd2=summary(ff2);
            
            #    con2<-file(paste(filename, "_Aux1.txt",sep=""), encoding="utf8");
            #    con3<-file(paste(filename, "_Aux2.txt",sep=""), encoding="utf8");
            #    con4<-file(paste(filename, "_Aux3.txt",sep=""), encoding="utf8");
            #    dput(aa, file=con2);
            #    dput(bb, file=con3);
            #    dput(cc, file=con4);
            for(z in 2:length(colnames(bb))){
              blob_features_min <- c(blob_features_min, as.double(strsplit(bb[1,z], ":")[[1]][2]));
              blob_features_firstQ <- c(blob_features_firstQ, as.double(strsplit(bb[2,z], ":")[[1]][2]));
              blob_features_medians <- c(blob_features_medians, as.double(strsplit(bb[3,z], ":")[[1]][2]));
              blob_features_means <- c(blob_features_means, as.double(strsplit(bb[4,z], ":")[[1]][2]));
              blob_features_thirdQ <- c(blob_features_thirdQ, as.double(strsplit(bb[5,z], ":")[[1]][2]));
              blob_features_max <- c(blob_features_max, as.double(strsplit(bb[6,z], ":")[[1]][2]));
            }
            blob_slices_counts_min <- c(blob_slices_counts_min, as.double(as.numeric(dd[1])));
            blob_slices_counts_firstQ <- c(blob_slices_counts_firstQ, as.double(as.numeric(dd[2])));
            blob_slices_counts_medians <- c(blob_slices_counts_medians, as.double(as.numeric(dd[3])));
            blob_slices_counts_means <- c(blob_slices_counts_means, as.double(as.numeric(dd[4])));
            blob_slices_counts_thirdQ <- c(blob_slices_counts_thirdQ, as.double(as.numeric(dd[5])));
            blob_slices_counts_max <- c(blob_slices_counts_max, as.double(as.numeric(dd[6])));
            
            blob_calculated_volumes_min <- c(blob_calculated_volumes_min, as.double(as.numeric(dd2[1])));
            blob_calculated_volumes_firstQ <- c(blob_calculated_volumes_firstQ, as.double(as.numeric(dd2[2])));
            blob_calculated_volumes_medians <- c(blob_calculated_volumes_medians, as.double(as.numeric(dd2[3])));
            blob_calculated_volumes_means <- c(blob_calculated_volumes_means, as.double(as.numeric(dd2[4])));
            blob_calculated_volumes_thirdQ <- c(blob_calculated_volumes_thirdQ, as.double(as.numeric(dd2[5])));
            blob_calculated_volumes_max <- c(blob_calculated_volumes_max, as.double(as.numeric(dd2[6])));
            
            blob_features_min_cumul <- rbind(blob_features_min_cumul, blob_features_min);
            blob_features_firstQ_cumul <- rbind(blob_features_firstQ_cumul, blob_features_firstQ);
            blob_features_medians_cumul <- rbind(blob_features_medians_cumul, blob_features_medians);
            blob_features_means_cumul <- rbind(blob_features_means_cumul, blob_features_means);
            blob_features_thirdQ_cumul <- rbind(blob_features_thirdQ_cumul, blob_features_thirdQ);
            blob_features_max_cumul <- rbind(blob_features_max_cumul, blob_features_max);
            
            blob_slices_counts_min_cumul <- rbind(blob_slices_counts_min_cumul, blob_slices_counts_min);
            blob_slices_counts_firstQ_cumul <- rbind(blob_slices_counts_firstQ_cumul, blob_slices_counts_firstQ);
            blob_slices_counts_medians_cumul <- rbind(blob_slices_counts_medians_cumul, blob_slices_counts_medians);
            blob_slices_counts_means_cumul <- rbind(blob_slices_counts_means_cumul, blob_slices_counts_means);
            blob_slices_counts_thirdQ_cumul <- rbind(blob_slices_counts_thirdQ_cumul, blob_slices_counts_thirdQ);
            blob_slices_counts_max_cumul <- rbind(blob_slices_counts_max_cumul, blob_slices_counts_max);
            
            blob_calculated_volumes_min_cumul <- rbind(blob_calculated_volumes_min_cumul, blob_calculated_volumes_min);
            blob_calculated_volumes_firstQ_cumul <- rbind(blob_calculated_volumes_firstQ_cumul, blob_calculated_volumes_firstQ);
            blob_calculated_volumes_medians_cumul <- rbind(blob_calculated_volumes_medians_cumul, blob_calculated_volumes_medians);
            blob_calculated_volumes_means_cumul <- rbind(blob_calculated_volumes_means_cumul, blob_calculated_volumes_means);
            blob_calculated_volumes_thirdQ_cumul <- rbind(blob_calculated_volumes_thirdQ_cumul, blob_calculated_volumes_thirdQ);
            blob_calculated_volumes_max_cumul <- rbind(blob_calculated_volumes_max_cumul, blob_calculated_volumes_max);
          }
          
          
          info = file.info(paste(proj$filtered_image_features_path, half_stack_lobes_list[i], sep="\\"));
          empty_half_stack = rownames(info[info$size < 4, ]);
          if(((length(empty_half_stack)==0 & !is.null(empty_half_stack)) & !is.null(empty_half_stack)) || !(length(empty_half_stack)==1 & empty_half_stack == "NA")){
            # half_stack lobes
            proj$dataset <- read.csv(paste(proj$filtered_image_features_path, half_stack_lobes_list[i], sep="/"), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
            
            filename <- paste(paste(proj$filtered_image_features_path, proj$files[i], sep="/"), "_Filtered_final_Half_Stack.csv", sep="");
            filename <- str_replace(filename, "\\\\\\\\", "\\\\");
            filename <- str_replace(filename, "/", "\\\\");
            con<-file(filename, encoding="utf8");
            proj$dataset$X.1 <-NULL;
            for(k in 2:5){
              proj$dataset[k] <- proj$dataset[k] / max(proj$dataset[k]); 
              #can also try standardize(...) for gaussian normalization TODO: log(x+1)
              proj$dataset[k] <- standardize(proj$dataset[k]); 
            }
            proj$dataset[, colSums(is.na(proj$dataset)) != nrow(proj$dataset)]
            #    proj$dataset<-proj$dataset[with(proj$dataset, order(Label)), ];
            
            aa=contents(proj$dataset);
            bb=summary(proj$dataset);
            cc=describe(proj$dataset);
            
            #    con2<-file(paste(filename, "_Aux1.txt",sep=""), encoding="utf8");
            #    con3<-file(paste(filename, "_Aux2.txt",sep=""), encoding="utf8");
            #    con4<-file(paste(filename, "_Aux3.txt",sep=""), encoding="utf8");
            #    dput(aa, file=con2);
            #    dput(bb, file=con3);
            #    dput(cc, file=con4);
            for(z in 2:length(colnames(bb))){
              half_stack_lobes_features_min <- c(half_stack_lobes_features_min, as.double(strsplit(bb[1,z], ":")[[1]][2]));
              half_stack_lobes_features_firstQ <- c(half_stack_lobes_features_firstQ, as.double(strsplit(bb[2,z], ":")[[1]][2]));
              half_stack_lobes_features_medians <- c(half_stack_lobes_features_medians, as.double(strsplit(bb[3,z], ":")[[1]][2]));
              half_stack_lobes_features_means <- c(half_stack_lobes_features_means, as.double(strsplit(bb[4,z], ":")[[1]][2]));
              half_stack_lobes_features_thirdQ <- c(half_stack_lobes_features_thirdQ, as.double(strsplit(bb[5,z], ":")[[1]][2]));
              half_stack_lobes_features_max <- c(half_stack_lobes_features_max, as.double(strsplit(bb[6,z], ":")[[1]][2]));
            }
            
            half_stack_lobes_features_min_cumul <- rbind(half_stack_lobes_features_min_cumul, half_stack_lobes_features_min);
            half_stack_lobes_features_firstQ_cumul <- rbind(half_stack_lobes_features_firstQ_cumul, half_stack_lobes_features_firstQ);
            half_stack_lobes_features_medians_cumul <- rbind(half_stack_lobes_features_medians_cumul, half_stack_lobes_features_medians);
            half_stack_lobes_features_means_cumul <- rbind(half_stack_lobes_features_means_cumul, half_stack_lobes_features_means);
            half_stack_lobes_features_thirdQ_cumul <- rbind(half_stack_lobes_features_thirdQ_cumul, half_stack_lobes_features_thirdQ);
            half_stack_lobes_features_max_cumul <- rbind(half_stack_lobes_features_max_cumul, half_stack_lobes_features_max);
          }        
          
          
          info = file.info(paste(proj$filtered_image_features_path, statistics_list[i], sep="\\"));
          empty_stats = rownames(info[info$size < 4, ]);
          if((length(empty_stats)==0 & !is.null(empty_stats)) || !(length(empty_stats)==1 & empty_stats == "NA")){
            
            
            #3d statistics
            proj$dataset <- read.csv(paste(proj$filtered_image_features_path, statistics_list[i], sep="/"), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
            
            filename <- paste(paste(proj$filtered_image_features_path, proj$files[i], sep="/"), "_Filtered_final_statistics.csv", sep="");
            filename <- str_replace(filename, "\\\\\\\\", "\\\\");
            filename <- str_replace(filename, "/", "\\\\");
            con<-file(filename, encoding="utf8");
            proj$dataset$X.1 <-NULL;
            for(k in 2:9){
              proj$dataset[k] <- proj$dataset[k] / max(proj$dataset[k]); 
              #can also try standardize(...) for gaussian normalization TODO: log(x+1)
              proj$dataset[k] <- standardize(proj$dataset[k]); 
            }
            proj$dataset[, colSums(is.na(proj$dataset)) != nrow(proj$dataset)]
            #    proj$dataset<-proj$dataset[with(proj$dataset, order(Label)), ];
            
            aa=contents(proj$dataset);
            bb=summary(proj$dataset);
            cc=describe(proj$dataset);
            
            #    con2<-file(paste(filename, "_Aux1.txt",sep=""), encoding="utf8");
            #    con3<-file(paste(filename, "_Aux2.txt",sep=""), encoding="utf8");
            #    con4<-file(paste(filename, "_Aux3.txt",sep=""), encoding="utf8");
            #    dput(aa, file=con2);
            #    dput(bb, file=con3);
            #    dput(cc, file=con4);
            for(z in 2:length(colnames(bb))){
              statistics_features_min <- c(statistics_features_min, as.double(strsplit(bb[1,z], ":")[[1]][2]));
              statistics_features_firstQ <- c(statistics_features_firstQ, as.double(strsplit(bb[2,z], ":")[[1]][2]));
              statistics_features_medians <- c(statistics_features_medians, as.double(strsplit(bb[3,z], ":")[[1]][2]));
              statistics_features_means <- c(statistics_features_means, as.double(strsplit(bb[4,z], ":")[[1]][2]));
              statistics_features_thirdQ <- c(statistics_features_thirdQ, as.double(strsplit(bb[5,z], ":")[[1]][2]));
              statistics_features_max <- c(statistics_features_max, as.double(strsplit(bb[6,z], ":")[[1]][2]));
            }
            
            statistics_features_min_cumul <- rbind(statistics_features_min_cumul, statistics_features_min);
            statistics_features_firstQ_cumul <- rbind(statistics_features_firstQ_cumul, statistics_features_firstQ);
            statistics_features_medians_cumul <- rbind(statistics_features_medians_cumul, statistics_features_medians);
            statistics_features_means_cumul <- rbind(statistics_features_means_cumul, statistics_features_means);
            statistics_features_thirdQ_cumul <- rbind(statistics_features_thirdQ_cumul, statistics_features_thirdQ);
            statistics_features_max_cumul <- rbind(statistics_features_max_cumul, statistics_features_max);
            
          }      
          #2d particles results
          info = file.info(paste(proj$filtered_image_features_path, results_list[i], sep="\\"));
          empty_results = rownames(info[info$size < 4, ]);
          if(((length(empty_results)==0 & !is.null(empty_results)) || (!(length(empty_results)==1 & empty_results=="NA")))){
            
            proj$dataset <- read.csv(paste(proj$filtered_image_features_path, results_list[i], sep="/"), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
            filename <- paste(paste(proj$filtered_image_features_path, proj$files[i], sep="/"), "_Filtered_final_Results.csv", sep="");
            filename <- str_replace(filename, "\\\\\\\\", "\\\\");
            filename <- str_replace(filename, "/", "\\\\");
            con<-file(filename, encoding="utf8");
            proj$dataset$X.1 <-NULL;
            for(k in 2:5){
              proj$dataset[k] <- proj$dataset[k] / max(proj$dataset[k]); 
              #can also try standardize(...) for gaussian normalization TODO: log(x+1) TODO: log(x+1)
              proj$dataset[k] <- standardize(proj$dataset[k]); 
            }
            proj$dataset[, colSums(is.na(proj$dataset)) != nrow(proj$dataset)]
            #    proj$dataset<-proj$dataset[with(proj$dataset, order(Label)), ];
            
            aa=contents(proj$dataset);
            bb=summary(proj$dataset);
            cc=describe(proj$dataset);
            
            #    con2<-file(paste(filename, "_Aux1.txt",sep=""), encoding="utf8");
            #    con3<-file(paste(filename, "_Aux2.txt",sep=""), encoding="utf8");
            #    con4<-file(paste(filename, "_Aux3.txt",sep=""), encoding="utf8");
            #    dput(aa, file=con2);
            #    dput(bb, file=con3);
            #    dput(cc, file=con4);
            for(z in 2:length(colnames(bb))){
              results_features_min <- c(results_features_min, as.double(strsplit(bb[1,z], ":")[[1]][2]));
              results_features_firstQ <- c(results_features_firstQ, as.double(strsplit(bb[2,z], ":")[[1]][2]));
              results_features_medians <- c(results_features_medians, as.double(strsplit(bb[3,z], ":")[[1]][2]));
              results_features_means <- c(results_features_means, as.double(strsplit(bb[4,z], ":")[[1]][2]));
              results_features_thirdQ <- c(results_features_thirdQ, as.double(strsplit(bb[5,z], ":")[[1]][2]));
              results_features_max <- c(results_features_max, as.double(strsplit(bb[6,z], ":")[[1]][2]));
            }
            
            results_features_min_cumul <- rbind(results_features_min_cumul, results_features_min);
            results_features_firstQ_cumul <- rbind(results_features_firstQ_cumul, results_features_firstQ);
            results_features_medians_cumul <- rbind(results_features_medians_cumul, results_features_medians);
            results_features_means_cumul <- rbind(results_features_means_cumul, results_features_means);
            results_features_thirdQ_cumul <- rbind(results_features_thirdQ_cumul, results_features_thirdQ);
            results_features_max_cumul <- rbind(results_features_max_cumul, results_features_max);
            
          }    
          # ct meta
          # OPTIONAL NORMALIZATION STEP -- TBD    
          #    for(k in 3:17){
          #      proj$dataset_no_constants_final_features[k] <- proj$dataset_no_constants_final_features[k] / max(proj$dataset_no_constants_final_features[k]); 
          #      #can also try standardize(...) for gaussian normalization TODO: log(x+1) TODO: log(x+1)
          #    proj$dataset_no_constants_final_features[k] <- standardize(proj$dataset_no_constants_final_features[k]); 
          #  }
          
          aa=contents(proj$dataset_no_constants_final_features);
          bb=summary(proj$dataset_no_constants_final_features);
          cc=describe(proj$dataset_no_constants_final_features);
          
          for(z in 2:length(colnames(bb))){
            ctmeta_features_min <- c(ctmeta_features_min, as.double(strsplit(bb[1,z], ":")[[1]][2]));
            ctmeta_features_firstQ <- c(ctmeta_features_firstQ, as.double(strsplit(bb[2,z], ":")[[1]][2]));
            ctmeta_features_medians <- c(ctmeta_features_medians, as.double(strsplit(bb[3,z], ":")[[1]][2]));
            ctmeta_features_means <- c(ctmeta_features_means, as.double(strsplit(bb[4,z], ":")[[1]][2]));
            ctmeta_features_thirdQ <- c(ctmeta_features_thirdQ, as.double(strsplit(bb[5,z], ":")[[1]][2]));
            ctmeta_features_max <- c(ctmeta_features_max, as.double(strsplit(bb[6,z], ":")[[1]][2]));
          }
          
          
          ctmeta_features_min_cumul <- rbind(ctmeta_features_min_cumul, ctmeta_features_min);
          ctmeta_features_firstQ_cumul <- rbind(ctmeta_features_firstQ_cumul, ctmeta_features_firstQ);
          ctmeta_features_medians_cumul <- rbind(ctmeta_features_medians_cumul, ctmeta_features_medians);
          ctmeta_features_means_cumul <- rbind(ctmeta_features_means_cumul, ctmeta_features_means);
          ctmeta_features_thirdQ_cumul <- rbind(ctmeta_features_thirdQ_cumul, ctmeta_features_thirdQ);
          ctmeta_features_max_cumul <- rbind(ctmeta_features_max_cumul, ctmeta_features_max);
          
          numslices <- c(numslices, num);
          if((length(empty_blobs)==0 & !is.null(empty_blobs)) || !(length(empty_blobs)==1 & empty_blobs == "NA")){
            numblobs <- c(numblobs, length(intm));
          }else{
            numblobs <- 0;
          }
          filenames <- c(filenames, gsub("_meta.csv","",proj$files[i]));  
          
          end <- proc.time();
          print(end-start);
          
        }
        processed_counts <- processed_counts + 1;
      }
    }
    
    if((length(empty_file)==0 & !is.null(empty_file)) || !(length(empty_file)==1 & empty_file == "NA")){
      proj$dataset2 <- NULL;
      proj$dataset2$ct_file_name <- filenames;
      proj$dataset2$num_slices <- numslices;
      
      if((length(empty_blobs)==0 & !is.null(empty_blobs)) || !(length(empty_blobs)==1 & empty_blobs == "NA")){
        proj$dataset2$num_blobs <- numblobs;
        proj$dataset2$blob_label_min   <- blob_features_min_cumul[,1];
        proj$dataset2$blob_X_min   <- blob_features_min_cumul[,2];
        proj$dataset2$blob_Y_min   <- blob_features_min_cumul[,3];
        proj$dataset2$blob_area_min   <- blob_features_min_cumul[,4];
        proj$dataset2$blob_area_conv_hull_min   <- blob_features_min_cumul[,5];
        proj$dataset2$blob_peri_min   <- blob_features_min_cumul[,6];
        proj$dataset2$blob_peri_conv_hull_min   <- blob_features_min_cumul[,7];
        proj$dataset2$blob_feret_min   <- blob_features_min_cumul[,8];
        proj$dataset2$blob_min_feret_min   <- blob_features_min_cumul[,9];
        proj$dataset2$blob_max_inscribed_diameter_min   <- blob_features_min_cumul[,10];
        proj$dataset2$blob_area_equivalent_diameter_min   <- blob_features_min_cumul[,11];
        proj$dataset2$blob_long_side_MBR_min   <- blob_features_min_cumul[,12];
        proj$dataset2$blob_short_side_MBR_min   <- blob_features_min_cumul[,13];
        proj$dataset2$blob_aspect_ratio_min   <- blob_features_min_cumul[,14];
        proj$dataset2$blob_area_peri_min   <- blob_features_min_cumul[,15];
        proj$dataset2$blob_circularity_min   <- blob_features_min_cumul[,16];
        proj$dataset2$blob_elongation_min   <- blob_features_min_cumul[,17];
        proj$dataset2$blob_convexity_min   <- blob_features_min_cumul[,18];
        proj$dataset2$blob_solidity_min   <- blob_features_min_cumul[,19];
        proj$dataset2$blob_num_holes_min   <- blob_features_min_cumul[,20];
        proj$dataset2$blob_thinnes_rt_min   <- blob_features_min_cumul[,21];
        proj$dataset2$blob_contour_temp_min   <- blob_features_min_cumul[,22];
        proj$dataset2$blob_orientation_min   <- blob_features_min_cumul[,23];
        proj$dataset2$blob_fract_dim_min   <- blob_features_min_cumul[,24];
        proj$dataset2$blob_fract_dim_goodness_min   <- blob_features_min_cumul[,25];
        
        proj$dataset2$blob_label_firstQ   <- blob_features_firstQ_cumul[,1];
        proj$dataset2$blob_X_firstQ   <- blob_features_firstQ_cumul[,2];
        proj$dataset2$blob_Y_firstQ   <- blob_features_firstQ_cumul[,3];
        proj$dataset2$blob_area_firstQ   <- blob_features_firstQ_cumul[,4];
        proj$dataset2$blob_area_conv_hull_firstQ   <- blob_features_firstQ_cumul[,5];
        proj$dataset2$blob_peri_firstQ   <- blob_features_firstQ_cumul[,6];
        proj$dataset2$blob_peri_conv_hull_firstQ   <- blob_features_firstQ_cumul[,7];
        proj$dataset2$blob_feret_firstQ   <- blob_features_firstQ_cumul[,8];
        proj$dataset2$blob_firstQ_feret_firstQ   <- blob_features_firstQ_cumul[,9];
        proj$dataset2$blob_max_inscribed_diameter_firstQ   <- blob_features_firstQ_cumul[,10];
        proj$dataset2$blob_area_equivalent_diameter_firstQ   <- blob_features_firstQ_cumul[,11];
        proj$dataset2$blob_long_side_MBR_firstQ   <- blob_features_firstQ_cumul[,12];
        proj$dataset2$blob_short_side_MBR_firstQ   <- blob_features_firstQ_cumul[,13];
        proj$dataset2$blob_aspect_ratio_firstQ   <- blob_features_firstQ_cumul[,14];
        proj$dataset2$blob_area_peri_firstQ   <- blob_features_firstQ_cumul[,15];
        proj$dataset2$blob_circularity_firstQ   <- blob_features_firstQ_cumul[,16];
        proj$dataset2$blob_elongation_firstQ   <- blob_features_firstQ_cumul[,17];
        proj$dataset2$blob_convexity_firstQ   <- blob_features_firstQ_cumul[,19-1];
        proj$dataset2$blob_solidity_firstQ   <- blob_features_firstQ_cumul[,20-1];
        proj$dataset2$blob_num_holes_firstQ   <- blob_features_firstQ_cumul[,21-1];
        proj$dataset2$blob_thinnes_rt_firstQ   <- blob_features_firstQ_cumul[,22-1];
        proj$dataset2$blob_contour_temp_firstQ   <- blob_features_firstQ_cumul[,23-1];
        proj$dataset2$blob_orientation_firstQ   <- blob_features_firstQ_cumul[,24-1];
        proj$dataset2$blob_fract_dim_firstQ   <- blob_features_firstQ_cumul[,25-1];
        proj$dataset2$blob_fract_dim_goodness_firstQ   <- blob_features_firstQ_cumul[,26-1];
        
        proj$dataset2$blob_label_medians   <- blob_features_medians_cumul[,2-1];
        proj$dataset2$blob_X_medians   <- blob_features_medians_cumul[,3-1];
        proj$dataset2$blob_Y_medians   <- blob_features_medians_cumul[,4-1];
        proj$dataset2$blob_area_medians   <- blob_features_medians_cumul[,5-1];
        proj$dataset2$blob_area_conv_hull_medians   <- blob_features_medians_cumul[,6-1];
        proj$dataset2$blob_peri_medians   <- blob_features_medians_cumul[,7-1];
        proj$dataset2$blob_peri_conv_hull_medians   <- blob_features_medians_cumul[,8-1];
        proj$dataset2$blob_feret_medians   <- blob_features_medians_cumul[,9-1];
        proj$dataset2$blob_medians_feret_medians   <- blob_features_medians_cumul[,10-1];
        proj$dataset2$blob_max_inscribed_diameter_medians   <- blob_features_medians_cumul[,11-1];
        proj$dataset2$blob_area_equivalent_diameter_medians   <- blob_features_medians_cumul[,12-1];
        proj$dataset2$blob_long_side_MBR_medians   <- blob_features_medians_cumul[,13-1];
        proj$dataset2$blob_short_side_MBR_medians   <- blob_features_medians_cumul[,14-1];
        proj$dataset2$blob_aspect_ratio_medians   <- blob_features_medians_cumul[,15-1];
        proj$dataset2$blob_area_peri_medians   <- blob_features_medians_cumul[,16-1];
        proj$dataset2$blob_circularity_medians   <- blob_features_medians_cumul[,17-1];
        proj$dataset2$blob_elongation_medians   <- blob_features_medians_cumul[,18-1];
        proj$dataset2$blob_convexity_medians   <- blob_features_medians_cumul[,19-1];
        proj$dataset2$blob_solidity_medians   <- blob_features_medians_cumul[,20-1];
        proj$dataset2$blob_num_holes_medians   <- blob_features_medians_cumul[,21-1];
        proj$dataset2$blob_thinnes_rt_medians   <- blob_features_medians_cumul[,22-1];
        proj$dataset2$blob_contour_temp_medians   <- blob_features_medians_cumul[,23-1];
        proj$dataset2$blob_orientation_medians   <- blob_features_medians_cumul[,24-1];
        proj$dataset2$blob_fract_dim_medians   <- blob_features_medians_cumul[,25-1];
        proj$dataset2$blob_fract_dim_goodness_medians   <- blob_features_medians_cumul[,26-1];
        
        proj$dataset2$blob_label_means   <- blob_features_means_cumul[,2-1];
        proj$dataset2$blob_X_means   <- blob_features_means_cumul[,3-1];
        proj$dataset2$blob_Y_means   <- blob_features_means_cumul[,4-1];
        proj$dataset2$blob_area_means   <- blob_features_means_cumul[,5-1];
        proj$dataset2$blob_area_conv_hull_means   <- blob_features_means_cumul[,6-1];
        proj$dataset2$blob_peri_means   <- blob_features_means_cumul[,7-1];
        proj$dataset2$blob_peri_conv_hull_means   <- blob_features_means_cumul[,8-1];
        proj$dataset2$blob_feret_means   <- blob_features_means_cumul[,9-1];
        proj$dataset2$blob_means_feret_means   <- blob_features_means_cumul[,10-1];
        proj$dataset2$blob_max_inscribed_diameter_means   <- blob_features_means_cumul[,11-1];
        proj$dataset2$blob_area_equivalent_diameter_means   <- blob_features_means_cumul[,12-1];
        proj$dataset2$blob_long_side_MBR_means   <- blob_features_means_cumul[,13-1];
        proj$dataset2$blob_short_side_MBR_means   <- blob_features_means_cumul[,14-1];
        proj$dataset2$blob_aspect_ratio_means   <- blob_features_means_cumul[,15-1];
        proj$dataset2$blob_area_peri_means   <- blob_features_means_cumul[,16-1];
        proj$dataset2$blob_circularity_means   <- blob_features_means_cumul[,17-1];
        proj$dataset2$blob_elongation_means   <- blob_features_means_cumul[,18-1];
        proj$dataset2$blob_convexity_means   <- blob_features_means_cumul[,19-1];
        proj$dataset2$blob_solidity_means   <- blob_features_means_cumul[,20-1];
        proj$dataset2$blob_num_holes_means   <- blob_features_means_cumul[,21-1];
        proj$dataset2$blob_thinnes_rt_means   <- blob_features_means_cumul[,22-1];
        proj$dataset2$blob_contour_temp_means   <- blob_features_means_cumul[,23-1];
        proj$dataset2$blob_orientation_means   <- blob_features_means_cumul[,24-1];
        proj$dataset2$blob_fract_dim_means   <- blob_features_means_cumul[,25-1];
        proj$dataset2$blob_fract_dim_goodness_means   <- blob_features_means_cumul[,26-1];
        
        proj$dataset2$blob_label_thirdQ   <- blob_features_thirdQ_cumul[,2-1];
        proj$dataset2$blob_X_thirdQ   <- blob_features_thirdQ_cumul[,3-1];
        proj$dataset2$blob_Y_thirdQ   <- blob_features_thirdQ_cumul[,4-1];
        proj$dataset2$blob_area_thirdQ   <- blob_features_thirdQ_cumul[,5-1];
        proj$dataset2$blob_area_conv_hull_thirdQ   <- blob_features_thirdQ_cumul[,6-1];
        proj$dataset2$blob_peri_thirdQ   <- blob_features_thirdQ_cumul[,7-1];
        proj$dataset2$blob_peri_conv_hull_thirdQ   <- blob_features_thirdQ_cumul[,8-1];
        proj$dataset2$blob_feret_thirdQ   <- blob_features_thirdQ_cumul[,9-1];
        proj$dataset2$blob_thirdQ_feret_thirdQ   <- blob_features_thirdQ_cumul[,10-1];
        proj$dataset2$blob_max_inscribed_diameter_thirdQ   <- blob_features_thirdQ_cumul[,11-1];
        proj$dataset2$blob_area_equivalent_diameter_thirdQ   <- blob_features_thirdQ_cumul[,12-1];
        proj$dataset2$blob_long_side_MBR_thirdQ   <- blob_features_thirdQ_cumul[,13-1];
        proj$dataset2$blob_short_side_MBR_thirdQ   <- blob_features_thirdQ_cumul[,14-1];
        proj$dataset2$blob_aspect_ratio_thirdQ   <- blob_features_thirdQ_cumul[,15-1];
        proj$dataset2$blob_area_peri_thirdQ   <- blob_features_thirdQ_cumul[,16-1];
        proj$dataset2$blob_circularity_thirdQ   <- blob_features_thirdQ_cumul[,17-1];
        proj$dataset2$blob_elongation_thirdQ   <- blob_features_thirdQ_cumul[,18-1];
        proj$dataset2$blob_convexity_thirdQ   <- blob_features_thirdQ_cumul[,19-1];
        proj$dataset2$blob_solidity_thirdQ   <- blob_features_thirdQ_cumul[,20-1];
        proj$dataset2$blob_num_holes_thirdQ   <- blob_features_thirdQ_cumul[,21-1];
        proj$dataset2$blob_thinnes_rt_thirdQ   <- blob_features_thirdQ_cumul[,22-1];
        proj$dataset2$blob_contour_temp_thirdQ   <- blob_features_thirdQ_cumul[,23-1];
        proj$dataset2$blob_orientation_thirdQ   <- blob_features_thirdQ_cumul[,24-1];
        proj$dataset2$blob_fract_dim_thirdQ   <- blob_features_thirdQ_cumul[,25-1];
        proj$dataset2$blob_fract_dim_goodness_thirdQ   <- blob_features_thirdQ_cumul[,26-1];
        
        proj$dataset2$blob_label_max   <- blob_features_max_cumul[,2-1];
        proj$dataset2$blob_X_max   <- blob_features_max_cumul[,3-1];
        proj$dataset2$blob_Y_max   <- blob_features_max_cumul[,4-1];
        proj$dataset2$blob_area_max   <- blob_features_max_cumul[,5-1];
        proj$dataset2$blob_area_conv_hull_max   <- blob_features_max_cumul[,6-1];
        proj$dataset2$blob_peri_max   <- blob_features_max_cumul[,7-1];
        proj$dataset2$blob_peri_conv_hull_max   <- blob_features_max_cumul[,8-1];
        proj$dataset2$blob_feret_max   <- blob_features_max_cumul[,9-1];
        proj$dataset2$blob_max_feret_max   <- blob_features_max_cumul[,10-1];
        proj$dataset2$blob_max_inscribed_diameter_max   <- blob_features_max_cumul[,11-1];
        proj$dataset2$blob_area_equivalent_diameter_max   <- blob_features_max_cumul[,12-1];
        proj$dataset2$blob_long_side_MBR_max   <- blob_features_max_cumul[,13-1];
        proj$dataset2$blob_short_side_MBR_max   <- blob_features_max_cumul[,14-1];
        proj$dataset2$blob_aspect_ratio_max   <- blob_features_max_cumul[,15-1];
        proj$dataset2$blob_area_peri_max   <- blob_features_max_cumul[,16-1];
        proj$dataset2$blob_circularity_max   <- blob_features_max_cumul[,17-1];
        proj$dataset2$blob_elongation_max   <- blob_features_max_cumul[,18-1];
        proj$dataset2$blob_convexity_max   <- blob_features_max_cumul[,19-1];
        proj$dataset2$blob_solidity_max   <- blob_features_max_cumul[,20-1];
        proj$dataset2$blob_num_holes_max   <- blob_features_max_cumul[,21-1];
        proj$dataset2$blob_thinnes_rt_max   <- blob_features_max_cumul[,22-1];
        proj$dataset2$blob_contour_temp_max   <- blob_features_max_cumul[,23-1];
        proj$dataset2$blob_orientation_max   <- blob_features_max_cumul[,24-1];
        proj$dataset2$blob_fract_dim_max   <- blob_features_max_cumul[,25-1];
        proj$dataset2$blob_fract_dim_goodness_max   <- blob_features_max_cumul[,26-1];
        
        
        proj$dataset2$blob_slices_counts_min   <- blob_slices_counts_min_cumul[,1];
        
        proj$dataset2$blob_slices_counts_firstQ   <- blob_slices_counts_firstQ_cumul[,1];
        
        proj$dataset2$blob_slices_counts_medians   <- blob_slices_counts_medians_cumul[,1];
        
        proj$dataset2$blob_slices_counts_means   <- blob_slices_counts_means_cumul[,1];
        
        proj$dataset2$blob_slices_counts_thirdQ   <- blob_slices_counts_thirdQ_cumul[,1];
        
        proj$dataset2$blob_slices_counts_max   <- blob_slices_counts_max_cumul[,1];
        
        
        proj$dataset2$blob_calculated_volumes_min   <- blob_calculated_volumes_min_cumul[,1];
        
        proj$dataset2$blob_calculated_volumes_firstQ   <- blob_calculated_volumes_firstQ_cumul[,1];
        
        proj$dataset2$blob_calculated_volumes_medians   <- blob_calculated_volumes_medians_cumul[,1];
        
        proj$dataset2$blob_calculated_volumes_means   <- blob_calculated_volumes_means_cumul[,1];
        
        proj$dataset2$blob_calculated_volumes_thirdQ   <- blob_calculated_volumes_thirdQ_cumul[,1];
        
        proj$dataset2$blob_calculated_volumes_max   <- blob_calculated_volumes_max_cumul[,1];
        
      }else{
        proj$dataset2$num_blobs <- "NA";
        proj$dataset2$blob_label_min   <- "NA";
        proj$dataset2$blob_X_min   <- "NA";
        proj$dataset2$blob_Y_min   <- "NA";
        proj$dataset2$blob_area_min   <- "NA";
        proj$dataset2$blob_area_conv_hull_min   <- "NA";
        proj$dataset2$blob_peri_min   <- "NA";
        proj$dataset2$blob_peri_conv_hull_min   <- "NA";
        proj$dataset2$blob_feret_min   <- "NA";
        proj$dataset2$blob_min_feret_min   <- "NA";
        proj$dataset2$blob_max_inscribed_diameter_min   <- "NA";
        proj$dataset2$blob_area_equivalent_diameter_min   <- "NA";
        proj$dataset2$blob_long_side_MBR_min   <- "NA";
        proj$dataset2$blob_short_side_MBR_min   <- "NA";
        proj$dataset2$blob_aspect_ratio_min   <- "NA";
        proj$dataset2$blob_area_peri_min   <- "NA";
        proj$dataset2$blob_circularity_min   <- "NA";
        proj$dataset2$blob_elongation_min   <- "NA";
        proj$dataset2$blob_convexity_min   <- "NA";
        proj$dataset2$blob_solidity_min   <- "NA";
        proj$dataset2$blob_num_holes_min   <- "NA";
        proj$dataset2$blob_thinnes_rt_min   <- "NA";
        proj$dataset2$blob_contour_temp_min   <- "NA";
        proj$dataset2$blob_orientation_min   <- "NA";
        proj$dataset2$blob_fract_dim_min   <- "NA";
        proj$dataset2$blob_fract_dim_goodness_min   <- "NA";
        
        proj$dataset2$blob_label_firstQ   <- "NA";
        proj$dataset2$blob_X_firstQ   <- "NA";
        proj$dataset2$blob_Y_firstQ   <- "NA";
        proj$dataset2$blob_area_firstQ   <- "NA";
        proj$dataset2$blob_area_conv_hull_firstQ   <- "NA";
        proj$dataset2$blob_peri_firstQ   <- "NA";
        proj$dataset2$blob_peri_conv_hull_firstQ   <- "NA";
        proj$dataset2$blob_feret_firstQ   <- "NA";
        proj$dataset2$blob_firstQ_feret_firstQ   <- "NA";
        proj$dataset2$blob_max_inscribed_diameter_firstQ   <- "NA";
        proj$dataset2$blob_area_equivalent_diameter_firstQ   <- "NA";
        proj$dataset2$blob_long_side_MBR_firstQ   <- "NA";
        proj$dataset2$blob_short_side_MBR_firstQ   <- "NA";
        proj$dataset2$blob_aspect_ratio_firstQ   <- "NA";
        proj$dataset2$blob_area_peri_firstQ   <- "NA";
        proj$dataset2$blob_circularity_firstQ   <- "NA";
        proj$dataset2$blob_elongation_firstQ   <- "NA";
        proj$dataset2$blob_convexity_firstQ   <- "NA";
        proj$dataset2$blob_solidity_firstQ   <- "NA";
        proj$dataset2$blob_num_holes_firstQ   <- "NA";
        proj$dataset2$blob_thinnes_rt_firstQ   <- "NA";
        proj$dataset2$blob_contour_temp_firstQ   <- "NA";
        proj$dataset2$blob_orientation_firstQ   <- "NA";
        proj$dataset2$blob_fract_dim_firstQ   <- "NA";
        proj$dataset2$blob_fract_dim_goodness_firstQ   <- "NA";
        
        proj$dataset2$blob_label_medians   <- "NA";
        proj$dataset2$blob_X_medians   <- "NA";
        proj$dataset2$blob_Y_medians   <- "NA";
        proj$dataset2$blob_area_medians   <- "NA";
        proj$dataset2$blob_area_conv_hull_medians   <- "NA";
        proj$dataset2$blob_peri_medians   <- "NA";
        proj$dataset2$blob_peri_conv_hull_medians   <- "NA";
        proj$dataset2$blob_feret_medians   <- "NA";
        proj$dataset2$blob_medians_feret_medians   <- "NA";
        proj$dataset2$blob_max_inscribed_diameter_medians   <- "NA";
        proj$dataset2$blob_area_equivalent_diameter_medians   <- "NA";
        proj$dataset2$blob_long_side_MBR_medians   <- "NA";
        proj$dataset2$blob_short_side_MBR_medians   <- "NA";
        proj$dataset2$blob_aspect_ratio_medians   <- "NA";
        proj$dataset2$blob_area_peri_medians   <- "NA";
        proj$dataset2$blob_circularity_medians   <- "NA";
        proj$dataset2$blob_elongation_medians   <- "NA";
        proj$dataset2$blob_convexity_medians   <- "NA";
        proj$dataset2$blob_solidity_medians   <- "NA";
        proj$dataset2$blob_num_holes_medians   <- "NA";
        proj$dataset2$blob_thinnes_rt_medians   <- "NA";
        proj$dataset2$blob_contour_temp_medians   <- "NA";
        proj$dataset2$blob_orientation_medians   <- "NA";
        proj$dataset2$blob_fract_dim_medians   <- "NA";
        proj$dataset2$blob_fract_dim_goodness_medians   <- "NA";
        
        proj$dataset2$blob_label_means   <- "NA";
        proj$dataset2$blob_X_means   <- "NA";
        proj$dataset2$blob_Y_means   <- "NA";
        proj$dataset2$blob_area_means   <- "NA";
        proj$dataset2$blob_area_conv_hull_means   <- "NA";
        proj$dataset2$blob_peri_means   <- "NA";
        proj$dataset2$blob_peri_conv_hull_means   <- "NA";
        proj$dataset2$blob_feret_means   <- "NA";
        proj$dataset2$blob_means_feret_means   <- "NA";
        proj$dataset2$blob_max_inscribed_diameter_means   <- "NA";
        proj$dataset2$blob_area_equivalent_diameter_means   <- "NA";
        proj$dataset2$blob_long_side_MBR_means   <- "NA";
        proj$dataset2$blob_short_side_MBR_means   <- "NA";
        proj$dataset2$blob_aspect_ratio_means   <- "NA";
        proj$dataset2$blob_area_peri_means   <- "NA";
        proj$dataset2$blob_circularity_means   <- "NA";
        proj$dataset2$blob_elongation_means   <- "NA";
        proj$dataset2$blob_convexity_means   <- "NA";
        proj$dataset2$blob_solidity_means   <- "NA";
        proj$dataset2$blob_num_holes_means   <- "NA";
        proj$dataset2$blob_thinnes_rt_means   <- "NA";
        proj$dataset2$blob_contour_temp_means   <- "NA";
        proj$dataset2$blob_orientation_means   <- "NA";
        proj$dataset2$blob_fract_dim_means   <- "NA";
        proj$dataset2$blob_fract_dim_goodness_means   <- "NA";
        
        proj$dataset2$blob_label_thirdQ   <- "NA";
        proj$dataset2$blob_X_thirdQ   <- "NA";
        proj$dataset2$blob_Y_thirdQ   <- "NA";
        proj$dataset2$blob_area_thirdQ   <- "NA";
        proj$dataset2$blob_area_conv_hull_thirdQ   <- "NA";
        proj$dataset2$blob_peri_thirdQ   <- "NA";
        proj$dataset2$blob_peri_conv_hull_thirdQ   <- "NA";
        proj$dataset2$blob_feret_thirdQ   <- "NA";
        proj$dataset2$blob_thirdQ_feret_thirdQ   <- "NA";
        proj$dataset2$blob_max_inscribed_diameter_thirdQ   <- "NA";
        proj$dataset2$blob_area_equivalent_diameter_thirdQ   <- "NA";
        proj$dataset2$blob_long_side_MBR_thirdQ   <- "NA";
        proj$dataset2$blob_short_side_MBR_thirdQ   <- "NA";
        proj$dataset2$blob_aspect_ratio_thirdQ   <- "NA";
        proj$dataset2$blob_area_peri_thirdQ   <- "NA";
        proj$dataset2$blob_circularity_thirdQ   <- "NA";
        proj$dataset2$blob_elongation_thirdQ   <- "NA";
        proj$dataset2$blob_convexity_thirdQ   <- "NA";
        proj$dataset2$blob_solidity_thirdQ   <- "NA";
        proj$dataset2$blob_num_holes_thirdQ   <- "NA";
        proj$dataset2$blob_thinnes_rt_thirdQ   <- "NA";
        proj$dataset2$blob_contour_temp_thirdQ   <- "NA";
        proj$dataset2$blob_orientation_thirdQ   <- "NA";
        proj$dataset2$blob_fract_dim_thirdQ   <- "NA";
        proj$dataset2$blob_fract_dim_goodness_thirdQ   <- "NA";
        
        proj$dataset2$blob_label_max   <- "NA";
        proj$dataset2$blob_X_max   <- "NA";
        proj$dataset2$blob_Y_max   <- "NA";
        proj$dataset2$blob_area_max   <- "NA";
        proj$dataset2$blob_area_conv_hull_max   <- "NA";
        proj$dataset2$blob_peri_max   <- "NA";
        proj$dataset2$blob_peri_conv_hull_max   <- "NA";
        proj$dataset2$blob_feret_max   <- "NA";
        proj$dataset2$blob_max_feret_max   <- "NA";
        proj$dataset2$blob_max_inscribed_diameter_max   <- "NA";
        proj$dataset2$blob_area_equivalent_diameter_max   <- "NA";
        proj$dataset2$blob_long_side_MBR_max   <- "NA";
        proj$dataset2$blob_short_side_MBR_max   <- "NA";
        proj$dataset2$blob_aspect_ratio_max   <- "NA";
        proj$dataset2$blob_area_peri_max   <- "NA";
        proj$dataset2$blob_circularity_max   <- "NA";
        proj$dataset2$blob_elongation_max   <- "NA";
        proj$dataset2$blob_convexity_max   <- "NA";
        proj$dataset2$blob_solidity_max   <- "NA";
        proj$dataset2$blob_num_holes_max   <- "NA";
        proj$dataset2$blob_thinnes_rt_max   <- "NA";
        proj$dataset2$blob_contour_temp_max   <- "NA";
        proj$dataset2$blob_orientation_max   <- "NA";
        proj$dataset2$blob_fract_dim_max   <- "NA";
        proj$dataset2$blob_fract_dim_goodness_max   <- "NA";
        
        
        proj$dataset2$blob_slices_counts_min   <- "NA";
        
        proj$dataset2$blob_slices_counts_firstQ   <- "NA";
        
        proj$dataset2$blob_slices_counts_medians   <- "NA";
        
        proj$dataset2$blob_slices_counts_means   <- "NA";
        
        proj$dataset2$blob_slices_counts_thirdQ   <- "NA";
        
        proj$dataset2$blob_slices_counts_max   <- "NA";
        
        
        proj$dataset2$blob_calculated_volumes_min   <- "NA";
        
        proj$dataset2$blob_calculated_volumes_firstQ   <- "NA";
        
        proj$dataset2$blob_calculated_volumes_medians   <- "NA";
        
        proj$dataset2$blob_calculated_volumes_means   <- "NA";
        
        proj$dataset2$blob_calculated_volumes_thirdQ   <- "NA";
        
        proj$dataset2$blob_calculated_volumes_max   <- "NA";
        
        
      }
      
      if((length(empty_half_stack)==0 & !is.null(empty_half_stack)) || !(length(empty_half_stack)==1 & empty_blobs == "NA")){
        proj$dataset2$half_stack_lobes_area_min   <- half_stack_lobes_features_min_cumul[,1];
        proj$dataset2$half_stack_lobes_mean_min   <- half_stack_lobes_features_min_cumul[,2];
        proj$dataset2$half_stack_lobes_min_min   <- half_stack_lobes_features_min_cumul[,3];
        proj$dataset2$half_stack_lobes_max_min   <- half_stack_lobes_features_min_cumul[,4];
        
        proj$dataset2$half_stack_lobes_area_firstQ   <- half_stack_lobes_features_firstQ_cumul[,1];
        proj$dataset2$half_stack_lobes_mean_firstQ   <- half_stack_lobes_features_firstQ_cumul[,2];
        proj$dataset2$half_stack_lobes_min_firstQ   <- half_stack_lobes_features_firstQ_cumul[,3];
        proj$dataset2$half_stack_lobes_max_firstQ   <- half_stack_lobes_features_firstQ_cumul[,4];
        
        proj$dataset2$half_stack_lobes_area_medians   <- half_stack_lobes_features_medians_cumul[,1];
        proj$dataset2$half_stack_lobes_mean_medians   <- half_stack_lobes_features_medians_cumul[,2];
        proj$dataset2$half_stack_lobes_min_medians   <- half_stack_lobes_features_medians_cumul[,3];
        proj$dataset2$half_stack_lobes_max_medians   <- half_stack_lobes_features_medians_cumul[,4];
        
        proj$dataset2$half_stack_lobes_area_means   <- half_stack_lobes_features_means_cumul[,1];
        proj$dataset2$half_stack_lobes_mean_means   <- half_stack_lobes_features_means_cumul[,2];
        proj$dataset2$half_stack_lobes_min_means   <- half_stack_lobes_features_means_cumul[,3];
        proj$dataset2$half_stack_lobes_max_means   <- half_stack_lobes_features_means_cumul[,4];
        
        proj$dataset2$half_stack_lobes_area_thirdQ   <- half_stack_lobes_features_thirdQ_cumul[,1];
        proj$dataset2$half_stack_lobes_mean_thirdQ   <- half_stack_lobes_features_thirdQ_cumul[,2];
        proj$dataset2$half_stack_lobes_min_thirdQ   <- half_stack_lobes_features_thirdQ_cumul[,3];
        proj$dataset2$half_stack_lobes_max_thirdQ   <- half_stack_lobes_features_thirdQ_cumul[,4];
        
        proj$dataset2$half_stack_lobes_area_max   <- half_stack_lobes_features_max_cumul[,1];
        proj$dataset2$half_stack_lobes_mean_max   <- half_stack_lobes_features_max_cumul[,2];
        proj$dataset2$half_stack_lobes_min_max   <- half_stack_lobes_features_max_cumul[,3];
        proj$dataset2$half_stack_lobes_max_max   <- half_stack_lobes_features_max_cumul[,4];
        
      }
      
      if((length(empty_stats)==0 & !is.null(empty_stats)) || (!(length(empty_stats)==1 & empty_stats=="NA"))){
        proj$dataset2$statistics_area_min   <- statistics_features_min_cumul[,1];
        proj$dataset2$statistics_mean_min   <- statistics_features_min_cumul[,2];
        proj$dataset2$statistics_stddev_patient_min   <- statistics_features_min_cumul[,3];
        proj$dataset2$statistics_mode_min   <- statistics_features_min_cumul[,4];
        proj$dataset2$statistics_min_min   <- statistics_features_min_cumul[,5];
        proj$dataset2$statistics_max_min   <- statistics_features_min_cumul[,6];
        proj$dataset2$statistics_voxels_min   <- statistics_features_min_cumul[,7];
        proj$dataset2$statistics_volume_mm_min   <- statistics_features_min_cumul[,8];
        #proj$dataset2$statistics_volume_perc_min   <- statistics_features_min_cumul[,9]; #buggy
        
        proj$dataset2$statistics_area_firstQ   <- statistics_features_firstQ_cumul[,1];
        proj$dataset2$statistics_mean_firstQ   <- statistics_features_firstQ_cumul[,2];
        proj$dataset2$statistics_stddev_patient_firstQ   <- statistics_features_firstQ_cumul[,3];
        proj$dataset2$statistics_mode_firstQ   <- statistics_features_firstQ_cumul[,4];
        proj$dataset2$statistics_firstQ_firstQ   <- statistics_features_firstQ_cumul[,5];
        proj$dataset2$statistics_max_firstQ   <- statistics_features_firstQ_cumul[,6];
        proj$dataset2$statistics_voxels_firstQ   <- statistics_features_firstQ_cumul[,7];
        proj$dataset2$statistics_volume_mm_firstQ   <- statistics_features_firstQ_cumul[,8];
        #proj$dataset2$statistics_volume_perc_firstQ   <- statistics_features_firstQ_cumul[,9];
        
        proj$dataset2$statistics_area_medians   <- statistics_features_medians_cumul[,1];
        proj$dataset2$statistics_mean_medians   <- statistics_features_medians_cumul[,2];
        proj$dataset2$statistics_stddev_patient_medians   <- statistics_features_medians_cumul[,3];
        proj$dataset2$statistics_mode_medians   <- statistics_features_medians_cumul[,4];
        proj$dataset2$statistics_medians_medians   <- statistics_features_medians_cumul[,5];
        proj$dataset2$statistics_max_medians   <- statistics_features_medians_cumul[,6];
        proj$dataset2$statistics_voxels_medians   <- statistics_features_medians_cumul[,7];
        proj$dataset2$statistics_volume_mm_medians   <- statistics_features_medians_cumul[,8];
        #proj$dataset2$statistics_volume_perc_medians   <- statistics_features_medians_cumul[,9];
        
        proj$dataset2$statistics_area_means   <- statistics_features_means_cumul[,1];
        proj$dataset2$statistics_mean_means   <- statistics_features_means_cumul[,2];
        proj$dataset2$statistics_stddev_patient_means   <- statistics_features_means_cumul[,3];
        proj$dataset2$statistics_mode_means   <- statistics_features_means_cumul[,4];
        proj$dataset2$statistics_means_means   <- statistics_features_means_cumul[,5];
        proj$dataset2$statistics_max_means   <- statistics_features_means_cumul[,6];
        proj$dataset2$statistics_voxels_means   <- statistics_features_means_cumul[,7];
        proj$dataset2$statistics_volume_mm_means   <- statistics_features_means_cumul[,8];
        #proj$dataset2$statistics_volume_perc_means   <- statistics_features_means_cumul[,9];
        
        proj$dataset2$statistics_area_thirdQ   <- statistics_features_thirdQ_cumul[,1];
        proj$dataset2$statistics_mean_thirdQ   <- statistics_features_thirdQ_cumul[,2];
        proj$dataset2$statistics_stddev_patient_thirdQ   <- statistics_features_thirdQ_cumul[,3];
        proj$dataset2$statistics_mode_thirdQ   <- statistics_features_thirdQ_cumul[,4];
        proj$dataset2$statistics_thirdQ_thirdQ   <- statistics_features_thirdQ_cumul[,5];
        proj$dataset2$statistics_max_thirdQ   <- statistics_features_thirdQ_cumul[,6];
        proj$dataset2$statistics_voxels_thirdQ   <- statistics_features_thirdQ_cumul[,7];
        proj$dataset2$statistics_volume_mm_thirdQ   <- statistics_features_thirdQ_cumul[,8];
        #proj$dataset2$statistics_volume_perc_thirdQ   <- statistics_features_thirdQ_cumul[,9];
        
        proj$dataset2$statistics_area_max   <- statistics_features_max_cumul[,1];
        proj$dataset2$statistics_mean_max   <- statistics_features_max_cumul[,2];
        proj$dataset2$statistics_stddev_patient_max   <- statistics_features_max_cumul[,3];
        proj$dataset2$statistics_mode_max   <- statistics_features_max_cumul[,4];
        proj$dataset2$statistics_max_max   <- statistics_features_max_cumul[,5];
        proj$dataset2$statistics_max_max   <- statistics_features_max_cumul[,6];
        proj$dataset2$statistics_voxels_max   <- statistics_features_max_cumul[,7];
        proj$dataset2$statistics_volume_mm_max   <- statistics_features_max_cumul[,8];
        #proj$dataset2$statistics_volume_perc_max   <- statistics_features_max_cumul[,9];
      }else{
        proj$dataset2$statistics_area_min <-  "NA";
        proj$dataset2$statistics_mean_min <-  "NA";
        proj$dataset2$statistics_stddev_patient_min <-  "NA";
        proj$dataset2$statistics_mode_min <-  "NA";
        proj$dataset2$statistics_min_min  <- "NA";
        proj$dataset2$statistics_max_min  <- "NA";
        proj$dataset2$statistics_voxels_min <-  "NA";
        proj$dataset2$statistics_volume_mm_min  <- "NA";
        #proj$dataset2$statistics_volume_perc_min   "NA"; #buggy
        
        proj$dataset2$statistics_area_firstQ   <- "NA";
        proj$dataset2$statistics_mean_firstQ   <- "NA";
        proj$dataset2$statistics_stddev_patient_firstQ   <- "NA";
        proj$dataset2$statistics_mode_firstQ   <- "NA";
        proj$dataset2$statistics_firstQ_firstQ   <- "NA";
        proj$dataset2$statistics_max_firstQ   <- "NA";
        proj$dataset2$statistics_voxels_firstQ   <- "NA";
        proj$dataset2$statistics_volume_mm_firstQ   <- "NA";
        #proj$dataset2$statistics_volume_perc_firstQ   <- "NA";
        
        proj$dataset2$statistics_area_medians   <- "NA";
        proj$dataset2$statistics_mean_medians   <- "NA";
        proj$dataset2$statistics_stddev_patient_medians   <- "NA";
        proj$dataset2$statistics_mode_medians   <- "NA";
        proj$dataset2$statistics_medians_medians   <- "NA";
        proj$dataset2$statistics_max_medians   <- "NA";
        proj$dataset2$statistics_voxels_medians   <- "NA";
        proj$dataset2$statistics_volume_mm_medians   <- "NA";
        #proj$dataset2$statistics_volume_perc_medians   <- "NA";
        
        proj$dataset2$statistics_area_means   <- "NA";
        proj$dataset2$statistics_mean_means   <- "NA";
        proj$dataset2$statistics_stddev_patient_means   <- "NA";
        proj$dataset2$statistics_mode_means   <- "NA";
        proj$dataset2$statistics_means_means   <- "NA";
        proj$dataset2$statistics_max_means   <- "NA";
        proj$dataset2$statistics_voxels_means   <- "NA";
        proj$dataset2$statistics_volume_mm_means   <- "NA";
        #proj$dataset2$statistics_volume_perc_means   <- "NA";
        
        proj$dataset2$statistics_area_thirdQ   <- "NA";
        proj$dataset2$statistics_mean_thirdQ   <- "NA";
        proj$dataset2$statistics_stddev_patient_thirdQ   <- "NA";
        proj$dataset2$statistics_mode_thirdQ   <- "NA";
        proj$dataset2$statistics_thirdQ_thirdQ   <- "NA";
        proj$dataset2$statistics_max_thirdQ   <- "NA";
        proj$dataset2$statistics_voxels_thirdQ   <- "NA";
        proj$dataset2$statistics_volume_mm_thirdQ   <- "NA";
        #proj$dataset2$statistics_volume_perc_thirdQ   <- "NA";
        
        proj$dataset2$statistics_area_max <-  "NA";
        proj$dataset2$statistics_mean_max <-  "NA";
        proj$dataset2$statistics_stddev_patient_max <-  "NA";
        proj$dataset2$statistics_mode_max <-  "NA";
        proj$dataset2$statistics_max_max  <- "NA";
        proj$dataset2$statistics_max_max  <- "NA";
        proj$dataset2$statistics_voxels_max <-  "NA";
        proj$dataset2$statistics_volume_mm_max  <- "NA";
        #proj$dataset2$statistics_volume_perc_max   <-"NA";
        
      }    
      
      if((((length(empty_results)==0 & !is.null(empty_results)) || !((length(empty_results)==1 & empty_results=="NA"))))){
        proj$dataset2$results_area_min   <- results_features_min_cumul[,1];
        proj$dataset2$results_mean_min   <- results_features_min_cumul[,2];
        proj$dataset2$results_min_min   <- results_features_min_cumul[,3];
        proj$dataset2$results_max_min   <- results_features_min_cumul[,4];
        
        proj$dataset2$results_area_firstQ   <- results_features_firstQ_cumul[,1];
        proj$dataset2$results_mean_firstQ   <- results_features_firstQ_cumul[,2];
        proj$dataset2$results_min_firstQ   <- results_features_firstQ_cumul[,3];
        proj$dataset2$results_max_firstQ   <- results_features_firstQ_cumul[,4];
        
        proj$dataset2$results_area_medians   <- results_features_medians_cumul[,1];
        proj$dataset2$results_mean_medians   <- results_features_medians_cumul[,2];
        proj$dataset2$results_min_medians   <- results_features_medians_cumul[,3];
        proj$dataset2$results_max_medians   <- results_features_medians_cumul[,4];
        
        proj$dataset2$results_area_means   <- results_features_means_cumul[,1];
        proj$dataset2$results_mean_means   <- results_features_means_cumul[,2];
        proj$dataset2$results_min_means   <- results_features_means_cumul[,3];
        proj$dataset2$results_max_means   <- results_features_means_cumul[,4];
        
        proj$dataset2$results_area_thirdQ   <- results_features_thirdQ_cumul[,1];
        proj$dataset2$results_mean_thirdQ   <- results_features_thirdQ_cumul[,2];
        proj$dataset2$results_min_thirdQ   <- results_features_thirdQ_cumul[,3];
        proj$dataset2$results_max_thirdQ   <- results_features_thirdQ_cumul[,4];
        
        proj$dataset2$results_area_max   <- results_features_max_cumul[,1];
        proj$dataset2$results_mean_max   <- results_features_max_cumul[,2];
        proj$dataset2$results_min_max   <- results_features_max_cumul[,3];
        proj$dataset2$results_max_max   <- results_features_max_cumul[,4];
        
      }
      else{
        
        proj$dataset2$results_area_min  <- "NA";
        proj$dataset2$results_mean_min<-   "NA";
        proj$dataset2$results_min_min <-  "NA";
        proj$dataset2$results_max_min     <-"NA";
        
        proj$dataset2$results_area_firstQ   <-"NA";
        proj$dataset2$results_mean_firstQ   <-"NA";
        proj$dataset2$results_min_firstQ   <-"NA";
        proj$dataset2$results_max_firstQ   <-"NA";
        
        proj$dataset2$results_area_medians     <-"NA";
        proj$dataset2$results_mean_medians     <-"NA";
        proj$dataset2$results_min_medians     <-"NA";
        proj$dataset2$results_max_medians     <-"NA";
        
        proj$dataset2$results_area_means     <-"NA";
        proj$dataset2$results_mean_means     <-"NA";
        proj$dataset2$results_min_means     <-"NA";
        proj$dataset2$results_max_means     <-"NA";
        
        proj$dataset2$results_area_thirdQ   <-"NA";
        proj$dataset2$results_mean_thirdQ   <-"NA";
        proj$dataset2$results_min_thirdQ   <-"NA";
        proj$dataset2$results_max_thirdQ   <-"NA";
        
        proj$dataset2$results_area_max     <-"NA";
        proj$dataset2$results_mean_max     <-"NA";
        proj$dataset2$results_min_max     <-"NA";
        proj$dataset2$results_max_max     <-"NA";
      }
      proj$dataset2$ctmeta_instance_number_min   <- ctmeta_features_min_cumul[,1];
      proj$dataset2$ctmeta_slice_location_min   <- ctmeta_features_min_cumul[,2];
      proj$dataset2$ctmeta_image_orientation_patient_min   <- ctmeta_features_min_cumul[,3];
      proj$dataset2$ctmeta_image_min   <- ctmeta_features_min_cumul[,4];
      proj$dataset2$ctmeta_position_min   <- ctmeta_features_min_cumul[,5];
      proj$dataset2$ctmeta_samples_per_pixel_min   <- ctmeta_features_min_cumul[,6];
      proj$dataset2$ctmeta_rows_min   <- ctmeta_features_min_cumul[,7];
      proj$dataset2$ctmeta_columns_min   <- ctmeta_features_min_cumul[,8];
      proj$dataset2$ctmeta_bits_allocated_min   <- ctmeta_features_min_cumul[,9];
      proj$dataset2$ctmeta_bits_stored_min   <- ctmeta_features_min_cumul[,10];
      proj$dataset2$ctmeta_window_center_min   <- ctmeta_features_min_cumul[,11];
      proj$dataset2$ctmeta_window_width_min   <- ctmeta_features_min_cumul[,12];
      proj$dataset2$ctmeta_rescale_intercept_min   <- ctmeta_features_min_cumul[,13];
      proj$dataset2$ctmeta_rescale_slope_min   <- ctmeta_features_min_cumul[,14];
      proj$dataset2$ctmeta_pixel_spacing_row_min   <- ctmeta_features_min_cumul[,15];
      proj$dataset2$ctmeta_pixel_spacing_col_min   <- ctmeta_features_min_cumul[,16];
      
      
      proj$dataset2$ctmeta_instance_number_firstQ   <- ctmeta_features_firstQ_cumul[,1];
      proj$dataset2$ctmeta_slice_location_firstQ   <- ctmeta_features_firstQ_cumul[,2];
      proj$dataset2$ctmeta_image_orientation_patient_firstQ   <- ctmeta_features_firstQ_cumul[,3];
      proj$dataset2$ctmeta_image_firstQ   <- ctmeta_features_firstQ_cumul[,4];
      proj$dataset2$ctmeta_position_firstQ   <- ctmeta_features_firstQ_cumul[,5];
      proj$dataset2$ctmeta_samples_per_pixel_firstQ   <- ctmeta_features_firstQ_cumul[,6];
      proj$dataset2$ctmeta_rows_firstQ   <- ctmeta_features_firstQ_cumul[,7];
      proj$dataset2$ctmeta_columns_firstQ   <- ctmeta_features_firstQ_cumul[,8];
      proj$dataset2$ctmeta_bits_allocated_firstQ   <- ctmeta_features_firstQ_cumul[,9];
      proj$dataset2$ctmeta_bits_stored_firstQ   <- ctmeta_features_firstQ_cumul[,10];
      proj$dataset2$ctmeta_window_center_firstQ   <- ctmeta_features_firstQ_cumul[,11];
      proj$dataset2$ctmeta_window_width_firstQ   <- ctmeta_features_firstQ_cumul[,12];
      proj$dataset2$ctmeta_rescale_intercept_firstQ   <- ctmeta_features_firstQ_cumul[,13];
      proj$dataset2$ctmeta_rescale_slope_firstQ   <- ctmeta_features_firstQ_cumul[,14];
      proj$dataset2$ctmeta_pixel_spacing_row_firstQ   <- ctmeta_features_firstQ_cumul[,15];
      proj$dataset2$ctmeta_pixel_spacing_col_firstQ   <- ctmeta_features_firstQ_cumul[,16];
      
      
      proj$dataset2$ctmeta_instance_number_medians   <- ctmeta_features_medians_cumul[,1];
      proj$dataset2$ctmeta_slice_location_medians   <- ctmeta_features_medians_cumul[,2];
      proj$dataset2$ctmeta_image_orientation_patient_medians   <- ctmeta_features_medians_cumul[,3];
      proj$dataset2$ctmeta_image_medians   <- ctmeta_features_medians_cumul[,4];
      proj$dataset2$ctmeta_position_medians   <- ctmeta_features_medians_cumul[,5];
      proj$dataset2$ctmeta_samples_per_pixel_medians   <- ctmeta_features_medians_cumul[,6];
      proj$dataset2$ctmeta_rows_medians   <- ctmeta_features_medians_cumul[,7];
      proj$dataset2$ctmeta_columns_medians   <- ctmeta_features_medians_cumul[,8];
      proj$dataset2$ctmeta_bits_allocated_medians   <- ctmeta_features_medians_cumul[,9];
      proj$dataset2$ctmeta_bits_stored_medians   <- ctmeta_features_medians_cumul[,10];
      proj$dataset2$ctmeta_window_center_medians   <- ctmeta_features_medians_cumul[,11];
      proj$dataset2$ctmeta_window_width_medians   <- ctmeta_features_medians_cumul[,12];
      proj$dataset2$ctmeta_rescale_intercept_medians   <- ctmeta_features_medians_cumul[,13];
      proj$dataset2$ctmeta_rescale_slope_medians   <- ctmeta_features_medians_cumul[,14];
      proj$dataset2$ctmeta_pixel_spacing_row_medians   <- ctmeta_features_medians_cumul[,15];
      proj$dataset2$ctmeta_pixel_spacing_col_medians   <- ctmeta_features_medians_cumul[,16];
      
      
      proj$dataset2$ctmeta_instance_number_means   <- ctmeta_features_means_cumul[,1];
      proj$dataset2$ctmeta_slice_location_means   <- ctmeta_features_means_cumul[,2];
      proj$dataset2$ctmeta_image_orientation_patient_means   <- ctmeta_features_means_cumul[,3];
      proj$dataset2$ctmeta_image_means   <- ctmeta_features_means_cumul[,4];
      proj$dataset2$ctmeta_position_means   <- ctmeta_features_means_cumul[,5];
      proj$dataset2$ctmeta_samples_per_pixel_means   <- ctmeta_features_means_cumul[,6];
      proj$dataset2$ctmeta_rows_means   <- ctmeta_features_means_cumul[,7];
      proj$dataset2$ctmeta_columns_means   <- ctmeta_features_means_cumul[,8];
      proj$dataset2$ctmeta_bits_allocated_means   <- ctmeta_features_means_cumul[,9];
      proj$dataset2$ctmeta_bits_stored_means   <- ctmeta_features_means_cumul[,10];
      proj$dataset2$ctmeta_window_center_means   <- ctmeta_features_means_cumul[,11];
      proj$dataset2$ctmeta_window_width_means   <- ctmeta_features_means_cumul[,12];
      proj$dataset2$ctmeta_rescale_intercept_means   <- ctmeta_features_means_cumul[,13];
      proj$dataset2$ctmeta_rescale_slope_means   <- ctmeta_features_means_cumul[,14];
      proj$dataset2$ctmeta_pixel_spacing_row_means   <- ctmeta_features_means_cumul[,15];
      proj$dataset2$ctmeta_pixel_spacing_col_means   <- ctmeta_features_means_cumul[,16];
      
      
      proj$dataset2$ctmeta_instance_number_thirdQ   <- ctmeta_features_thirdQ_cumul[,1];
      proj$dataset2$ctmeta_slice_location_thirdQ   <- ctmeta_features_thirdQ_cumul[,2];
      proj$dataset2$ctmeta_image_orientation_patient_thirdQ   <- ctmeta_features_thirdQ_cumul[,3];
      proj$dataset2$ctmeta_image_thirdQ   <- ctmeta_features_thirdQ_cumul[,4];
      proj$dataset2$ctmeta_position_thirdQ   <- ctmeta_features_thirdQ_cumul[,5];
      proj$dataset2$ctmeta_samples_per_pixel_thirdQ   <- ctmeta_features_thirdQ_cumul[,6];
      proj$dataset2$ctmeta_rows_thirdQ   <- ctmeta_features_thirdQ_cumul[,7];
      proj$dataset2$ctmeta_columns_thirdQ   <- ctmeta_features_thirdQ_cumul[,8];
      proj$dataset2$ctmeta_bits_allocated_thirdQ   <- ctmeta_features_thirdQ_cumul[,9];
      proj$dataset2$ctmeta_bits_stored_thirdQ   <- ctmeta_features_thirdQ_cumul[,10];
      proj$dataset2$ctmeta_window_center_thirdQ   <- ctmeta_features_thirdQ_cumul[,11];
      proj$dataset2$ctmeta_window_width_thirdQ   <- ctmeta_features_thirdQ_cumul[,12];
      proj$dataset2$ctmeta_rescale_intercept_thirdQ   <- ctmeta_features_thirdQ_cumul[,13];
      proj$dataset2$ctmeta_rescale_slope_thirdQ   <- ctmeta_features_thirdQ_cumul[,14];
      proj$dataset2$ctmeta_pixel_spacing_row_thirdQ   <- ctmeta_features_thirdQ_cumul[,15];
      proj$dataset2$ctmeta_pixel_spacing_col_thirdQ   <- ctmeta_features_thirdQ_cumul[,16];
      
      
      proj$dataset2$ctmeta_instance_number_max   <- ctmeta_features_max_cumul[,1];
      proj$dataset2$ctmeta_slice_location_max   <- ctmeta_features_max_cumul[,2];
      proj$dataset2$ctmeta_image_orientation_patient_max   <- ctmeta_features_max_cumul[,3];
      proj$dataset2$ctmeta_image_max   <- ctmeta_features_max_cumul[,4];
      proj$dataset2$ctmeta_position_max   <- ctmeta_features_max_cumul[,5];
      proj$dataset2$ctmeta_samples_per_pixel_max   <- ctmeta_features_max_cumul[,6];
      proj$dataset2$ctmeta_rows_max   <- ctmeta_features_max_cumul[,7];
      proj$dataset2$ctmeta_columns_max   <- ctmeta_features_max_cumul[,8];
      proj$dataset2$ctmeta_bits_allocated_max   <- ctmeta_features_max_cumul[,9];
      proj$dataset2$ctmeta_bits_stored_max   <- ctmeta_features_max_cumul[,10];
      proj$dataset2$ctmeta_window_center_max   <- ctmeta_features_max_cumul[,11];
      proj$dataset2$ctmeta_window_width_max   <- ctmeta_features_max_cumul[,12];
      proj$dataset2$ctmeta_rescale_intercept_max   <- ctmeta_features_max_cumul[,13];
      proj$dataset2$ctmeta_rescale_slope_max   <- ctmeta_features_max_cumul[,14];
      proj$dataset2$ctmeta_pixel_spacing_row_max   <- ctmeta_features_max_cumul[,15];
      proj$dataset2$ctmeta_pixel_spacing_col_max   <- ctmeta_features_max_cumul[,16];
      
      proj$dataset2 <- as.dataframe(proj$dataset2);
      
      filename <- paste(proj$final_path, paste(type, "_Dataset.csv", sep=""), sep="");
      con<-file(filename, encoding="utf8");
      
      if(type == "CANCER"){
        write.csv(paste(settings_paths$R_code_paths$meta_csv_cancer_directory, proj$dataset2, sep=""), file=con, row.names=FALSE);
      }else{
        write.csv(paste(settings_paths$R_code_paths$meta_csv_normal_directory, proj$dataset2, sep=""), file=con, row.names=FALSE);
      }
    }  

    if(type == "CANCER"){
      proj$final_1_dataset <- read.csv(paste(settings_paths$R_code_paths$meta_csv_cancer_directory,type,"_Dataset.csv", sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
      proj$final_1_dataset$cancer <- 1;
    }else{
      proj$final_2_dataset <- read.csv(paste(settings_paths$R_code_paths$meta_csv_normal_directory,type,"_Dataset.csv", sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
      proj$final_2_dataset$cancer <- 0;  
    }
    
    print(paste("--Processed: ", processed_counts, sep=""));  
    
  }
  
###BUGGY###
  #   proj$log_path <- paste(settings_paths$R_code_paths$working_directory, "/Log_K_1-1397_missing367.txt", sep="");
  # proj$log <-NULL;
  # proj$log_dataset <-NULL;
  # proj$log <- unlist(read.csv(proj$log_path, header=FALSE));
  # log_length <- length(proj$log);
  # log_counter_incr <- 0;
  # dataframe_counter <- 0;
  # additional_counter <- 0;
  # cumul_idx <- 0;
  # m<-1;
  # test_endpoint_offset <- 70;
  # mistakes<-FALSE; ##ADJUST THIS ONE
  # while(TRUE){
  #   off_idx <- 0; cumul_idx <- 0;
  #   dataframe_counter <- dataframe_counter + 1;
  #   temp = as.character(proj$log[[m + off_idx]]);
  #   temp = strsplit(temp, "/")[[1]][1];
  #   proj$log_dataset$log_id[dataframe_counter] <- str_trim(strsplit(temp," ")[[1]][4]);
  #   off_idx <- off_idx + 6; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$avg_max_background[dataframe_counter] <- strsplit(temp, "intensity: ")[[1]][2];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$avg_min_background[dataframe_counter] <- strsplit(temp, "intensity: ")[[1]][2];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$max_intensity_background[dataframe_counter] <- strsplit(temp, "MAX: ")[[1]][2];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$min_intensity_background[dataframe_counter] <- strsplit(temp, "MIN: ")[[1]][2];
  #   
  #   off_idx <- off_idx + 2; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   if(temp == "  -circular ct has been detected with high probability -- or artifact\\shiny artifact present (less probable)."){
  #     proj$log_dataset$circular_ct_very_likely[dataframe_counter] <- 1;    
  #     cumul_idx <- cumul_idx + 2;
  #     off_idx <- off_idx + 2;
  #   }else{
  #     proj$log_dataset$circular_ct_very_likely[dataframe_counter] <- 0;    
  #   }
  #   off_idx <- off_idx + 2; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$orig_avg_mean_intensity_background[dataframe_counter] <- strsplit(temp, "intensity: ")[[1]][2];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   if(temp == "-Adjusting minimum value (to -1024) ..."){
  #     proj$log_dataset$min_background_adjusted[dataframe_counter] <- 1;    
  #     cumul_idx <- cumul_idx + 1;
  #     off_idx <- off_idx + 1;
  #   }else{
  #     proj$log_dataset$min_background_adjusted[dataframe_counter] <- 0;    
  #   }
  #   off_idx <- off_idx + 0; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$new_max_intensity_background[dataframe_counter] <- strsplit(temp, "MAX: ")[[1]][2];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$new_min_intensity_background[dataframe_counter] <- strsplit(temp, "MIN: ")[[1]][2];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   proj$log_dataset$avg_mean_intensity_background[dataframe_counter] <- strsplit(temp, "intensity: ")[[1]][2];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp1 = proj$log[[off_idx+m]];
  #   off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #   temp2 = proj$log[[off_idx+m]];
  #   if(as.numeric(temp1) > as.numeric(temp2)){
  #     proj$log_dataset$reversed_stack[dataframe_counter] <-1;
  #   }else{
  #     proj$log_dataset$reversed_stack[dataframe_counter] <-0;
  #   }    
  #   off_idx <- off_idx + 5; cumul_idx <- off_idx;
  #   temp = as.character(proj$log[[m+off_idx]]);
  #   if("Rotating" %in% unlist(str_split(as.character(temp)," "))){
  #     proj$log_dataset$rotated_by[dataframe_counter] <- strsplit(temp, "g.): -")[[1]][2];
  #   }else{
  #     proj$log_dataset$rotated_by[dataframe_counter] <- "NA";
  #     off_idx <- off_idx -1;
  #   }
  #   ##mistakes|missing in imagej processing:
  #   if(mistakes){
  #     proj$log_dataset$num_lobe_cleared_outside[dataframe_counter] <- "NA";
  #     proj$log_dataset$num_lobe_cleared_all[dataframe_counter]  <- "NA";
  #     proj$log_dataset$total_slices[dataframe_counter]  <- "NA";
  #     proj$log_dataset$lobe_cleared_outside_ratio[dataframe_counter]  <- "NA";
  #     proj$log_dataset$top_lobe_slice[dataframe_counter]  <- "NA";
  #     proj$log_dataset$bottom_lobe_slice[dataframe_counter]  <- "NA";
  #     proj$log_dataset$avg_lobe_area[dataframe_counter]  <- "NA";
  #     proj$log_dataset$overall_time[dataframe_counter]  <- "NA";
  #   }else{    
  #     off_idx <- off_idx + 4; cumul_idx <- off_idx;
  #     temp = as.character(proj$log[[m+off_idx]]);
  #     proj$log_dataset$num_lobe_cleared_outside[dataframe_counter] <- strsplit(temp, "lobe: ")[[1]][2];
  #     off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #     temp = as.character(proj$log[[m+off_idx]]);
  #     proj$log_dataset$num_lobe_cleared_all[dataframe_counter] <- strsplit(temp, "all: ")[[1]][2];
  #     off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #     temp = as.character(proj$log[[m+off_idx]]);
  #     proj$log_dataset$total_slices[dataframe_counter] <- strsplit(temp, "slices: ")[[1]][2];
  #     proj$log_dataset$lobe_cleared_outside_ratio[dataframe_counter] <- as.character(as.numeric(proj$log_dataset$num_lobe_cleared_outside) / as.numeric(proj$log_dataset$total_slices));
  #     off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #     temp = as.character(proj$log[[m+off_idx]]);
  #     proj$log_dataset$top_lobe_slice[dataframe_counter] <- strsplit(temp, "slice: ")[[1]][2];
  #     off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #     temp = as.character(proj$log[[m+off_idx]]);
  #     proj$log_dataset$bottom_lobe_slice[dataframe_counter] <- strsplit(temp, "slice: ")[[1]][2];
  #     off_idx <- off_idx + 3; cumul_idx <- off_idx;
  #     temp = as.character(proj$log[[m+off_idx]]);
  #     proj$log_dataset$avg_lobe_area[dataframe_counter] <- strsplit(temp, "area: ")[[1]][2];
  #     off_idx <- off_idx + 1; cumul_idx <- off_idx;
  #     temp = as.character(proj$log[[m+off_idx]]);
  #     proj$log_dataset$overall_time[dataframe_counter] <- strsplit(temp, "time: ")[[1]][2];
  #   }
  #   cumul_idx <- cumul_idx+1;
  #   if(m > log_length-test_endpoint_offset){ 
  #     break;
  #   }
  #   m <- m + cumul_idx;
  # }      
  # proj$log_dataset <- as.data.frame(proj$log_dataset);  
  
  
  
  library(plyr)
  proj$final_final_dataset <- NULL;
  proj$final_final_dataset <- rbind.fill(proj$final_1_dataset, proj$final_2_dataset);
  
  #######################as.data.frame optional?
  #  proj$final_final_dataset <- proj$final_final_dataset[sapply(proj$dataset, function(x) length(unique(na.omit(x)))) > 1];
  #  proj$final_final_dataset <- proj$final_final_dataset[,colSums(is.na(proj$final_final_dataset))<nrow(proj$final_final_dataset)];
  #replace_zeroes = (colSums(proj$final_final_dataset[,2:length(proj$final_final_dataset)], na.rm=T) != 0)
  #proj$final_final_dataset <- proj$final_final_dataset[,replace_zeroes];
  ######################optionals?
  proj$final_final_dataset[rowSums(is.na(proj$final_final_dataset))==0,];
  ######################TODO - use df[rowSums(is.na(df))==0,]?
  
  dirname <- settings_paths$R_code_paths$meta_csv_final_directory;
  dir.create(dirname);
  con<-file(paste(dirname, "\\FINAL_FINAL.csv", sep=""), encoding="utf8");
  write.csv(proj$final_final_dataset, file=con, row.names=FALSE);
   
}

#------------------------------------------END------------------------------------------------------------
