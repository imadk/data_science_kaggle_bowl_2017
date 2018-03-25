#************************************MAIN***********************************#
#'processMetadataCSVDataset.r
#'
#'(C) 2017 - Imad Khoury
#'
#**************************************************#
library(jsonlite);
settings_url <- "D:/Kaggle/Bowl2017/SETTINGS.json";#
settings_paths <- fromJSON(txt=settings_url);      #
#**************************************************#

setwd(settings_paths$R_code_paths$working_directory);
source(paste(settings_paths$R_code_paths$code_directory,"/export_csv_submission.r",sep=""));
library(Hmisc, quietly=TRUE)
library(stringr)
library(methods)
library(uuid)
#########TODO: implement proper cv from a to z for models who do not support it inherently
#########TODO: subtract mean of all cts
#########TODO: meta blobs halftsact stats summaries (for all dataset and for filtered dataset)
SESSION<-paste(strsplit(UUIDgenerate(), "-")[[1]][1], strsplit(UUIDgenerate(), "-")[[1]][2], sep="");


#function - increment session id
newSession <- function(){
  SESSION <- SESSION + 1
  SESSION
}

#function - add to log
addToLog <- function(lfile, strng){
  sys_time <- as.character(Sys.time());
  strng <- paste("                                                                                            ----",sys_time,"----\n", strng);
  cat(strng, append = TRUE, sep="\n", file = lfile);
}

#function - get region in ct given my predetermined regioning and nodule xy coordinate
#TODO - TBRedone.
addNoduleRegion <- function(xxx, yyy){
  vvv <- floor(xxx / 128); uuu <- floor(yyy / 128);
  idx <- uuu*(512/128) + vvv;
  if(idx == 0 | idx == 1 | idx == 2 | idx == 3 | idx == 4 | idx == 7 | idx == 8 | idx == 11){ # ++ idx == 9 | idx == 10 for top and bottom of lobes 
    1				 	
  } 
  if(idx == 5 | idx == 6){#lungtrunkheart
    2				 	
  } 
  if(idx == 9 | idx == 10){ #spine etc
    3				 	
  } 
  0
}
#function - get region in ct given my predetermined regioning and nodule xy coordinate
addNoduleRegionMoreDetails <- function(xxx, yyy){
  vvv <- floor(as.numeric(xxx) / 128); uuu <- floor(as.numeric(yyy) / 128);
  idx <- uuu*(512/128) + vvv;
  regions <- rep(0, length(xxx));
  regions[which(idx==1 | idx==2)] <- 1000;#north
  regions[which(idx==4 | idx==7 | idx==8 | idx==11)] <- 2000;  #west\east
  regions[which(idx==9 | idx==10 | idx==13 | idx==14)] <- 3000;  #south
  regions[which(idx==5 | idx==6)] <- 5000;  #lungtrunkheart
  regions <- as.factor(regions);
  regions
}


#function - logloss matrix
view_final_dataset = function(df, project, testing=FALSE){
  if(!testing){
    View(df[proj$sample,c(project$input, project$ident,  project$target)]);
    df[proj$sample,c(project$input, project$ident, project$target)]
  }else{
    View(df[proj$sample,c(project$input, project$ident)]);
    df[proj$sample,c(project$input, project$ident)]
  }
}


#function - logloss per element
logLossVector <- function(pred, actual)
{
  score <- -(actual*log(pred) + (1-actual)*log(1-pred));
  score[actual==pred] <- 0;
  score[is.nan(score)] <- Inf;
  mean(score)
}

#function - logloss matrix
logLossMatrix = function(pred, actual){
  -1*mean(log(pred[stats::model.matrix(~ actual + 0) - pred > 0]))
}

#function - classification accuracy 
classificationAccuracy = function(pred, actual){
  length(which(pred == actual)) / length(pred) #pred and actual assumed same length as the shall 
}

#function - classification accuracy for probs
classificationAccuracyForProbs = function(pred, actual){
  sum(1 - abs(pred-actual)) / length(pred) 
}

#function - confusion matrix
confusionMatrix = function(pred, actual){
  pred[stats::model.matrix(~ actual + 0) - pred > 0]
}

par(ask=F);#turn-off plot asking
proj <- NULL;
proj$seed <- 66;

PRINT_BATCH_SIZE <- 5000;

#function - to numeric
asnumericPerColumn <- function(dframe, exc){
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      temp <- as.numeric(as.vector(dframe[,j]));
      dframe[,j] <-temp;
    }
  }
  dframe
}


#function - center xy coordinates around ct mean coordinates - ignore NAs
center_around_CT_CenterPerColumn <- function(dframe, exc) {
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      #todo try rm.na arg in max and min functions
      notna_rows<-which(dframe[j]!="NA");
      if(length(notna_rows)!=0){
        xcenter <- mean(as.numeric(as.vector(dframe[,j])) - rep((512/2)), length(as.numeric(as.vector(dframe[,j]))));
        as.numeric(as.vector(dframe[,j])) - rep(xcenter, (length(dframe[,j])));
        xcenter<-NULL;
        notna_rows<-NULL;
      }
    }
  }
  dframe
}

#function - center xy coordinates around 0 - ignore NAs
center_around_ZeroPerColumn <- function(dframe, exc) {
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      #todo try rm.na arg in max and min functions
      notna_rows<-which(dframe[j]!="NA");
      if(length(notna_rows)!=0){
        xcenter = mean(as.numeric(as.vector(dframe[,j]))) - rep((512/2), length(dframe[,j]));
        as.numeric(as.vector(dframe[,j])) - rep(xcenter, (length(dframe[,j])));
      }
    }
  }
  dframe
}


#function - normalize per data frame column - ignore NAs
normalizePerColumn <- function(dframe, exc){
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      #todo try rm.na arg in max and min functions
      notna_rows<-which(dframe[j]!="NA");
      if(length(notna_rows)!=0){
        temp <- as.numeric(as.vector(dframe[notna_rows,j]));
        dframe[notna_rows,j] <-(temp-min(temp))/(max(temp)-min(temp));
      }
    }
  }
  dframe
}

#function - normalize per data frame column - ignore NAs
normalizePerColumnLogPlusOne <- function(dframe, exc){
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      #todo try rm.na arg in max and min functions
      notna_rows<-which(dframe[j]!="NA");
      if(length(notna_rows)!=0){
        temp <- as.numeric(as.vector(dframe[notna_rows,j]));
        dframe[notna_rows,j] <-(log(temp+1));
      }
    }
  }
  dframe
}

#function - impute mean
imputeMean <- function(dframe, exc){
  impute.mean <- function(x) {replace(x, is.na(x) | is.nan(x) | is.infinite(x), mean(x[!is.na(x) & !is.nan(x) & !is.infinite(x)]))};
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      #todo try rm.na arg in max and min functions
      dframe[,j] <- impute.mean(dframe[,j]);
    }
  }
  dframe
}

#function - impute zero
imputeZero <- function(dframe, exc){
  impute.zero <- function(x) {replace(x, is.na(x) | is.nan(x) | is.infinite(x), 0)};
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      #todo try rm.na arg in max and min functions
      dframe[,j] <- impute.zero(dframe[,j]);
    }
  }
  dframe
}

#function - impute remove
imputeRemove <- function(dframe, exc){
  for(j in 1:length(dframe)){
    if(!((colnames(dframe[j]) %in% exc))){
      #todo try rm.na arg in max and min functions
      dframe[which(is.na(dframe[,j]) | is.nan(dframe[,j]) | is.infinite(dframe[,j])), j] <- NULL;
    }
  }
  dframe
}

#function #----------------- Validation Function -----------------##
validation <- function(project, H2O_save_dir, validation_file){
  lloss<-list();
  for(j in 1: length(project$models_names)){
    project$pr<-NULL;
    if(grepl("H2O", project$models_names[j])){
      h2OModels <- list.files(path = H2O_save_dir, recursive = FALSE);
    }else{
      the_model <- readRDS(paste(temp_dir, project$models_names[j], ".rds", sep=""));
    }
    print("");
    print(paste("Scoring ",project$models_names[j]," on validation set (size: ",(100-proj$PERC),"%)...",sep=""));
    if(grepl("H2O", project$models_names[j])){
      the_model <- h2o.loadModel(paste(H2O_save_dir, h2OModels[j], "", sep=""));
      newdata <- project$dataset[project$validate, c(project$input, project$ident)];
      con_h2o<-file(paste(temp_dir, "h2o_validationset.csv",sep=""), encoding="utf8");
      write.csv(newdata, file=con_h2o, row.names=FALSE);
      newdata <- h2o.importFile(path = paste(temp_dir, "h2o_validationset.csv",sep=""), destination_frame = "h2o_validationset");
      project$pr <- h2o.predict(the_model, newdata);
    }else{
      project$pr <- predict(the_model, newdata=(project$dataset[project$validate,c(project$input)]), type="prob")[,2];
    }
    
    project$aggregate_actual_validationset <- NULL;
    #print("Aggregating by ct_file_name using logical OR...");
    print("Aggregating by ct_file_name using mean of probabilities...");
    addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
    project$aggregate_actual_validationset <- data.frame("ct_file_name"=project$dataset$ct_file_name[project$validate], as.vector(project$dataset$cancer[project$validate]));
    agg_temp <- aggregate(x = project$aggregate_actual_validationset, by = list(project$aggregate_actual_validationset$ct_file_name), FUN = mean);
    project$aggregate_actual_validationset <- data.frame("ct_file_name"=agg_temp[1], "actual.pr"=agg_temp[3]);
    colnames(project$aggregate_actual_validationset) <- c("ct_file_name", "actual.pr");
    agg_temp<-NULL;
    
    project$aggregate_validationset <- NULL;
    if(grepl("H2O", project$models_names[j])){
      #print("Aggregating by ct_file_name using logical OR...");
      print("Aggregating by ct_file_name using mean of probabilities...");
      addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
      project$aggregate_validationset <- data.frame("ct_file_name"=project$dataset$ct_file_name[project$validate], as.vector(project$pr$p1));
      agg_temp <- aggregate(x = project$aggregate_validationset, by = list(project$aggregate_validationset$ct_file_name), FUN = mean);
      project$aggregate_validationset <- data.frame("ct_file_name"=agg_temp[1], "pred.pr"=agg_temp[3]);
      colnames(project$aggregate_validationset) <- c("ct_file_name", "pred.pr");
      agg_temp<-NULL;
    }else{
      #print("Aggregating by ct_file_name using logical OR...");
      print("Aggregating by ct_file_name using mean of probabilities..."); #TODO: REDO THIS-"MEAN"
      addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
      project$aggregate_validationset <- data.frame("ct_file_name"=project$dataset$ct_file_name[project$validate], project$pr);
      agg_temp <- aggregate(x = project$aggregate_validationset, by = list(project$aggregate_validationset$ct_file_name), FUN = mean);
      project$aggregate_validationset <- data.frame("ct_file_name"=agg_temp[1], agg_temp[3]);
      colnames(project$aggregate_validationset) <- c("ct_file_name", "pred.pr");
      agg_temp<-NULL;
    }
    pred_actual_dframe <- data.frame(project$aggregate_actual_validationset$actual.pr, project$aggregate_validationset$pred.pr);
    #  print("confusion matrix...");
    confx<-confusionMatrix(project$aggregate_validationset$pred.pr, project$aggregate_actual_validationset$actual.pr);
    #  print(confx);
    #print("logloss matrix...");
    #llossM<-logLossMatrix(project$aggregate_validationset$pred.pr, project$aggregate_actual_validationset$actual.pr);
    print("LogLoss (lower better)...:");
    addToLog(logFile, "LogLoss (lower better)...:");
    lloss[[j]] <- logLossVector(project$aggregate_validationset$pred.pr, project$aggregate_actual_validationset$actual.pr);
    print(lloss[[j]]);
    addToLog(logFile, paste(lloss[[j]]));
    print("Classification Accuracy (higher better)...:");
    addToLog(logFile, "Classification Accuracy (higher better)...:");
    classacc<-classificationAccuracyForProbs(project$aggregate_validationset$pred.pr, project$aggregate_actual_validationset$actual.pr);
    print(classacc);
    addToLog(logFile, paste(classacc));
    print(pred_actual_dframe[1:5,]);
    print(paste("##LL(\\) ", lloss[[j]], " -- ", "ACC(/)  ", classacc ,"-> ?", sep=""));
    addToLog(logFile, paste("##LL(\\) ", lloss[[j]], " -- ", "ACC(/)  ", classacc ,"-> ?", sep=""));

    ##--if validation file supplied--##
    if(!is.null(validation_file)){
    print("");
      vfile_csv <- read.csv(validation_file, na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
      if(dim(vfile_csv)[2] >= length(proj$input)){
        projv<-NULL; projv$dataset<-NULL;
        print(proj$input)
        projv$dataset <- vfile_csv;
        projv$dataset <- Center_Features(logFile, projv)$ds;
        projv <- Augment_Features(logFile, projv, TRUE);
        projv <- Augment_Samples(logFile, projv, TRUE);
        projv <- NormalizeAndNumerize(logFile, projv);
        projv <- Correlations_PCA(projv);
        projv <- Models_Init( projv);
      }else{
        projv<- NULL; projv <- project; projv$dataset<-NULL;
        projv$dataset <- project$testset;
        projv$dataset <- Center_Features(logFile, projv)$ds;
        projv <- Augment_Features(logFile, projv, TRUE);
        projv <- Augment_Samples(logFile, projv, TRUE);
        projv <- NormalizeAndNumerize(logFile, projv);
        projv <- Correlations_PCA(projv);
        projv <- Models_Init( projv);
      }      
      
      print(paste("Scoring ",project$models_names[j]," on validation file (different than validation set) (size: ",nrow(vfile_csv), " -- " ,(nrow(vfile_csv) / proj$nobs),"%)...",sep=""));
      if(grepl("H2O", project$models_names[j])){
        the_model <- h2o.loadModel(paste(H2O_save_dir, h2OModels[j], "", sep=""));
        newdata2 <- vfile_csv[, c(projv$input, projv$ident)];
        con_h2o<-file(paste(temp_dir, "h2o_validationset2.csv",sep=""), encoding="utf8");
        write.csv(newdata2, file=con_h2o, row.names=FALSE);
        newdata2 <- h2o.importFile(path = paste(temp_dir, "h2o_validationset.csv2",sep=""), destination_frame = "h2o_validationset2");
        project$pr <- h2o.predict(the_model, newdata2);
      }else{
        projv$pr <- predict(the_model, newdata=vfile_csv[, c(projv$input, projv$ident)], type="prob")[,2];
      }
      
      projv$aggregate_actual_validationset <- NULL;
      #print("Aggregating by ct_file_name using logical OR...");
      print("Aggregating by ct_file_name using mean of probabilities...");
      addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
      projv$aggregate_actual_validationset <- data.frame("ct_file_name"=vfile_csv$ct_file_name[projv$validate], as.vector(projv$dataset$cancer[projv$validate]));
      agg_temp <- aggregate(x = projv$aggregate_actual_validationset, by = list(projv$aggregate_actual_validationset$ct_file_name), FUN = mean);
      projv$aggregate_actual_validationset <- data.frame("ct_file_name"=agg_temp[1], "actual.pr"=agg_temp[3]);
      colnames(projv$aggregate_actual_validationset) <- c("ct_file_name", "actual.pr");
      agg_temp<-NULL;

      projv$aggregate_validationset <- NULL;
      if(grepl("H2O", projv$models_names[j])){
        #print("Aggregating by ct_file_name using logical OR...");
        print("Aggregating by ct_file_name using mean of probabilities...");
        addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
        projv$aggregate_validationset <- data.frame("ct_file_name"=projv$dataset$ct_file_name[projv$validate], as.vector(projv$pr$p1));
        agg_temp <- aggregate(x = projv$aggregate_validationset, by = list(projv$aggregate_validationset$ct_file_name), FUN = mean);
        projv$aggregate_validationset <- data.frame("ct_file_name"=agg_temp[1], "pred.pr"=agg_temp[3]);
        colnames(projv$aggregate_validationset) <- c("ct_file_name", "pred.pr");
        agg_temp<-NULL;
      }else{
        #print("Aggregating by ct_file_name using logical OR...");
        print("Aggregating by ct_file_name using mean of probabilities..."); #TODO: REDO THIS-"MEAN"
        addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
        projv$aggregate_validationset <- data.frame("ct_file_name"=projv$dataset$ct_file_name[projv$validate], projv$pr);
        agg_temp <- aggregate(x = projv$aggregate_validationset, by = list(projv$aggregate_validationset$ct_file_name), FUN = mean);
        projv$aggregate_validationset <- data.frame("ct_file_name"=agg_temp[1], agg_temp[3]);
        colnames(projv$aggregate_validationset) <- c("ct_file_name", "pred.pr");
        agg_temp<-NULL;
      }
      pred_actual_dframe <- data.frame(projv$aggregate_actual_validationset$actual.pr, projv$aggregate_validationset$pred.pr);
      #  print("confusion matrix...");
      confx<-confusionMatrix(projv$aggregate_validationset$pred.pr, projv$aggregate_actual_validationset$actual.pr);
      #  print(confx);
      #print("logloss matrix...");
      #llossM<-logLossMatrix(projv$aggregate_validationset$pred.pr, projv$aggregate_actual_validationset$actual.pr);
      print("LogLoss (lower better)...:");
      addToLog(logFile, "LogLoss (lower better)...:");
      lloss[[j]] <- logLossVector(projv$aggregate_validationset$pred.pr, projv$aggregate_actual_validationset$actual.pr);
      print(lloss[[j]]);
      addToLog(logFile, paste(lloss[[j]]));
      print("Classification Accuracy (higher better)...:");
      addToLog(logFile, "Classification Accuracy (higher better)...:");
      classacc<-classificationAccuracyForProbs(projv$aggregate_validationset$pred.pr, projv$aggregate_actual_validationset$actual.pr);
      print(classacc);
      addToLog(logFile, paste(classacc));
      print(pred_actual_dframe[1:5,]);
      print(paste("##LL(\\) ", lloss[[j]], " -- ", "ACC(/)  ", classacc ,"-> ?", sep=""));
      addToLog(logFile, paste("##LL(\\) ", lloss[[j]], " -- ", "ACC(/)  ", classacc ,"-> ?", sep=""));

      
    }      
  }
  
  lloss
}
#----------------- Validation Function END ----------------#

#function #----------------- Stacking Function -----------------##
# stacking <- function(project, H2O_save_dir,stack_num){
#   if(PERC != 50){
#     stop("For stacking percentage of test\validation set must be 50%...");
#   }
#   for(j in 1: length(project$models_names)){
#     project$pr<-NULL;
#     if(grepl("H2O", project$models_names[j])){
#       h2OModels <- list.files(path = H2O_save_dir, recursive = FALSE);
#     }else{
#       the_model <- readRDS(paste(temp_dir, project$models_names[j], ".rds", sep=""));
#     }
#     print("");
#     print(paste("Scoring ",project$models_names[j]," on validation set (size: ",(100-proj$PERC),"%)...",sep=""));
#     addToLog(logFile, paste("Scoring ",project$models_names[j]," on validation set (size: ",(100-proj$PERC),"%)...",sep=""));
#     if(grepl("H2O", project$models_names[j])){
#       the_model <- h2o.loadModel(paste(H2O_save_dir, h2OModels[j], "", sep=""));
#       newdata <- project$dataset[project$validate, c(project$input, project$ident)];
#       con_h2o<-file(paste(temp_dir, "h2o_validationset.csv",sep=""), encoding="utf8");
#       write.csv(newdata, file=con_h2o, row.names=FALSE);
#       newdata <- h2o.importFile(path = paste(temp_dir, "h2o_validationset.csv",sep=""), destination_frame = "h2o_validationset");
#       project$pr <- h2o.predict(the_model, newdata);
#     }else{
#       project$pr <- predict(the_model, newdata=(project$dataset[project$validate,c(project$input)]), type="prob")[,2];
#     }
#     
#     project$aggregate_actual_validationset <- NULL;
#     #print("Aggregating by ct_file_name using logical OR...");
#     print("Aggregating by ct_file_name using mean of probabilities...");
#     addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
#     project$aggregate_actual_validationset <- data.frame("ct_file_name"=project$dataset$ct_file_name[project$validate], as.vector(project$dataset$cancer[project$validate]));
#     agg_temp <- aggregate(x = project$aggregate_actual_validationset, by = list(project$aggregate_actual_validationset$ct_file_name), FUN = mean);
#     project$aggregate_actual_validationset <- data.frame("ct_file_name"=agg_temp[1], "actual.pr"=agg_temp[3]);
#     colnames(project$aggregate_actual_validationset) <- c("ct_file_name", "actual.pr");
#     agg_temp<-NULL;
#     
#     project$aggregate_validationset <- NULL;
#     if(grepl("H2O", project$models_names[j])){
#       #print("Aggregating by ct_file_name using logical OR...");
#       print("Aggregating by ct_file_name using mean of probabilities...");
#       addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
#       project$aggregate_validationset <- data.frame("ct_file_name"=project$dataset$ct_file_name[project$validate], as.vector(project$pr$p1));
#       agg_temp <- aggregate(x = project$aggregate_validationset, by = list(project$aggregate_validationset$ct_file_name), FUN = mean);
#       project$aggregate_validationset <- data.frame("ct_file_name"=agg_temp[1], "pred.pr"=agg_temp[3]);
#       colnames(project$aggregate_validationset) <- c("ct_file_name", "pred.pr");
#       agg_temp<-NULL;
#     }else{
#       #print("Aggregating by ct_file_name using logical OR...");
#       print("Aggregating by ct_file_name using mean of probabilities..."); #TODO: REDO THIS-"MEAN
#       addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
#       project$aggregate_validationset <- data.frame("ct_file_name"=project$dataset$ct_file_name[project$validate], project$pr);
#       agg_temp <- aggregate(x = project$aggregate_validationset, by = list(project$aggregate_validationset$ct_file_name), FUN = mean);
#       project$aggregate_validationset <- data.frame("ct_file_name"=agg_temp[1], agg_temp[3]);
#       colnames(project$aggregate_validationset) <- c("ct_file_name", "pred.pr");
#       agg_temp<-NULL;
#     }
#     print("");
#     print(paste("Saving predictions for stack ", stack_num, "...", sep=""));
#     addToLog(logFile, paste("Saving predictions for stack ", stack_num, "...", sep=""));
#     ##write.csv(cbind(subdata, as.data.frame(proj$pr)), file=paste("D:/Kaggle/Bowl2017/FINAL_FINAL_score_","_"+proj$models_names[j]+".csv",sep=""), row.names=FALSE);
#     export_csv_submission(proj$models_names[j], SESSION, paste("stack_", stack_num, sep=""), proj$aggregate_validationset$pred.pr, FALSE);
#   }
#   
# }
#----------------- Stacking Function END ----------------#


#-------temp dir inits--------
temp_dir<- settings_paths$R_code_paths$temp_directory;
temp_dir<- paste(temp_dir,"/Session",(SESSION),"/",sep="");
dir.create(temp_dir);
#-------temp dir inits--------
logFile <- paste(temp_dir, "LOG.txt", sep="");
file.create(logFile);
cat(logFile, " ", append = FALSE, sep="\n", file = logFile);

countNAS <- 0;
countNAS_test <- 0;

#=============================================TESTSET========================
start <- proc.time();
LOAD_TEST_SET <- function(project){
  project$dataset_KAGGLE_TEST_SET <- NULL;
  TOTAL_DATASET_LEN_TESTSET <- 0;
  #Load TEST SET BLOBS
  h<-NULL; dframe<-NULL; blobs_list<-NULL;
  
  results_dir<-NULL; results_dir <- "RESULTS";
  meta_csv_dir<-NULL; meta_csv_dir <- settings_paths$R_code_paths$meta_csv_final_directory;
  meta_csv_file<-NULL; meta_csv_file <- paste(meta_csv_dir, "/", "FINAL_FINAL.csv", sep="");
  meta_frame <-read.csv(meta_csv_file, na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
  res_path<-NULL; res_path<-settings_paths$test_paths$stage2_images_dir;
  dcm_list<-NULL; dcm_list <- list.dirs(path = res_path, recursive = FALSE);
  TOTAL_DATASET_LEN_TESTSET <- TOTAL_DATASET_LEN_TESTSET + length(dcm_list);
  for(h in 1:length(dcm_list)){
    if(!is.na(dcm_list[h])){
      dcm_dir <- dcm_list[h];
      project$results_path<-NULL; project$results_path <-paste(dcm_dir,"\\",results_dir, sep="");
      blobs_list<-NULL; blobs_list <- list.files(path = project$results_path, pattern = ".csv", recursive = FALSE);
      for(j in 1:1){
        if(length(blobs_list)!=0){
          dframe<-NULL;
          info<-NULL; info = file.info(paste(project$results_path, "\\",blobs_list[j], sep=""));
          info <- info$size;
          if(!is.na(info)){
            if(as.numeric(info) > 3){
              if(dir.exists(project$results_path)){
                dframe <- read.csv(paste(project$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                dframe <-dframe[which(dframe$Area.equivalent.circle.diameter >= 3 & dframe$Area.equivalent.circle.diameter <= 30),];
                dframe <-dframe[which(dframe$Elong. <= 0.6),];
                if(nrow(dframe)==0){
                  dframe <- read.csv(paste(project$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
                  dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
                }
                dframe$ct_file_name <- str_split(dcm_dir,"/")[[1]][7];
                dframe$total_slices <- meta_frame$num_slices[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                #dframe$total_blobs <- meta_frame$num_blobs[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                #dframe$mean_bits_stored <- meta_frame$ctmeta_bits_stored_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                #dframe$mean_slice_location <- meta_frame$ctmeta_slice_location_means[which(meta_frame$ct_file_name == dframe$ct_file_name)];
                
                project$dataset_KAGGLE_TEST_SET <- rbind(project$dataset_KAGGLE_TEST_SET, dframe);
                if(h%%50 ==0){print(""); print(paste("Loading & processing KAGGLE_TEST_SET ", h, ".../", nrow(dframe), "..."))
                  addToLog(logFile, paste("Loading & processing KAGGLE_TEST_SET ", h, ".../", nrow(dframe), "..."));
                };
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
              notavs[length(heading)] <- str_split(dcm_dir,"/")[[1]][7];
              dframe <- NULL;
              dframe <-data.frame(t(notavs));
              colnames(dframe)<-heading;
              project$dataset_KAGGLE_TEST_SET <- rbind(project$dataset_KAGGLE_TEST_SET, dframe);
            }
          }
        }
      }
    }
  }
  
  DATASET_LEN_TESTSET <- NULL;
  project$dataset_testset <- NULL;
  project$dataset_testset <- rbind(project$dataset_testset, project$dataset_KAGGLE_TEST_SET);
  DATASET_LEN_TESTSET<- nrow(project$dataset_testset);
  print(""); print(paste("TESTSET DF_LEN:", DATASET_LEN_TESTSET, " from ", TOTAL_DATASET_LEN_TESTSET));
  addToLog(logFile, paste("TESTSET DF_LEN:", DATASET_LEN_TESTSET, " from ", TOTAL_DATASET_LEN_TESTSET));
  project$dataset_testset
}

#loadAndProcessDataset(c(paste(SESSION),"KAGGLE","dummy_mandatory"));

print(""); print("Reading the latest train set from csv file of backup -- tmp -- dir...");
addToLog(logFile,"Reading the latest train set from csv file of backup -- tmp -- dir...");
bckp_dir<- settings_paths$R_code_paths$backup_directory;
bckp_dir <- "D:/Kaggle/Bowl2017/image_input/backup_csv - Copy/";
proj$dataset <- read.csv(paste(bckp_dir, "train_set.csv",sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#proj$testset <- read.csv(paste(bckp_dir, "test_set.csv",sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");

proj$testset <- LOAD_TEST_SET(proj);

print(""); print(paste("BLOBS DATASET LENGTH:", nrow(proj$dataset), " by ", length(proj$dataset)));
print(paste("BLOBS TESTSET LENGTH:", nrow(proj$testset), " by ", length(proj$testset)));
addToLog(logFile, paste("BLOBS DATASET LENGTH:", nrow(proj$dataset), " by ", length(proj$dataset)));
addToLog(logFile,paste("BLOBS TESTSET LENGTH:", nrow(proj$testset), " by ", length(proj$testset)));

end <- proc.time(); print(end-start);

# proj$dataset <- rbind(proj$dataset_cancer, proj$dataset_normal);
# proj$testset <- proj$dataset_testset;


# temp_files <- list.files(path = temp_dir, recursive = FALSE);
# #temp_dirs <- list.dirs(path = temp_dir, recursive = FALSE);
# temp_files <- setdiff(temp_files, temp_files[which(grepl(".zip", temp_files))]);
# temp_files <- setdiff(temp_files, temp_files[which(grepl("try", temp_files))]);
# #temp_files <- c(temp_files, temp_dirs);
# temp_session_dir <- paste("try",(SESSION-1),sep="");
# dir.create(temp_session_dir);
# my.file.rename <- function(from, to) {
#   todir <- dirname(to)
#   if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
#   file.rename(from = from,  to = to)
# }
# for(m in 1: length(temp_files)){
#   my.file.rename(from = paste(temp_dir, temp_files[m], sep=""), to = paste(temp_dir, temp_session_dir, "\\",temp_files[m], sep=""));
# }
#-------temp dir inits end------

#-------temp optional------
#TODO: use settings path
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# con<-file(paste(temp_dir, "KAGGLE_TEST_SET.csv", sep=""), encoding="utf8");
# write.csv(proj$dataset_KAGGLE_TEST_SET, file=con, row.names=FALSE);
#TODO: use settings path
#temp_dir<- settings_paths$R_code_paths$temp_directory;
#con<-file(paste(temp_dir, "LUNA_CANCER.csv", sep=""), encoding="utf8");
#write.csv(proj$dataset_LUNA_CANCER, file=con, row.names=FALSE);
#TODO: use settings path
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# con<-file(paste(temp_dir, "LUNA_EXTRA_CANCER.csv", sep=""), encoding="utf8");
# write.csv(proj$dataset_LUNA_EXTRA_CANCER, file=con, row.names=FALSE);
# #TODO: use settings path
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# con<-file(paste(temp_dir, "KAGGLE_CANCER.csv", sep=""), encoding="utf8");
# write.csv(proj$dataset_KAGGLE_CANCER, file=con, row.names=FALSE);
# #TODO: use settings path
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# con<-file(paste(temp_dir, "KAGGLE_NORMAL.csv", sep=""), encoding="utf8");
# write.csv(proj$dataset_KAGGLE_NORMAL, file=con, row.names=FALSE);
# #TODO: use settings path
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# con<-file(paste(temp_dir, "train_set.csv", sep=""), encoding="utf8");
# write.csv(proj$dataset, file=con, row.names=FALSE);
# #TODO: use settings path
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# con<-file(paste(temp_dir, "test_set.csv", sep=""), encoding="utf8");
# write.csv(proj$testset, file=con, row.names=FALSE);
#-------end temp optional------


#View(proj$dataset);
#View(proj$testset);

#proj$dataset <- read.csv(paste(settings_paths$R_code_paths$meta_csv_final_directory, "/FINAL_FINAL.csv",sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#proj$testset <- read.csv(paste(settings_paths$R_code_paths$meta_csv_final_directory, "/FINAL_FINAL.csv",sep=""),na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");

Proj_Init <- function(project) {
  set.seed(project$seed);
  
  project$PERC <- 100;
  
  project$nobs <- nrow(project$dataset) 
  project$sample <- project$train <- sample(nrow(project$dataset), project$PERC/100*project$nobs) 
  project$validate <- sample(setdiff(seq_len(nrow(project$dataset)), project$train))
  #project$test <- setdiff(setdiff(seq_len(nrow(project$dataset)), project$train), project$validate) 
  
  
  
  project$target  <- "cancer"
  project$ident   <- "ct_file_name"
  
  project$ignore<-c("X.1", "X","Y", "Frame", "Label");
  
  #project$ignore<-c(project$ignore, unique(setdiff(project$input, project$input2)));
  
  
  project$input <- colnames(project$dataset);
  project$input <- setdiff(project$input, project$ignore);
  project$input <- setdiff(project$input, project$ident);
  project$input <- setdiff(project$input, project$target);
  
  
  project$numeric <- NULL
  project$categoric <- NULL
  project$risk    <- NULL
  
  #class weighting
  project$target_class_labels <- c("0", "1");
  project$target_class_labels <- c(as.name(project$target_class_labels[1]), as.name(project$target_class_labels[2]));
  
  classwts = c(1.0, 2.0); names(classwts) =  project$target_class_labels;
  subs <- 1/classwts; swap = -1; swap = subs[2]; subs[2] = subs[1]; subs[1] = swap; 
  subspercs <- c((subs)[1] / sum (subs), (subs)[2] / sum (subs));
  project$weights <- classwts;
  project$subsamps <- subs;
  project$subspercs <- subspercs;
  project
}

addToLog(logFile, paste("Class weights (Note: not all models use them -- see below):", proj$weights));
addToLog(logFile, paste("Global seed:", proj$seed));


###################################################################### CENTER FEATURES #######################################################################
Center_Features <- function(logFile, project) {
  start <- proc.time(); 
  print(""); print("Centering X, Y coord. features around ct scan center ...")
  addToLog(logFile, "Centering X, Y coord. features around ct scan center ...");
  temp_exclude <- setdiff(project$input, c("X","Y"));
  exclude3 <- c(project$target, project$ident, temp_exclude);
  exclude3_test <- c(project$ident, temp_exclude);
  #Center per column
  project$dataset <- center_around_CT_CenterPerColumn(data.frame(project$dataset, stringsAsFactors=FALSE), exclude3);
  project$testset <- center_around_CT_CenterPerColumn(data.frame(project$testset, stringsAsFactors=FALSE), exclude3_test);
  end <- proc.time(); print(end-start);
  list(ds = project$dataset, ts = project$testset)
}

###################################################################### FEATURES AUGMENT FEATURES #######################################################################
Augment_Features <- function(logFile, project, quietly=FALSE) {
  start<- proc.time();
  print(""); print("Augmenting features with nodules regions...")
  addToLog(logFile, "Augmenting features with nodules regions...");
  new_feature <- "Region";
  project$dataset$Region <- rep(NA, nrow(project$dataset));
  project$dataset$Region <- addNoduleRegionMoreDetails(project$dataset$X, project$dataset$Y);
  project$testset$Region <- rep(NA, nrow(project$testset));
  project$testset$Region <- addNoduleRegionMoreDetails(project$testset$X, project$testset$Y);
  #update...  
  project$input<- unique(c(project$input, new_feature));
  
  if(!quietly){
    print(""); print(paste("BLOBS DATASET LENGTH IS NOW:", nrow(project$dataset), " by ", length(project$dataset)));
    print(paste("BLOBS TESTSET LENGTH IS NOW:", nrow(project$testset), " by ", length(project$testset)));
    addToLog(logFile, paste("BLOBS DATASET LENGTH IS NOW:", nrow(project$dataset), " by ", length(project$dataset)));
    addToLog(logFile,paste("BLOBS TESTSET LENGTH IS NOW:", nrow(project$testset), " by ", length(project$testset)));
    addToLog(logFile, paste(project$input));
  }
  #ratios.........
  #-number of blobs distribution over nnumber of slices (normalized) slice as a percentage
  #max minus min intensity.........
  
  #TODO META BLOBS STATS HALFSTACK ETC
  
  end <- proc.time(); print(end-start);
  project
}
###################################################################### SAMPLES RESTRICT\AUGMENT SAMPLES #######################################################################
Augment_Samples <- function(logFile, project, quietly=FALSE) {
  start <- proc.time();
  print(""); print("Oversampling with nodules located in important regions...");
  addToLog(logFile, "Oversampling with nodules located in important regions...");
  project$oversampling_factor <- 2;#Don't Zero for NONE (comment-out)
  addToLog(logFile, paste("Factor:",project$oversampling_factor));
  temp_todup <- NULL;
  temp_todup = project$dataset[which((as.numeric(project$dataset$Region)-1) > 1),];
  for(e in 1:project$oversampling_factor-1){
    project$dataset = rbind(project$dataset, temp_todup);
  }
  temp_todup <- NULL;
  #update...
  project$nobs <- nrow(project$dataset);
  project$sample <- project$train <- sample(nrow(project$dataset), project$PERC/100*project$nobs)
  project$validate <- sample(setdiff(seq_len(nrow(project$dataset)), project$train))
  if(!quietly){
    print(""); print(paste("BLOBS DATASET LENGTH IS NOW:", nrow(project$dataset), " by ", length(project$dataset)));
    print(paste("BLOBS TEST LENGTH DID NOT CHANGE:", nrow(project$testset), " by ", length(project$testset)));
    addToLog(logFile, paste("BLOBS DATASET LENGTH IS NOW:", nrow(project$dataset), " by ", length(project$dataset)));
    addToLog(logFile, paste("BLOBS TEST LENGTH DID NOT CHANGE:", nrow(project$testset), " by ", length(project$testset)));
    addToLog(logFile, paste(project$input));
  }  
  ####
  #print(""); print("Removing from dataset nodules outside of regions of interest...");
  #addToLog(logFile, "Removing from dataset nodules outside of regions of interest...");
  
  # Duplicate weighting for cases-to-be-weighed. One study priors:
  # Heart region: (70.5%) # Peripheral (29.5%) # Upper zone (19.6%)
  # Mid zone (30.3%) # Lower zone (19.8%) # Entire lungs (13.9%) # Others (16.4%)
  # print(""); print("Oversampling/undersampling with nodules located in specific regions according to priors...");
  # addToLog(logFile, "Oversampling/undersampling with nodules located in specific regions according to priors...");
  # prior_heart <- 70.5;
  # prior_peripheral <- 29.5;
  # prior_upperzone <- 19.6;
  # prior_midzone <- 30.3;
  # prior_lowerzone <- 19.8;
  # prior_entire <- 13.9;
  # prior_others <- 16.4;
  # ssss <- prior_heart + prior_peripheral + prior_upperzone + prior_midzone + prior_lowerzone + 0.5*(prior_entire + prior_others);
  # prior_heart <- prior_heart/ssss;
  # prior_peripheral <- prior_peripheral/ssss;
  # prior_upperzone <- prior_upperzone/ssss;
  # prior_midzone <- prior_midzone/ssss;
  # prior_lowerzone <- prior_lowerzone/ssss;
  # prior_eo <- (0.5*(prior_others +prior_entire))/ssss;
  # priors <- rep(0, length(project$dataset$Region)); #others
  # project$priors[which(project$dataset$Region==1000)] <- 100*prior_upperzone;#upperzone
  # project$priors[which(project$dataset$Region==2000)] <- 100*prior_peripheral;#peripheral
  # project$priors[which(project$dataset$Region==3000)] <- 100*prior_lowerzone;#lowerzone
  # project$priors[which(project$dataset$Region==5000)] <- 100*prior_heart;#heart
  # project$priors[which(project$dataset$Region==0)] <- 100*prior_eo;#entire/others
  # project$oversampling_factors <- project$priors;
  # ofac <- as.numeric(names(table(project$dataset$oversampling_factors))) / sum(as.numeric(names(table(project$dataset$oversampling_factors))))
  # for(q in 1: length(ofac)-1){
  #   ofac[q] <- ceil(ofac[q+1] / ofac[q]);
  # }
  # ofac[length(ofac)] <- 1;
  # addToLog(logFile, paste("Factors:", paste(names(table(project$priors)))));
  # 
  # temp_todup <- NULL;
  # temp_todup = project$dataset[which((as.numeric(project$dataset$Region)-1) > 1),];
  # for(e in 1:project$oversampling_factor-1){
  #   project$dataset = rbind(project$dataset, temp_todup);
  # }
  # temp_todup <- NULL;
  # #update...
  # project$nobs <- nrow(project$dataset)
  # project$sample <- project$train <- sample(nrow(project$dataset), project$PERC/100*project$nobs)
  # project$validate <- sample(setdiff(seq_len(nrow(project$dataset)), project$train))
  if(!quietly){
  # print(""); print(paste("BLOBS DATASET LENGTH IS NOW:", nrow(project$dataset), " by ", length(project$dataset)));
  # print(paste("BLOBS TEST LENGTH DID NOT CHANGE:", nrow(project$testset), " by ", length(project$testset)));
  # addToLog(logFile, paste("BLOBS DATASET LENGTH IS NOW:", nrow(project$dataset), " by ", length(project$dataset)));
  # addToLog(logFile, paste("BLOBS TEST LENGTH DID NOT CHANGE:", nrow(project$testset), " by ", length(project$testset)));
  # addToLog(logFile, paste(project$input));
  }
  
  
  end <- proc.time(); print(end-start);
  project
}
###################################################################### NORMALIZATION \ NUMERIZATION #######################################################################
library(caret, quietly = TRUE);
NormalizeAndNumerize <- function(logFile, project) {
  start <- proc.time(); 
  exclude <- c(project$target, project$ident, "X.1", "Frame", "Label","X","Y", "Region");
  exclude_test <- c(project$ident, "X.1", "Frame", "Label","X","Y", "Region");
  exclude2 <- c(project$ident, "Region");
  
  #AsNumeric per column
  project$dataset <- asnumericPerColumn(data.frame(project$dataset, stringsAsFactors=FALSE), exclude2);
  project$testset <- asnumericPerColumn(data.frame(project$testset, stringsAsFactors=FALSE), exclude2);
  addToLog(logFile, "asnumericPerColumn");
  addToLog(logFile, paste("...excluded features:", exclude2));
  
  #Normalize per column
  #project$dataset <- normalizePerColumn(data.frame(project$dataset, stringsAsFactors=FALSE), exclude);
  #project$testset <- normalizePerColumn(data.frame(project$testset, stringsAsFactors=FALSE), exclude_test);
  addToLog(logFile, "normalizePerColumn");
  addToLog(logFile, paste("...excluded features:", exclude));
  addToLog(logFile, paste("...excluded features(testset):", exclude_test));
  
  #NormalizeLog per column
  project$dataset <- normalizePerColumnLogPlusOne(data.frame(project$dataset, stringsAsFactors=FALSE), exclude);
  project$testset <- normalizePerColumnLogPlusOne(data.frame(project$testset, stringsAsFactors=FALSE), exclude_test);
  addToLog(logFile, "normalizePerColumnLogPlusOne");
  addToLog(logFile, paste("...excluded features:", exclude));
  addToLog(logFile, paste("...excluded features(testset):", exclude_test));
  
  #Inpute-Mean per column
  project$dataset <- imputeMean(project$dataset, exclude);
  project$testset <- imputeMean(project$testset, exclude_test);
  addToLog(logFile, "imputeMean");
  addToLog(logFile, paste("...excluded features:", exclude));
  addToLog(logFile, paste("...excluded features(testset):", exclude_test));
  
  #Impute Zero per column
  # project$dataset <- imputeZero(project$dataset, exclude);
  # project$testset <- imputeZero(project$testset, exclude_test);
  # addToLog(logFile, "imputeZero");
  # addToLog(logFile, paste("...excluded features:", exclude));
  # addToLog(logFile, paste("...excluded features(testset):", exclude_test));
  
  #Impute Remove per column
  #project$dataset <- imputeRemove(project$dataset, exclude);
  ##update...
  #project$nobs <- nrow(project$dataset)
  #project$sample <- project$train <- sample(nrow(project$dataset), project$PERC/100*project$nobs)
  #project$validate <- sample(setdiff(seq_len(nrow(project$dataset)), project$train))
  # addToLog(logFile, "imputeRemove");
  # addToLog(logFile, paste("...excluded features:", exclude));
  # addToLog(logFile, paste("...update:", paste(project$nobs)));
  
  #Deal with Near-Zero Variance Columns
  project$nearZV_dataset <- checkConditionalX(project$dataset, project$target);
  project$input<- setdiff(project$input, project$nearZV_dataset);
  addToLog(logFile, "nearZeroVarianceColumns");
  addToLog(logFile, paste("...update:", paste(project$input)));
  
  #view_final_dataset(project$dataset, project)
  #view_final_dataset(project$testset, project, TRUE)
  end <- proc.time(); print(end-start);
  project
}
######################################################################CORRELATIONS\PCA#######################################################################
Correlations_PCA <- function(project) {
  # datatrain<-project$dataset[project$sample,c(project$input, project$target)];
  # datatest<-project$testset[project$sample,c(project$input, project$target)];
  # print(""); print("Correlation of train-set");
  # project$cor <- cor(datatrain, use="pairwise", method="pearson");
  # project$ord <- order(project$cor[1,]);
  # project$cor <- project$cor[project$ord, project$ord];
  # print(project$cor);
  # 
  # print("Correlation of test-set");
  # project$cor <- cor(datatest, use="pairwise", method="pearson");
  # project$ord <- order(project$cor[1,]);
  # project$cor <- project$cor[project$ord, project$ord];
  # print(project$cor);
  # datatrain<-NULL;
  # datatest<-NULL;
  
  project$input_excluded <- c("Area.Peri.", "Maximum.inscriped.circle.diameter", 
                           "Area.Conv..Hull", "Peri.", "Long.Side.Length.MBR", 
                           "Fract..Dim..Goodness", "Orientation", "Thinnes.Rt.");
  
  project$input <- setdiff(project$input, project$input_excluded);
  project
}


######################################################################MODELS INIT#######################################################################
Models_Init <- function(project) {
  project$models_names<- NULL;
  project
}

######################################################################INIT#######################################################################
init <- function(prjct){
  
  prjct <- Proj_Init(prjct);
  prjct$dataset <- Center_Features(logFile, prjct)$ds;
  prjct$testset <- Center_Features(logFile, prjct)$ts;
  prjct <- Augment_Features(logFile, prjct);
  prjct <- Augment_Samples(logFile, prjct);
  prjct <- NormalizeAndNumerize(logFile, prjct);
  prjct <- Correlations_PCA(prjct);
  prjct <- Models_Init(prjct);
}
proj <- init(proj);

#proj backup
saveRDS(proj, paste(temp_dir, "proj_backup" , ".rds", sep=""));

######################################################################MODEL 1 -- RF::RF#######################################################################
#start <- proc.time(); 
# library(randomForest, quietly=TRUE)
# 
# set.seed(proj$seed)
# 
# ######Fit_intercept . True/False . Normalize . True/False k-neighbors . N_neighbors . p . 2,3 SVM . C . Gamma . 'Auto', RS' . class_weight . 'Balanced', None Logistic . Penally . L1 or 12 Regression . C Naive Bayes (all variations) NONE NONE Lasso . Alpha . 0.1, 1.0, 10 . Normalize . True/False Random Forest . N_estimators . 120, 300, 500, 800, 1200 . Max_depth . 5, 8, 15, 25, 30, None . Min_samples_split . 1, 2, 5, 10, 15,100 . Min_samples_leat . 1, 2, 5,10 . Max features . Log2, six!, None Xgbaost Eta 0.01,0.015, 0.025, 0.05, 0.1 Gamma Max_depth min_child weight 3, 5, 7, 9, 12, 15, 17, 25 1, 3, 5, 7 Subsample Colsample_bytree Lambda alpha 0.01-0.1,1.0 , RS' 0, 0.1, 0.5, 1.0 RS'
# ####grid <- expand.grid(size=c(5,10,15,20,25,30,35,40,45,50), k=c(3,5))
# ####RF: . N_estimators . 120, 300, 500, 800, 1200 . Max_depth . 5, 8, 15, 25, 30, None . Min_samples_split . 1, 2, 5, 10, 15,100 . Min_samples_leat . 1, 2, 5,10 . Max features . Log2, six!, None
# 
# print("");
# print("-----------------------------------------------------------------------------------------------------------");
# print(" 1) Modelling with Random Forests (ntrees=600 mtry=12 na.roughfix importance=true and replace=false)...");
#addToLog(logFile, " 1) Modelling with Random Forests (ntrees=600 mtry=12 na.roughfix importance=true and replace=false)...");
##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
# proj$rf <- NULL;
# proj$rf <- randomForest::randomForest(as.factor(cancer) ~ .,
#                                       data=proj$dataset[proj$sample,c(proj$input, proj$target)],
#                                       ntree=11,
#                                       mtry=12,
#                                       nodesize=5,
#                                       importance=TRUE, proximity=TRUE,
#                                       #                                       na.action=randomForest::na.roughfix,
#                                       na.action=na.exclude,
#                                       replace=FALSE,
#                       subsamps=c(proj$subspercs[1]*proj$samples, proj$subspercs[2]*proj$samples))
# 
# 
#addToLog(logFile, "params ntrees=11, mtry=12, nodesize=5, importance=T, proximity=T, na.exclude, replace=F");

# print(proj$rf)
# print("DONE!")
# print("");
# print(" 2) Modelling with Random Forests (ntrees=1000 mtry=12 na.roughfix importance=true and replace=false)...");
#addToLog(logFile, " 2) Modelling with Random Forests (ntrees=1000 mtry=12 na.roughfix importance=true and replace=false)...");
# proj$rf2 <- NULL;
# proj$rf2 <- randomForest::randomForest(as.factor(proj$dataset$cancer)[proj$sample] ~ .,
#                                        data=proj$dataset[proj$sample,c(proj$input, proj$target)],
#                                        ntree=1000,
#                                        mtry=12,
#                                        nodesize=5,
#                                        importance=TRUE, proximity=TRUE,
#                                         na.action=randomForest::na.roughfix,
#               #                         na.action=na.exclude,
#                                        replace=FALSE)
#addToLog(logFile, "params ntree=1000, mtry=12, nodesize=5, importance=T, proximity=T, na.roughfix, replace=F");
# 
# print(proj$rf2)
# addToLog(logFile, paste(proj$rf2));
# print("DONE!")
# print("");
# 
# 
# print("");
# print("ROC AUC RF1...");
# print(pROC::roc(proj$rf$y, as.numeric(proj$rf$predicted)))
# print(pROC::ci.auc(proj$rf$y, as.numeric(proj$rf$predicted)))
# rn <- NULL;
# rn <- round(randomForest::importance(proj$rf), 2)
# rn[order(rn[,3], decreasing=TRUE),]
# 
# print("");
# print("ROC AUC RF2...");
# print(pROC::roc(proj$rf2$y, as.numeric(proj$rf2$predicted)))
# print(pROC::ci.auc(proj$rf2$y, as.numeric(proj$rf2$predicted)))
# rn <- NULL;
# rn <- round(randomForest::importance(proj$rf2), 2)
# rn[order(rn[,3], decreasing=TRUE),]
# 
# 
# model_name_1 <- "Random_Forests_(rf)_1";
# model_name_2 <- "Random_Forests_(rf)_2";
# proj$models_names<-c(proj$models_names, model_name_1, model_name_2);
# saveRDS(proj$rf, paste(temp_dir, model_name_1 , ".rds", sep=""));
# saveRDS(proj$rf2, paste(temp_dir, model_name_2 , ".rds", sep=""));
end <- proc.time(); print(end-start);
######################################################################MODEL 2 -- Caret::kNN#######################################################################
start <- proc.time(); 
# library(caret, quietly=TRUE)
# 
# #TODO: automatic gird tuning first -- now is manual
# set.seed(proj$seed)
# print("");
# print("-----------------------------------------------------------------------------------------------------------");
# print("Modelling with kNN (caret) (tuneGrid=expand.grid(.k=1:30), 10cv-15 ICAComp parallel)...");
# addToLog(logFile. "Modelling with kNN (caret) (tuneGrid=expand.grid(.k=1:30), 10cv-15 ICAComp parallel)...");
# proj$caret_knn <- NULL;
##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
# proj$caret_knn <- train( cancer~., data=proj$dataset[proj$sample,c(proj$input, proj$target)], 
#                          method='knn', tuneGrid=expand.grid(.k=1:30),
#                          metric='logLoss', trControl=trainControl(
#                            method='repeatedcv', 
#                            number=10, 
#                            repeats=15,
#                            classProbs=TRUE,
#                            summaryFunction=twoClassSummary,
#                            allowParallel=TRUE)) #na.pass
# addToLog(logFile, "params: tuneGrid=expand.grid(.k=1:30),metric='logLoss', trControl=trainControl(method='repeatedcv',number=10,repeats=15,classProbs=TRUE,summaryFunction=twoClassSummary,allowParallel=TRUE)");
#classwt = proj$weights;
# 
# 
# print(proj$caret_knn);
# print("DONE!")
# print("");
# 
# 
# model_name_1<-"Caret_kNN(knn)";
# proj$models_names<-c(proj$models_names, model_name_1);
# saveRDS(proj$caret_knn, paste(temp_dir, model_name_1 , ".rds", sep=""));
# 

end <- proc.time(); print(end-start);

######################################################################MODEL 3 -- Caret::XGBOOST#######################################################################
# start <- proc.time();
# library(caret, quietly=TRUE)
# 
# set.seed(proj$seed)
# print("");
# print("-----------------------------------------------------------------------------------------------------------");
# print("Modelling with xgBoost (caret) (method = cv, number = 5, allowParallel)...");
#addToLog(logFile, "Modelling with xgBoost (caret) (method = cv, number = 5, allowParallel)...");
# #   Eta 0 001.0015, 0.025, 0.05, 0.1 #   Gamma o 0.05-0.1,0.3.0.5,0.7,0.9,1.0
# #   Max_depth o 3, 5, 7, 9, 12, 15, 17, 25 #   Min_child_weight o 1, 3, 5, 7
# #   Subsample o 0.6. 0.7, 0.8, 0.9, 1.0 #   Colsample_bytree o 0.6. 0.7, 0.8, 0.9, 1.0
# #   Lambda a 0.0101, 1.0 , RS' #   alpha 0 0, 0.1, 0.5, 1.0 RSâ
# # reg:logistic
# 
##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
# xgb_grid_1 = expand.grid(
#   nrounds = 1000,
#   eta = c(0.01, 0.015, 0.025, 0.05, 0.1), #c(0.01, 0.001, 0.0001),
#   max_depth = c( 3, 5, 7, 9, 12, 15, 17, 25),
#   gamma = c(0.05, 0.075, 0.1,0.3,0.5,0.7,0.9,1.0),
#   colsample_bytree = c(0.5, 0.75, 1.0),
#   min_child_weight = c(1.0,1.1,1.2,1.5,2.0),
#   subsample = c(0.5, 0.75, 1.0)
# )
# xgb_trcontrol_1 = trainControl(
#   method = "cv",
#   number = 5,
#   verboseIter = TRUE,
#   returnData = FALSE,
#   returnResamp = "all",
#   classProbs = TRUE,
#   summaryFunction = multiClassSummary,
#   allowParallel = TRUE
# )
# 
# dataf <- NULL; vars = NULL; x<-NULL; y<-NULL;
# ###dataf <- proj$dataset[proj$sample,c(proj$input, proj$target)];
# dataf <- datasets::iris;
# vars = colnames(dataf);
# ###vars = setdiff(vars,proj$target);
# vars = setdiff(vars,"Survived");
# vars = sapply(vars, as.name);
# ###dataf$cancer <- as.factor(dataf$cancer);
# dataf$Species <- as.factor(dataf$Species);
# ###dataf_target <- dataf$cancer;
# dataf_target <- dataf$Species;
# #dataf$cancer <- factor(dataf$cancer, levels=c(0,1), labels=proj$target_class_labels);
# #dataf$cancer %>% {as.numeric(.)-1};
# #dataf$Region <- as.numeric(dataf$Region);
# colnames(dataf) <- sapply(colnames(dataf), as.name);
# ###formula <- cancer~.
# formula <- Species~.
# dataf <- as.data.frame(dataf);
# row.names(dataf)<-NULL;
# dataf <- dataf[,as.character(vars)];
# dataf <- asnumericPerColumn(dataf,c("Species"));
# # control <- trainControl(method="cv", number=10)
# # metric <- "logloss"
# # fit.xgb <- train(formula, data=dtrain, method="xgbTree", metric=metric, trControl=control, nthread =4)
# # fit.xgbl <- train(formula, data=dtrain, method="xgbLinear", metric=metric, trControl=control, nthread =4)
# 
# proj$caret_xgboost <- NULL;
# proj$caret_xgboost <- train(
#                         x = dataf,
# #                        y = dataf$cancer,
#                         #formula,
#                         y = dataf_target,
#                         metric='logLoss',
#                         trControl = xgb_trcontrol_1,
#                         tuneGrid = xgb_grid_1,
#                         #PCAthresh=ICAcomp,
#                         method = "xgbTree",
#                         na.action = na.pass
# #                        classwt = proj$weights
#                       );

#addToLog(logFile, "params: nrounds = 1000,eta = c(0.01, 0.015, 0.025, 0.05, 0.1), #c(0.01, 0.001, 0.0001),max_depth = c( 3, 5, 7, 9, 12, 15, 17, 25),   gamma = c(0.05, 0.075, 0.1,0.3,0.5,0.7,0.9,1.0), colsample_bytree = c(0.5, 0.75, 1.0),min_child_weight = c(1.0,1.1,1.2,1.5,2.0),subsample = c(0.5, 0.75, 1.0),method = 'cv',number = 5,verboseIter = TRUE,returnData = FALSE,returnResamp = 'all',classProbs = TRUE,summaryFunction = multiClassSummary,   allowParallel = TRUE, metric='logLoss',trControl = xgb_trcontrol_1,tuneGrid = xgb_grid_1 ,method = 'xgbTree',na.action = na.pass");

# #
# 
# # xgb.params <- list(
# #   "objective"  = "binary:logistic"
# #   , "eval_metric" = "auc"
# #   , "eta" = 0.07
# #   , "subsample" = 0.9
# #   , "colsample_bytree" = 0.8
# #   , "max_depth" = 8
# # )
# #
# # mid <- Sys.time()
# # cat('\nTime elapsed so far:\n', (mid-start), '\n')
# #
# # ## Maybe need to use data.matrix instead of 'as.matrix'
# # x.train <- xgb.DMatrix(as.matrix(x.train), label=y.train)
# # fit.xgb.cv <- xgb.cv(params=xgb.params, data=x.train, nrounds=120,
# #                      folds=cv.list, prediction=TRUE, stratified=TRUE,
# #                      verbose=TRUE, showsd=FALSE, print.every.n=10)
# # plot(fit.xgb.cv$dt$test.auc.mean)
# # preds <- fit.xgb.cv$pred
# # print(length(preds))
# #
# # # Note: 'y.train' and 'preds' aren't the same length...
# # cv.auc <- auc(roc(predictor=preds, response=y.train))
# # cat('\nThe calculated AUC via CV\n')
# # print(cv.auc)
# 
# print(proj$caret_xgboost);
# print("DONE!")
# print("");
# 
# 
# 
# model_name_1<-"Caret_xgBoost(xgboost)";
# proj$models_names<-c(proj$models_names, model_name_1);
# saveRDS(proj$caret_xgboost, paste(temp_dir, model_name_1 , ".rds", sep=""));
# 
# end <- proc.time(); print(end-start);
# 
######################################################################MODEL 4 -- XGBOOST::XGBOOST#######################################################################
# start <- proc.time();
# library(xgboost, quietly = TRUE);
# library(parallel, quietly = TRUE);
# library(NMOF, quietly = TRUE);
# 
# 
# # loglossobj <- function(preds, dtrain) {
# #   # dtrain is the internal format of the training data
# #   # We extract the labels from the training data
# #   labels <- getinfo(dtrain, "cancer")
# #   # We compute the 1st and 2nd gradient, as grad and hess
# #   preds <- 1/(1 + exp(-preds))
# #   grad <- preds - labels
# #   hess <- preds * (1 - preds)
# #   # Return the result as a list
# #   return(list(grad = grad, hess = hess))
# # }
# 
# set.seed(proj$seed)
# print("");
# print("-----------------------------------------------------------------------------------------------------------");
# print("Modelling with xgBoost (xgboost) (...)...");
# addToLog(logFile, "Modelling with xgBoost (xgboost) (...)...");
# 
# 
# dataf <- NULL; vars = NULL; x<-NULL; y<-NULL;
# ###dataf <- proj$dataset[proj$sample,c(proj$input, proj$target)];
# dataf <- datasets::iris;
# vars = colnames(dataf);
# ###vars = setdiff(vars,proj$target);
# vars = setdiff(vars,"Species");
# vars = sapply(vars, as.name);
# ###dataf$cancer <- as.factor(dataf$cancer);
# dataf$Species <- as.factor(dataf$Species);
# ###dataf_target <- dataf$cancer;
# dataf_target <- dataf$Species;
# #dataf$cancer <- factor(dataf$cancer, levels=c(0,1), labels=proj$target_class_labels);
# #dataf$cancer %>% {as.numeric(.)-1};
# #dataf$Region <- as.numeric(dataf$Region);
# colnames(dataf) <- sapply(colnames(dataf), as.name);
# ###formula <- cancer~.
# formula <- Species~.
# dataf <- as.data.frame(dataf);
# row.names(dataf)<-NULL;
# dataf <- dataf[,as.character(vars)];
# dataf$Species<-sample(rep(0:1,each=75));
# 
# trainLabel <- dataf$Species;
# train <- dataf[,as.character(vars)]; ##todo: doublecheck -1
# 
# ##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
# # xgboost task parameters
# nrounds <- 400;
# folds <- 5;
# obj <- 'binary:logistic';
# eval <- 'logloss';
# 
# # Parameter grid to search
# params <- list(
#   eval_metric = eval,
#   objective = obj,
#   eta = c(0.01, 0.015, 0.025, 0.05, 0.1), #c(0.01, 0.001, 0.0001),
#   max_depth = c( 3, 5, 7, 9, 12, 15, 17, 25),
#   gamma = c(0.05, 0.075, 0.1,0.3,0.5,0.7,0.9,1.0), #opt?
#   max_delta_step = c(0,1),
#   colsample_bytree = c(0.5, 0.75, 1.0), #opt?
#   min_child_weight = c(1.0,1.1,1.2,1.5,2.0), #opt?
#   subsample = c(0.5, 0.75, 1.0),
#   scale_pos_weight = 1
# )
# 
# 
# # Table to track performance from each worker node
# res <- data.frame()
# 
# # Simple cross validated xgboost training function (returning minimum error for grid search)
# xgbCV <- function (params) {
#   fit <- xgb.cv(
#     data = data.matrix(train),
#     label = trainLabel,
#     param =params,
#     missing = NA,
#     nfold = folds,
#     prediction = FALSE,
#     early.stop.round = 50,
#     maximize = FALSE,
#     nrounds = nrounds
#   )
#   rounds <- nrow(fit)
#   metric = paste('test.',eval,'.mean',sep='')
#   idx <- which.min(fit[,fit[[metric]]])
#   val <- fit[idx,][[metric]]
#   res <<- rbind(res,c(idx,val,rounds))
#   colnames(res) <<- c('idx','val','rounds')
#   return(val)
# }
# 
# # Find minimal testing error in parallel
# cl <- makeCluster(round(detectCores()/2))
# clusterExport(cl, c("xgb.cv",'train','trainLabel','nrounds','res','eval','folds'))
# sol <- gridSearch(
#   fun = xgbCV,
#   levels = params,
#   method = 'snow',
#   cl = cl,
#   keepNames = TRUE,
#   asList = TRUE
# )
# 
# # Combine all model results
# comb=clusterEvalQ(cl,res)
# results <- ldply(comb,data.frame)
# stopCluster(cl)
# 
# # Train model given solution above
# params <- c(sol$minlevels,objective = obj, eval_metric = eval)
# xgbModel <- xgboost(
#   data = xgb.DMatrix(data.matrix(train),missing=NaN, label = trainLabel),
#   param = params,
#   nrounds = results[which.min(results[,2]),1]
# )
# 
# #print(params)
# print(results)
# 
# 
# addToLog(logFile, "eval_metric = eval, objective = binary:logistic, eta = c(0.01, 0.015, 0.025, 0.05, 0.1), c(0.01, 0.001, 0.0001), max_depth = c( 3, 5, 7, 9, 12, 15, 17, 25), gamma = c(0.05, 0.075,0.1,0.3,0.5,0.7,0.9,1.0), max_delta_step = c(0,1), colsample_bytree = c(0.5, 0.75, 1.0), min_child_weight = c(1.0,1.1,1.2,1.5,2.0), subsample = c(0.5, 0.75, 1.0),scale_pos_weight = 1,data = data.matrix(train), label = trainLabel, param =params, missing = NA, nfold = 5, prediction = FALSE, early.stop.round = 50, maximize = FALSE,nrounds = 400");
# 
# 
# print(proj$xgboost);
# print("DONE!")
# print("");
# 
# 
# model_name_1<-"XGBoost(xgboost)";
# proj$models_names<-c(proj$models_names, model_name_1);
# saveRDS(proj$xgboost, paste(temp_dir, model_name_1 , ".rds", sep=""));
# 
# 
# 
# 
# 
# 
# 
# 
# 
# end <- proc.time(); print(end-start);
######################################################################MODEL 5 -- H2O::RF#######################################################################
start <- proc.time();
library(h2o)
#   library(devtools)
#   install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package");
Sys.sleep(3); h2o.init(nthreads=-1);
#oN_trees . 120, 300, 500, 800, 1200 .
#oMax_depth . 5, 8, 15, 25, 30, None .
#oMin_samples_split . 1, 2, 5, 10, 15,100 .
#oMin_samples_leat . 1, 2, 5,10 .
#oMax features . Log2, six!, None
dataf <- NULL; x = NULL; y = NULL;
dataf <- proj$dataset[proj$sample,c(proj$input, proj$target)];
y = proj$target;
x = colnames(dataf);
x = setdiff(x,proj$target);

con_h2o<-file(paste(temp_dir, "h2o_trainset.csv",sep=""), encoding="utf8");
write.csv(dataf, file=con_h2o, row.names=FALSE);
train <- h2o.importFile(path = paste(temp_dir, "h2o_trainset.csv",sep=""), destination_frame = "h2o_trainset");

train$cancer <- as.factor(train$cancer);

seeds <- c(proj$seed);
print("");
print("-----------------------------------------------------------------------------------------------------------");
print("Modelling with H2O randomForests (nfolds = 10 -- default settings and not (max_depth = 15, ntrees = 500, sample_rate = 0.7...)...");
addToLog(logFile,"Modelling with H2O randomForests (nfolds = 10 -- default settings and not (max_depth = 15, ntrees = 500, sample_rate = 0.7...)..." );
# proj$caret_knn <- NULL;
# proj$H2O_def<-NULL;
# proj$H2O_def <- lapply(seeds, function(seed){
#   h2o.randomForest(x, y, train, nfolds = 10, seed = seed)
# });
proj$H2O_tuned<-NULL;
#--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
proj$H2O_tuned <- lapply(seeds, function(seed){
  h2o.randomForest(x, y, train, nfolds = 10, seed = seed,
                   max_depth = 15, ntrees = 500, sample_rate = 0.7,
                   mtries = 4, col_sample_rate_per_tree = 0.9,
                   stopping_metric = "logloss",
                   stopping_tolerance = 0,
                   stopping_rounds = 5,
                   score_tree_interval = 3,
                   class_sampling_factors = proj$weights
  )
});


addToLog(logFile, "x, y, train, nfolds = 10, seed = seed, max_depth = 15, ntrees = 500, sample_rate = 0.7,mtries = 4, col_sample_rate_per_tree = 0.9,stopping_metric = 'logloss',stopping_tolerance = 0,stopping_rounds = 5,score_tree_interval = 3,class_sampling_factors = proj$weights");

# proj$H2O_def_logloss<-NULL; proj$H2O_def_log_loss <- sapply(proj$H2O_def, h2o.logloss, xval = TRUE);
proj$H2O_tuned_logloss<-NULL; proj$H2O_tuned_log_loss <- sapply(proj$H2O_tuned, h2o.logloss, xval = TRUE);

# boxplot(c(proj$H2O_def, proj$H2O_tuned) ~ c(rep(1, 6),rep(2, 6)));

H2O_model_save_dir <- NULL; H2O_model_save_dir <- paste(temp_dir, "H2O_models/", sep="");
dir.create(H2O_model_save_dir);
# model_name_1<-"Random_Forests_(H2O_rf)_1";
model_name_2<-"Random_Forests_(H2O_rf)_2";
# proj$models_names<-c(proj$models_names, model_name_1);
proj$models_names<-c(proj$models_names, model_name_2);
# h2o.saveModel(object = proj$H2O_def[[1]], path = paste(H2O_model_save_dir, "" , "", sep=""), force = TRUE);
h2o.saveModel(object = proj$H2O_tuned[[1]], path = paste(H2O_model_save_dir, "" , "", sep=""), force = TRUE);

#saveRDS(proj$H2O_def[[1]], paste(temp_dir, model_name_1 , ".rds", sep=""));
#saveRDS(proj$H2O_tuned[[1]], paste(temp_dir, model_name_2 , ".rds", sep=""));

end <- proc.time(); print(end-start);
#####################################################################MODEL 6 -- H2O::GBM#######################################################################
start <- proc.time();
library(h2o)
#   library(devtools)
#   install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package");
Sys.sleep(3); h2o.init(nthreads=-1);
dataf <- NULL; x = NULL; y = NULL;
dataf <- proj$dataset[proj$sample,c(proj$input, proj$target)];
y = proj$target;
x = colnames(dataf);
x = setdiff(x,proj$target);

con_h2o<-file(paste(temp_dir, "h2o_trainset.csv",sep=""), encoding="utf8");
write.csv(dataf, file=con_h2o, row.names=FALSE);
h2o_trainset <- h2o.importFile(path = paste(temp_dir, "h2o_trainset.csv",sep=""), destination_frame = "h2o_trainset");

h2o_trainset$cancer <- as.factor(h2o_trainset$cancer);


print("");
print("-----------------------------------------------------------------------------------------------------------");
print("Modelling with H2O GBM (nfolds = 10 -- default settings and not (...)...");
addToLog(logFile,"Modelling with H2O GBM (nfolds = 5 -- default settings and not (...)..." );
proj$H2O_tuned_GBM<-NULL;
#--## *** PARAMS *** -- Don't forget to update addToLog call below##--##

proj$H2O_tuned_GBM <- h2o.gbm(
  x = x, 
  y = y, 
  training_frame = h2o_trainset, 
  ntrees = 200,
  max_depth = 5, 
  learn_rate = 0.1, 
  class_sampling_factors = proj$weights 
  );

addToLog(logFile, " ntrees = 200,   max_depth = 5,  learn_rate = 0.1, class_sampling_factors<-proj$weights");

######proj$H2O_tuned_gbm_logloss<-NULL; proj$H2O_tuned_gbm_log_loss <- sapply(proj$H2O_tuned_GBM, h2o.logloss, xval = TRUE);
# boxplot(c(proj$H2O_def, proj$H2O_tuned) ~ c(rep(1, 6),rep(2, 6)));

H2O_model_save_dir <- NULL; H2O_model_save_dir <- paste(temp_dir, "H2O_models/", sep="");
dir.create(H2O_model_save_dir);
model_name_1<-"GBM_(H2O_GBM)_1";
proj$models_names<-c(proj$models_names, model_name_1);
h2o.saveModel(object = proj$H2O_tuned_GBM, path = paste(H2O_model_save_dir, "" , "", sep=""), force = TRUE);



end <- proc.time(); print(end-start);
######################################################################MODEL 7 -- H2O::DEEPLEARNING#######################################################################
start <- proc.time(); 
#Sys.sleep(3); h2o.init(nthreads=-1);
##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
#addToLog(...)
end <- proc.time(); print(end-start);
######################################################################MODEL 8 -- MY::GG-CLUSTERING#######################################################################
start <- proc.time(); 
##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
#addToLog(...)
end <- proc.time(); print(end-start);
######################################################################MODEL 9 -- H2O::ANOMALY#######################################################################
start <- proc.time(); 
##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
#addToLog(...)
end <- proc.time(); print(end-start);
######################################################################MODEL 10 -- DL::MXNET#######################################################################
start <- proc.time(); 
##--## *** PARAMS *** -- Don't forget to update addToLog call below##--##
#addToLog(...)
end <- proc.time(); print(end-start);

########################################################VALIDATE#########################################################
start <- proc.time(); 
#---select older session---#
# SESSION <- 16;
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# temp_dir<- paste(temp_dir,"\\Session",(SESSION),"\\",sep="");
# H2O_model_save_dir <- paste(temp_dir, "H2O_models/", sep="");
#---select older session---#
H2O_model_save_dir <- H2O_model_save_dir;
SESSION <- SESSION;
##SESSION <- newSession();
h2o.init(nthreads=-1);
llo <- validation(proj, H2O_model_save_dir, validation_file = NULL);
llo <- -1; ##ADJUST THIS ONE##

end <- proc.time(); print(end-start);
########################################################PREDICT#########################################################
TEST_PREDICT <- function(H2O_model_save_dir, SESSION, project){
  start <- proc.time(); 
  for(j in 1: length(project$models_names)){
    project$pr<-NULL;
    if(grepl("H2O", project$models_names[j])){
      h2OModels <- list.files(path = H2O_model_save_dir, recursive = FALSE);
    }else{
      the_model <- readRDS(paste(temp_dir, project$models_names[j], ".rds", sep=""));
    }
    print("");
    print(paste("Scoring ",project$models_names[j]," on csv file...",sep=""));
    addToLog(logFile, paste("Scoring ",project$models_names[j]," on csv file...",sep=""));
    if(grepl("H2O", project$models_names[j])){
      the_model <- h2o.loadModel(paste(H2O_model_save_dir, h2OModels[j], "", sep=""));
      newdata <- project$testset[,c(project$input, project$ident)];
      con_h2o<-file(paste(temp_dir, "h2o_testset.csv",sep=""), encoding="utf8");
      write.csv(newdata, file=con_h2o, row.names=FALSE);
      newdata <- h2o.importFile(path = paste(temp_dir, "h2o_testset.csv",sep=""), destination_frame = "h2o_testset");
      project$pr <- h2o.predict(the_model, newdata);
    }else{
      project$pr <- predict(the_model, newdata=(project$testset[,c(project$input)]), type="prob")[,2];
    }
    
    ##bound the results, otherwise you might get infinity results
    ###apply(project$pr, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)); 
    
    project$aggregate_testset <- NULL;
    if(grepl("H2O", project$models_names[j])){
      #print("Aggregating by ct_file_name using logical OR...");
      print("Aggregating by ct_file_name using mean of probabilities...");
      addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
      project$aggregate_testset <- data.frame("ct_file_name"=project$testset$ct_file_name, as.vector(project$pr$p1));
      agg_temp <- aggregate(x = project$aggregate_testset, by = list(project$aggregate_testset$ct_file_name), FUN = mean);
      project$aggregate_testset <- data.frame("ct_file_name"=agg_temp[1], "pred.pr"=agg_temp[3]);
      colnames(project$aggregate_testset) <- c("ct_file_name", "pred.pr");
      agg_temp<-NULL;
    }else{
      #print("Aggregating by ct_file_name using logical OR...");
      print("Aggregating by ct_file_name using mean of probabilities..."); #TODO: REDO THIS-"MEAN"
      addToLog(logFile, "Aggregating by ct_file_name using mean of probabilities...");
      project$aggregate_testset <- data.frame("ct_file_name"=project$testset$ct_file_name, project$pr);
      agg_temp <- aggregate(x = project$aggregate_testset, by = list(project$aggregate_testset$ct_file_name), FUN = mean);
      project$aggregate_testset <- data.frame("ct_file_name"=agg_temp[1], agg_temp[3]);
      colnames(project$aggregate_testset) <- c("ct_file_name", "pred.pr");
      agg_temp<-NULL;
    }
    # this below is Kaggle's job:P!
    # print("logLoss matrix...");
    # lloss<-logLossMatrix(project$aggregate_testset$pred.pr, project$dataset$cancer);
    # print(lloss);
    # print("confusion matrix...");
    # confx<-confusionMatrix(project$aggregate_testset$pred.pr, project$dataset$cancer);
    # print(confx);
    # ...
    
    #deal with NAs in testset
    ##omitted <- attr(na.omit(project$testset[,c(project$input)]), "na.action")
    ##subdata <- subset(project$testset[,], select=c("cancer"));
    print("");
    print(paste("Saving predictions ", paste(project$models_names[j], " @", (SESSION)," ", llo," ...", sep=""), sep=""));
    addToLog(logFile, paste("Saving predictions ", paste(project$models_names[j], " @", (SESSION)," ", llo," ...", sep=""), sep=""));
    ##write.csv(cbind(subdata, as.data.frame(project$pr)), file=paste("D:/Kaggle/Bowl2017/FINAL_FINAL_score_","_"+project$models_names[j]+".csv",sep=""), row.names=FALSE);
    export_csv_submission(project$models_names[j], SESSION, llo[[j]], project$aggregate_testset$pred.pr, length(project$aggregate_testset$pred.pr), 506,FALSE);
  }
  
  shell.exec(temp_dir)
  
  end <- proc.time(); print(end-start);
}  

Sys.sleep(3);
###############################################DO TEST PREDICTION###############################
#---select older session---#
# SESSION <- 16;
# temp_dir<- settings_paths$R_code_paths$temp_directory;
# temp_dir<- paste(temp_dir,"\\Session",(SESSION),"\\",sep="");
# H2O_model_save_dir <- paste(temp_dir, "H2O_models/", sep="");
#---select older session---#
H2O_model_save_dir <- H2O_model_save_dir;
SESSION <- SESSION;
TEST_PREDICT(H2O_model_save_dir, SESSION, proj);



h2o.shutdown(prompt=FALSE);




#------------------------------------------END------------------------------------------------------------























# proj$numeric <- c("num_slices", "blob_Y_min", "blob_area_min", 
#                   "blob_feret_min", "blob_min_feret_min", "blob_short_side_MBR_min", 
#                   "blob_aspect_ratio_min", "blob_convexity_min", "blob_solidity_min", 
#                   "blob_orientation_min", "blob_fract_dim_min", "blob_Y_firstQ", 
#                   "blob_area_firstQ", "blob_feret_firstQ", "blob_firstQ_feret_firstQ", 
#                   "blob_short_side_MBR_firstQ", "blob_aspect_ratio_firstQ", "blob_convexity_firstQ", 
#                   "blob_solidity_firstQ", "blob_orientation_firstQ", "blob_fract_dim_firstQ", 
#                   "blob_Y_medians", "blob_area_medians", "blob_feret_medians", 
#                   "blob_medians_feret_medians", "blob_short_side_MBR_medians", 
#                   "blob_aspect_ratio_medians", "blob_convexity_medians", "blob_solidity_medians", 
#                   "blob_orientation_medians", "blob_fract_dim_medians", "blob_Y_means", 
#                   "blob_area_means", "blob_feret_means", "blob_means_feret_means", 
#                   "blob_short_side_MBR_means", "blob_aspect_ratio_means", "blob_convexity_means", 
#                   "blob_solidity_means", "blob_orientation_means", "blob_fract_dim_means", 
#                   "blob_Y_thirdQ", "blob_area_thirdQ", "blob_feret_thirdQ", "blob_thirdQ_feret_thirdQ", 
#                   "blob_short_side_MBR_thirdQ", "blob_aspect_ratio_thirdQ", "blob_convexity_thirdQ", 
#                   "blob_solidity_thirdQ", "blob_orientation_thirdQ", "blob_fract_dim_thirdQ", 
#                   "blob_Y_max", "blob_area_max", "blob_feret_max", "blob_max_feret_max", 
#                   "blob_short_side_MBR_max", "blob_aspect_ratio_max", "blob_convexity_max", 
#                   "blob_solidity_max", "blob_orientation_max", "blob_fract_dim_max", 
#                   "blob_slices_counts_medians", "blob_slices_counts_means", "blob_calculated_volumes_firstQ", 
#                   "blob_calculated_volumes_medians", "half_stack_lobes_area_min", 
#                   "half_stack_lobes_mean_min", "half_stack_lobes_mean_firstQ", 
#                   "half_stack_lobes_min_firstQ", "half_stack_lobes_min_medians", 
#                   "half_stack_lobes_max_medians", "half_stack_lobes_max_means", 
#                   "half_stack_lobes_area_thirdQ", "half_stack_lobes_area_max", 
#                   "half_stack_lobes_mean_max", "statistics_mean_min", "statistics_stddev_patient_min", 
#                   "statistics_voxels_min", "statistics_volume_mm_min", "statistics_mode_firstQ", 
#                   "statistics_firstQ_firstQ", "statistics_area_medians", "statistics_mean_medians", 
#                   "statistics_max_medians", "statistics_voxels_medians", "statistics_stddev_patient_means", 
#                   "statistics_mode_means", "statistics_volume_mm_means", "statistics_area_thirdQ", 
#                   "statistics_thirdQ_thirdQ", "statistics_max_thirdQ", "statistics_mean_max", 
#                   "statistics_stddev_patient_max", "statistics_volume_mm_max", 
#                   "results_area_min", "results_area_firstQ", "results_mean_firstQ", 
#                   "results_mean_medians", "results_min_medians", "results_min_means", 
#                   "results_max_means", "results_max_thirdQ", "results_area_max", 
#                   "ctmeta_instance_number_min", "ctmeta_slice_location_min", "ctmeta_samples_per_pixel_min", 
#                   "ctmeta_rows_min", "ctmeta_window_center_min", "ctmeta_window_width_min", 
#                   "ctmeta_pixel_spacing_col_min", "ctmeta_instance_number_firstQ", 
#                   "ctmeta_position_firstQ", "ctmeta_samples_per_pixel_firstQ", 
#                   "ctmeta_bits_stored_firstQ", "ctmeta_window_center_firstQ", "ctmeta_pixel_spacing_row_firstQ", 
#                   "ctmeta_pixel_spacing_col_firstQ", "ctmeta_image_medians", "ctmeta_position_medians", 
#                   "ctmeta_bits_allocated_medians", "ctmeta_bits_stored_medians", 
#                   "ctmeta_rescale_slope_medians", "ctmeta_pixel_spacing_row_medians", 
#                   "ctmeta_image_orientation_patient_means", "ctmeta_image_means", 
#                   "ctmeta_columns_means", "ctmeta_bits_allocated_means", "ctmeta_rescale_intercept_means", 
#                   "ctmeta_rescale_slope_means", "ctmeta_slice_location_thirdQ", 
#                   "ctmeta_image_orientation_patient_thirdQ", "ctmeta_rows_thirdQ", 
#                   "ctmeta_columns_thirdQ", "ctmeta_window_width_thirdQ", "ctmeta_rescale_intercept_thirdQ", 
#                   "ctmeta_instance_number_max", "ctmeta_slice_location_max", "ctmeta_samples_per_pixel_max", 
#                   "ctmeta_rows_max", "ctmeta_window_center_max", "ctmeta_window_width_max", 
#                   "ctmeta_pixel_spacing_col_max")
# proj$ignore  <- c("blob_long_side_MBR_min", "blob_short_side_MBR_min", "blob_aspect_ratio_min", "blob_elongation_min", 
#                   "blob_long_side_MBR_firstQ", "blob_short_side_MBR_firstQ", "blob_aspect_ratio_firstQ", "blob_elongation_firstQ", 
#                   "blob_long_side_MBR_medians", "blob_short_side_MBR_medians", "blob_aspect_ratio_medians", "blob_elongation_medians", 
#                   "blob_area_means", "blob_area_conv_hull_means", "blob_peri_means", "blob_peri_conv_hull_means", "blob_feret_means", 
#                   "blob_means_feret_means", "blob_max_inscribed_diameter_means", "blob_area_equivalent_diameter_means", 
#                   "blob_long_side_MBR_means", "blob_short_side_MBR_means", "blob_aspect_ratio_means", "blob_area_peri_means", 
#                   "blob_circularity_means", "blob_elongation_means", "blob_convexity_means", "blob_solidity_means", 
#                   "blob_num_holes_means", "blob_thinnes_rt_means", "blob_contour_temp_means", "blob_orientation_means", 
#                   "blob_fract_dim_means", "blob_fract_dim_goodness_means", "blob_long_side_MBR_thirdQ", "blob_short_side_MBR_thirdQ", 
#                   "blob_aspect_ratio_thirdQ", "blob_elongation_thirdQ", "blob_long_side_MBR_max", "blob_short_side_MBR_max", 
#                   "blob_aspect_ratio_max", "blob_elongation_max", "blob_slices_counts_min", "statistics_area_min", "statistics_mean_min", 
#                   "statistics_stddev_patient_min", "statistics_mode_min", "statistics_min_min", "statistics_max_min", 
#                   "statistics_voxels_min", "statistics_volume_mm_min", "statistics_area_firstQ", "statistics_mean_firstQ", 
#                   "statistics_stddev_patient_firstQ", "statistics_mode_firstQ", "statistics_firstQ_firstQ", "statistics_max_firstQ", 
#                   "statistics_voxels_firstQ", "statistics_volume_mm_firstQ", "statistics_area_medians", "statistics_mean_medians", 
#                   "statistics_stddev_patient_medians", "statistics_mode_medians", "statistics_medians_medians", "statistics_max_medians", 
#                   "statistics_voxels_medians", "statistics_volume_mm_medians", "statistics_area_means", "statistics_mean_means", 
#                   "statistics_stddev_patient_means", "statistics_mode_means", "statistics_means_means", "statistics_max_means", 
#                   "statistics_voxels_means", "statistics_volume_mm_means", "statistics_area_thirdQ", "statistics_mean_thirdQ", 
#                   "statistics_stddev_patient_thirdQ", "statistics_mode_thirdQ", "statistics_thirdQ_thirdQ", "statistics_max_thirdQ", 
#                   "statistics_voxels_thirdQ", "statistics_volume_mm_thirdQ", "statistics_area_max", "statistics_mean_max", 
#                   "statistics_stddev_patient_max", "statistics_mode_max", "statistics_max_max", "statistics_voxels_max", 
#                   "statistics_volume_mm_max", "results_mean_min", "results_min_min", "results_max_min", "results_mean_firstQ", 
#                   "results_min_firstQ", "results_max_firstQ", "results_mean_medians", "results_min_medians", "results_max_medians", 
#                   "results_area_means", "results_mean_means", "results_min_means", "results_max_means", "results_mean_thirdQ", 
#                   "results_min_thirdQ", "results_max_thirdQ", "results_mean_max", "results_min_max", "results_max_max", 
#                   "ctmeta_instance_number_min", "ctmeta_samples_per_pixel_min", "ctmeta_rows_min", "ctmeta_columns_min", 
#                   "ctmeta_bits_allocated_min", "ctmeta_rescale_slope_min", "ctmeta_image_orientation_patient_firstQ", 
#                   "ctmeta_image_firstQ", "ctmeta_position_firstQ", "ctmeta_samples_per_pixel_firstQ", "ctmeta_rows_firstQ", 
#                   "ctmeta_columns_firstQ", "ctmeta_bits_allocated_firstQ", "ctmeta_rescale_slope_firstQ", 
#                   "ctmeta_pixel_spacing_row_firstQ", "ctmeta_pixel_spacing_col_firstQ", "ctmeta_image_orientation_patient_medians", 
#                   "ctmeta_image_medians", "ctmeta_position_medians", "ctmeta_samples_per_pixel_medians", "ctmeta_rows_medians", 
#                   "ctmeta_columns_medians", "ctmeta_bits_allocated_medians", "ctmeta_rescale_slope_medians", 
#                   "ctmeta_pixel_spacing_row_medians", "ctmeta_pixel_spacing_col_medians", "ctmeta_image_orientation_patient_means", 
#                   "ctmeta_image_means", "ctmeta_position_means", "ctmeta_samples_per_pixel_means", "ctmeta_rows_means", 
#                   "ctmeta_columns_means", "ctmeta_bits_allocated_means", "ctmeta_rescale_slope_means", "ctmeta_pixel_spacing_row_means", 
#                   "ctmeta_pixel_spacing_col_means", "ctmeta_image_orientation_patient_thirdQ", "ctmeta_image_thirdQ", 
#                   "ctmeta_position_thirdQ", "ctmeta_samples_per_pixel_thirdQ", "ctmeta_rows_thirdQ", "ctmeta_columns_thirdQ", 
#                   "ctmeta_bits_allocated_thirdQ", "ctmeta_rescale_slope_thirdQ", "ctmeta_pixel_spacing_row_thirdQ", 
#                   "ctmeta_pixel_spacing_col_thirdQ", "ctmeta_image_orientation_patient_max", "ctmeta_image_max", "ctmeta_position_max", 
#                   "ctmeta_samples_per_pixel_max", "ctmeta_rows_max", "ctmeta_columns_max", "ctmeta_bits_allocated_max", 
#                   "ctmeta_rescale_slope_max", "ctmeta_pixel_spacing_row_max", "ctmeta_pixel_spacing_col_max")

# proj$input <- c("ct_file_name","num_slices", "blob_Y_min", "blob_area_min", 
#                 "blob_feret_min", "blob_min_feret_min", "blob_convexity_min", 
#                 "blob_solidity_min", "blob_orientation_min", "blob_fract_dim_min", 
#                 "blob_Y_firstQ", "blob_area_firstQ", "blob_feret_firstQ", "blob_firstQ_feret_firstQ", 
#                 "blob_convexity_firstQ", "blob_solidity_firstQ", "blob_orientation_firstQ", 
#                 "blob_fract_dim_firstQ", "blob_Y_medians", "blob_area_medians", 
#                 "blob_feret_medians", "blob_medians_feret_medians", "blob_convexity_medians", 
#                 "blob_solidity_medians", "blob_orientation_medians", "blob_fract_dim_medians", 
#                 "blob_fract_dim_means", "blob_Y_thirdQ", "blob_area_thirdQ", 
#                 "blob_feret_thirdQ", "blob_thirdQ_feret_thirdQ", "blob_convexity_thirdQ", 
#                 "blob_solidity_thirdQ", "blob_orientation_thirdQ", "blob_fract_dim_thirdQ", 
#                 "blob_Y_max", "blob_area_max", "blob_feret_max", "blob_max_feret_max", 
#                 "blob_convexity_max", "blob_solidity_max", "blob_orientation_max", 
#                 "blob_fract_dim_max", "blob_slices_counts_medians", "blob_slices_counts_means", 
#                 "blob_calculated_volumes_firstQ", "blob_calculated_volumes_medians", 
#                 "half_stack_lobes_area_min", "half_stack_lobes_area_thirdQ", 
#                 "half_stack_lobes_area_max", "statistics_mean_min", "statistics_stddev_patient_min", 
#                 "statistics_voxels_min", "statistics_volume_mm_means", "statistics_area_thirdQ", 
#                 "statistics_max_thirdQ", "statistics_mean_max", "statistics_stddev_patient_max", 
#                 "statistics_volume_mm_max", "results_area_min", "results_area_firstQ", 
#                 "results_area_max", "ctmeta_instance_number_min", "ctmeta_slice_location_min", 
#                 "ctmeta_samples_per_pixel_min", "ctmeta_rows_min", "ctmeta_window_center_min", 
#                 "ctmeta_window_width_min", "ctmeta_pixel_spacing_col_min", "ctmeta_instance_number_firstQ", 
#                 "ctmeta_samples_per_pixel_firstQ", "ctmeta_bits_stored_firstQ", 
#                 "ctmeta_window_center_firstQ", "ctmeta_bits_allocated_medians", 
#                 "ctmeta_bits_stored_medians", "ctmeta_rescale_slope_medians", 
#                 "ctmeta_columns_means", "ctmeta_bits_allocated_means", "ctmeta_rescale_intercept_means", 
#                 "ctmeta_rescale_slope_means", "ctmeta_slice_location_thirdQ", 
#                 "ctmeta_rows_thirdQ", "ctmeta_columns_thirdQ", "ctmeta_window_width_thirdQ", 
#                 "ctmeta_rescale_intercept_thirdQ", "ctmeta_instance_number_max", 
#                 "ctmeta_slice_location_max", "ctmeta_samples_per_pixel_max", 
#                 "ctmeta_rows_max", "ctmeta_window_center_max", "ctmeta_window_width_max");
# 
# proj$input2 <- c("ct_file_name", "num_slices", "num_blobs", "blob_label_min",
#                  "blob_X_min", "blob_Y_min", "blob_area_min", "blob_area_conv_hull_min",
#                  "blob_peri_min", "blob_peri_conv_hull_min", "blob_feret_min", "blob_min_feret_min",
#                  "blob_max_inscribed_diameter_min", "blob_area_equivalent_diameter_min", "blob_area_peri_min", "blob_circularity_min",
#                  "blob_convexity_min", "blob_solidity_min", "blob_num_holes_min", "blob_thinnes_rt_min",
#                  "blob_contour_temp_min", "blob_orientation_min", "blob_fract_dim_min", "blob_fract_dim_goodness_min",
#                  "blob_label_firstQ", "blob_X_firstQ", "blob_Y_firstQ", "blob_area_firstQ",
#                  "blob_area_conv_hull_firstQ", "blob_peri_firstQ", "blob_peri_conv_hull_firstQ", "blob_feret_firstQ",
#                  "blob_firstQ_feret_firstQ", "blob_max_inscribed_diameter_firstQ", "blob_area_equivalent_diameter_firstQ", "blob_area_peri_firstQ",
#                  "blob_circularity_firstQ", "blob_convexity_firstQ", "blob_solidity_firstQ", "blob_num_holes_firstQ",
#                  "blob_thinnes_rt_firstQ", "blob_contour_temp_firstQ", "blob_orientation_firstQ", "blob_fract_dim_firstQ",
#                  "blob_fract_dim_goodness_firstQ", "blob_label_medians", "blob_X_medians", "blob_Y_medians",
#                  "blob_area_medians", "blob_area_conv_hull_medians", "blob_peri_medians", "blob_peri_conv_hull_medians",
#                  "blob_feret_medians", "blob_medians_feret_medians", "blob_max_inscribed_diameter_medians", "blob_area_equivalent_diameter_medians",
#                  "blob_area_peri_medians", "blob_circularity_medians", "blob_convexity_medians", "blob_solidity_medians",
#                  "blob_num_holes_medians", "blob_thinnes_rt_medians", "blob_contour_temp_medians", "blob_orientation_medians",
#                  "blob_fract_dim_medians", "blob_fract_dim_goodness_medians", "blob_label_means", "blob_X_means",
#                  "blob_Y_means", "blob_label_thirdQ", "blob_X_thirdQ", "blob_Y_thirdQ",
#                  "blob_area_thirdQ", "blob_area_conv_hull_thirdQ", "blob_peri_thirdQ", "blob_peri_conv_hull_thirdQ",
#                  "blob_feret_thirdQ", "blob_thirdQ_feret_thirdQ", "blob_max_inscribed_diameter_thirdQ", "blob_area_equivalent_diameter_thirdQ",
#                  "blob_area_peri_thirdQ", "blob_circularity_thirdQ", "blob_convexity_thirdQ", "blob_solidity_thirdQ",
#                  "blob_num_holes_thirdQ", "blob_thinnes_rt_thirdQ", "blob_contour_temp_thirdQ", "blob_orientation_thirdQ",
#                  "blob_fract_dim_thirdQ", "blob_fract_dim_goodness_thirdQ", "blob_label_max", "blob_X_max",
#                  "blob_Y_max", "blob_area_max", "blob_area_conv_hull_max", "blob_peri_max",
#                  "blob_peri_conv_hull_max", "blob_feret_max", "blob_max_feret_max", "blob_max_inscribed_diameter_max",
#                  "blob_area_equivalent_diameter_max", "blob_area_peri_max", "blob_circularity_max", "blob_convexity_max",
#                  "blob_solidity_max", "blob_num_holes_max", "blob_thinnes_rt_max", "blob_contour_temp_max",
#                  "blob_orientation_max", "blob_fract_dim_max", "blob_fract_dim_goodness_max", "blob_slices_counts_firstQ",
#                  "blob_slices_counts_medians", "blob_slices_counts_means", "blob_slices_counts_thirdQ", "blob_slices_counts_max",
#                  "blob_calculated_volumes_min", "blob_calculated_volumes_firstQ", "blob_calculated_volumes_medians", "blob_calculated_volumes_means",
#                  "blob_calculated_volumes_thirdQ", "blob_calculated_volumes_max", "results_area_min", "results_area_firstQ",
#                  "results_area_medians", "results_area_thirdQ", "results_area_max", "ctmeta_slice_location_min",
#                  "ctmeta_image_orientation_patient_min", "ctmeta_image_min", "ctmeta_position_min", "ctmeta_bits_stored_min",
#                  "ctmeta_window_center_min", "ctmeta_window_width_min", "ctmeta_rescale_intercept_min", "ctmeta_pixel_spacing_row_min",
#                  "ctmeta_pixel_spacing_col_min", "ctmeta_instance_number_firstQ", "ctmeta_slice_location_firstQ", "ctmeta_bits_stored_firstQ",
#                  "ctmeta_window_center_firstQ", "ctmeta_window_width_firstQ", "ctmeta_rescale_intercept_firstQ", "ctmeta_instance_number_medians",
#                  "ctmeta_slice_location_medians", "ctmeta_bits_stored_medians", "ctmeta_window_center_medians", "ctmeta_window_width_medians",
#                  "ctmeta_rescale_intercept_medians", "ctmeta_instance_number_means", "ctmeta_slice_location_means", "ctmeta_bits_stored_means",
#                  "ctmeta_window_center_means", "ctmeta_window_width_means", "ctmeta_rescale_intercept_means", "ctmeta_instance_number_thirdQ",
#                  "ctmeta_slice_location_thirdQ", "ctmeta_bits_stored_thirdQ", "ctmeta_window_center_thirdQ", "ctmeta_window_width_thirdQ",
#                  "ctmeta_rescale_intercept_thirdQ", "ctmeta_instance_number_max", "ctmeta_slice_location_max", "ctmeta_bits_stored_max",
#                  "ctmeta_window_center_max", "ctmeta_window_width_max", "ctmeta_rescale_intercept_max")
# 
# proj$input <- proj$input2;


#=============================================C========================
# proj$blobs_features <-NULL;
# proj$dataset_LUNA_CANCER <- NULL;
# proj$dataset_LUNA_EXTRA_CANCER <- NULL;
# proj$dataset_KAGGLE_CANCER <- NULL;
# 
# TOTAL_DATASET_LEN_CANCER <- 0;
# #Load CANCER Blobs
# h<-NULL; dframe<-NULL; blobs_list<-NULL;
# 

# results_dir<-NULL; results_dir <- "RESULTS_ANNOTATIONS";
# #TODO: use settings_paths
# proj$results_path<-NULL; proj$results_path <-paste("D:\\LUNA\\input_images\\IMAGE_OUTPUTS\\", results_dir, sep="");
# blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
# TOTAL_DATASET_LEN_CANCER <- TOTAL_DATASET_LEN_CANCER + length(blobs_list);
# for(j in (1:length(blobs_list))){#length(blobs_list)){
#   dframe<-NULL;
#   dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#   dframe <- dframe[which(dframe$Area.equivalent.circle.diameter == max(dframe$Area.equivalent.circle.diameter)),];
#   if(nrow(dframe)==0){
#     dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#     dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
#   }
#   #Adjust pixels dimensions due to ImageJ BioFormatExporter:
#   col_adj<-colnames(dframe)[c(4:15,17)];
#   dframe[col_adj] = dframe[col_adj]*10^4;
#   dframe$ct_file_name <- "dummy";
#   dframe$cancer <- 1;
#   proj$dataset_LUNA_CANCER <- rbind(proj$dataset_LUNA_CANCER, dframe);
#   if(j%%PRINT_BATCH_SIZE ==0){print(paste("Loading & processing LUNA_CANCER_SET ",j, ".../", nrow(dframe), "..."));}
# }
# ##EXTRA2
# # results_dir<-NULL; results_dir <- "RESULTS_EXTRA2_ANNOTATIONS";
# # #TODO: use settings_paths
# # proj$results_path<-NULL; proj$results_path <-paste("D:\\LUNA\\input_images\\IMAGE_OUTPUTS\\", results_dir, sep="");
# # blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
# # TOTAL_DATASET_LEN_CANCER <- TOTAL_DATASET_LEN_CANCER + length(blobs_list);
# # for(j in (1:length(blobs_list))){#length()){
# #   dframe<-NULL;
# #   dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
# #   dframe <- dframe[which(dframe$Area.equivalent.circle.diameter == max(dframe$Area.equivalent.circle.diameter)),];
# #   if(nrow(dframe)==0){
# #     dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
# #     dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
# #   }
# #   #Adjust pixels dimensions due to ImageJ BioFormatExporter:
# #   col_adj<-colnames(dframe)[c(4:15,17)];
# #   dframe[col_adj] = dframe[col_adj]*10^4;
# #   dframe$ct_file_name <- "dummy";
# #   dframe$cancer <- 1;
# #   proj$dataset_LUNA_EXTRA_CANCER <- rbind(proj$dataset_LUNA_EXTRA_CANCER, dframe);
# #   if(j%%PRINT_BATCH_SIZE ==0){print(paste("Loading & processing LUNA_EXTRA2_CANCER_SET ", j, ".../", nrow(dframe), "..."));}
# # }
# 
# 
#KAGGLESET
# results_dir<-NULL; results_dir <- "RESULTS";
# #TODO: use settings_paths
# res_path<-NULL; res_path<-"D:\\Kaggle\\Bowl2017\\image_input\\stage2_images\\";
# dcm_list<-NULL; dcm_list <- list.dirs(path = res_path, recursive = FALSE);
# dcm_list <- dcm_list[which("CANCER"==as.vector(str_match(dcm_list, "CANCER")))]
# #TODO: use settings_paths
# TOTAL_DATASET_LEN_CANCER <- TOTAL_DATASET_LEN_CANCER + length(dcm_list);
# for(h in (1:length(dcm_list))){#length(dcm_list)){
#   if(!is.na(dcm_list[h])){
#     dcm_dir <- dcm_list[h];
#     proj$results_path<-NULL; proj$results_path <-paste(dcm_dir,"\\",results_dir, sep="");
#     blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
#     for(j in 1:1){
#       if(length(blobs_list)!=0){
#         dframe<-NULL;
#         info<-NULL; info = file.info(paste(proj$results_path, "\\",blobs_list[j], sep=""));
#         info <- info$size;
#         if(!is.na(info)){
#           if(as.numeric(info) > 3){
#             if(dir.exists(proj$results_path)){
#               dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#               dframe <- dframe[which(dframe$Area.equivalent.circle.diameter >= 3 & dframe$Area.equivalent.circle.diameter <= 30),];
#               dframe <-dframe[which(dframe$Elong. <= 0.6),];
#               if(nrow(dframe)==0){
#                 dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#                 dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
#               }
#               dframe$ct_file_name <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
#               dframe$cancer <- 1;
#               proj$dataset_KAGGLE_CANCER <- rbind(proj$dataset_KAGGLE_CANCER, dframe);
#               if(h%%PRINT_BATCH_SIZE >=0 && h%%PRINT_BATCH_SIZE <=5){print(paste("Loading & processing KAGGLE_CANCER_SET ", h, ".../", nrow(dframe), "..."));}
#             }else{
#               countNAS <- countNAS +1;
#               heading <- c("X.1", "Frame", "Label", "X", "Y", "Area", "Area.Conv..Hull",
#                            "Peri.", "Peri..Conv..Hull", "Feret", "Min..Feret", "Maximum.inscriped.circle.diameter",
#                            "Area.equivalent.circle.diameter", "Long.Side.Length.MBR", "Short.Side.Length.MBR",
#                            "Aspect.Ratio", "Area.Peri.", "Circ.", "Elong.", "Convexity",
#                            "Solidity", "Num..of.Holes", "Thinnes.Rt.", "Contour.Temp.",
#                            "Orientation", "Fract..Dim.", "Fract..Dim..Goodness", "ct_file_name","cancer"
#               );
#               notavs<- rep(NA, length(heading));
#               notavs[length(heading)-1] <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
#               notavs[length(heading)] <- 1;
#               dframe <-NULL;
#               dframe <-data.frame(t(notavs));
#               colnames(dframe)<-heading;
#               proj$dataset_KAGGLE_CANCER <- rbind(proj$dataset_KAGGLE_CANCER, dframe);
#             }
#           }
#         }
#       }
#     }
#   }
# }
# 
# DATASET_LEN_CANCER <- NULL;
# proj$dataset_cancer <- NULL;
# proj$dataset_cancer <- rbind(proj$dataset_cancer, proj$dataset_LUNA_CANCER);
# proj$dataset_cancer <- rbind(proj$dataset_cancer, proj$dataset_KAGGLE_CANCER);
# DATASET_LEN_CANCER <- nrow(proj$dataset_cancer);
# 
# print(paste("CANCER DF_LEN:", DATASET_LEN_CANCER, " from ", TOTAL_DATASET_LEN_CANCER));



# #=============================================N========================
# proj$dataset_KAGGLE_NORMAL <- NULL;
# proj$dataset_LUNA_NORMAL <- NULL;
# TOTAL_DATASET_LEN_NORMAL <- 0;
# h<-NULL; dframe<-NULL; blobs_list<-NULL;
# #CADIDATES
# results_dir<-NULL; results_dir <- "RESULTS";
# #TODO: use settings_paths
# res_path<-NULL; res_path<-"D:\\Kaggle\\Bowl2017\\image_input\\stage2_images\\";
# dcm_list<-NULL; dcm_list <- list.dirs(path = res_path, recursive = FALSE);
# dcm_list <- dcm_list[which("NORMAL"==as.vector(str_match(dcm_list, "NORMAL")))];
# #TODO: use settings_paths
# TOTAL_DATASET_LEN_NORMAL <- TOTAL_DATASET_LEN_NORMAL + length(dcm_list);
# for(h in (1):(length(dcm_list))){
#   if(!is.na(dcm_list[h])){
#     dcm_dir <- dcm_list[h];
#     proj$results_path<-NULL; proj$results_path <-paste(dcm_dir,"\\",results_dir, sep="");
#     blobs_list<-NULL; blobs_list <- list.files(path = proj$results_path, pattern = ".csv", recursive = FALSE);
#     for(j in 1:1){
#       if(length(blobs_list)!=0){
#         dframe<-NULL;
#         info<-NULL; info = file.info(paste(proj$results_path, "\\",blobs_list[j], sep=""));
#         info <- info$size;
#         if(!is.na(info)){
#           if(as.numeric(info) > 3){
#             if(dir.exists(proj$results_path)){
#               dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#               dframe <- dframe[which(dframe$Area.equivalent.circle.diameter >= 3 & dframe$Area.equivalent.circle.diameter <= 30),];
#               dframe <-dframe[which(dframe$Elong. <= 0.6),];
#               if(nrow(dframe)==0){
#                 dframe<-read.csv(paste(proj$results_path, "\\", blobs_list[j], sep=""), na.strings=c("", "NaN", "NULL", "N/A", "null", "-", "#REF!", "#VALUE!", "?", "#NULL!", "#NUM!", "#DIV/0", "n/a", "#NAME?", "NIL", "nil", "na", "#N/A", "NA"), strip.white=TRUE, encoding="UTF-8",row.names=NULL, sep=",");
#                 dframe <-dframe[which(dframe$Area.equivalent.circle.diameter == min(dframe$Area.equivalent.circle.diameter)) ,];
#               }
#               dframe$ct_file_name <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
#               dframe$cancer <- 0;
#               proj$dataset_KAGGLE_NORMAL <- rbind(proj$dataset_KAGGLE_NORMAL, dframe);
#               if(h%%PRINT_BATCH_SIZE >=0 && h%%PRINT_BATCH_SIZE <=5){print(paste("Loading & processing KAGGLE_NORMAL_SET ", h, ".../", nrow(dframe), "..."));}
#             }
#           }else{
#             # countNAS <- countNAS +1;
#             # heading <- c("X.1", "Frame", "Label", "X", "Y", "Area", "Area.Conv..Hull",
#             #              "Peri.", "Peri..Conv..Hull", "Feret", "Min..Feret", "Maximum.inscriped.circle.diameter",
#             #              "Area.equivalent.circle.diameter", "Long.Side.Length.MBR", "Short.Side.Length.MBR",
#             #              "Aspect.Ratio", "Area.Peri.", "Circ.", "Elong.", "Convexity",
#             #              "Solidity", "Num..of.Holes", "Thinnes.Rt.", "Contour.Temp.",
#             #              "Orientation", "Fract..Dim.", "Fract..Dim..Goodness", "ct_file_name", "cancer"
#             # );
#             # notavs<- rep(NA, length(heading));
#             # notavs[length(heading)-1] <- str_split(str_split(dcm_dir,"\\\\")[[1]][6],"\\/")[[1]][2];
#             # notavs[length(heading)] <- 0;
#             # dframe <-NULL;
#             # dframe <-data.frame(t(notavs));
#             # colnames(dframe)<-heading;
#             # proj$dataset_KAGGLE_NORMAL <- rbind(proj$dataset_KAGGLE_NORMAL, dframe);
#           }
#         }
#       }
#     }
#   }
# }
# 
# DATASET_LEN_NORMAL <- NULL;
# proj$dataset_normal <- NULL;
# #proj$dataset_normal <- rbind(proj$dataset_normal, proj$dataset_LUNA_NORMAL);
# proj$dataset_normal <- rbind(proj$dataset_normal, proj$dataset_KAGGLE_NORMAL);
# DATASET_LEN_NORMAL <- nrow(proj$dataset_normal);
# print(paste("NORMAL DF_LEN:", DATASET_LEN_NORMAL, " from ", TOTAL_DATASET_LEN_NORMAL));
# print(paste("COUNT_NAS:", countNAS, " & for test: ", countNAS_test));
# 

##ALT
# searchGridSubCol <- expand.grid(colsample_bytree = c(0.5, 0.75, 1.0),    subsample = c(0.5, 0.75, 1.0));
# ntrees <- 100;
# xMatrix <- as.matrix(dataf);
# DMMatrixTrain <- xgb.DMatrix(data = xMatrix[,-1], label = "Species");
#
# logLossErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
#   currentSubsampleRate <- parameterList[["subsample"]];
#   currentColsampleRate <- parameterList[["colsample_bytree"]];
#
#   xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = ntrees, nfold = 5, showsd = TRUE,
#                            metrics = "logloss", verbose = TRUE, "eval_metric" = "logloss",
#                            "objective" = "binary:logistic", "max.depth" = 15, "eta" = 2/ntrees,
#                            "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
#   #classwt = proj$weights
#   xvalidationScores <- as.data.frame(xgboostModelCV);
#   #rmse <- tail(xvalidationScores$test.rmse.mean, 1)
#   loglosserror <- tail(xvalidationScores$test.logloesserror.mean, 1); #TODO: double check
#   return(c(loglosserror, currentSubsampleRate, currentColsampleRate));
# })


# My XGB grid:
# xgb_grid_1 = expand.grid(
#     nrounds = 1000,
#     eta = c(0.01, 0.015, 0.025, 0.05, 0.1), #c(0.01, 0.001, 0.0001),
#     max_depth = c( 3, 5, 7, 9, 12, 15, 17, 25),
#     gamma = c(0.05, 0.075, 0.1,0.3,0.5,0.7,0.9,1.0),
#     colsample_bytree = c(0.5, 0.75, 1.0),
#     min_child_weight = c(1.0,1.1,1.2,1.5,2.0),
#     subsample = c(0.5, 0.75, 1.0)
#   )


#gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
#print(h2o.auc(h2o.performance(gbm, newdata = test)))
#model <- do.call(h2o.gbm,
## update parameters in place
# {
#   p <- gbm@parameters
#   p$model_id = NULL          ## do not overwrite the original grid model
#   p$training_frame = df      ## use the full dataset
#   p$validation_frame = NULL  ## no validation frame
#   p$nfolds = 5               ## cross-validation
#   p
# }
# )
# model@model$cross_validation_metrics_summary

# for (i in 1:5) {
#   proj$H2O_tuned_GBM <- h2o.getModel(sortedGrid@model_ids[[i]])
#   cvgbm <- do.call(h2o.gbm,
#                    {
#                      p <- gbm@parameters
#                      p$model_id = NULL          ## do not overwrite the original grid model
#                      p$training_frame = h2o_trainset      ## use the full dataset
#                      p$validation_frame = NULL  ## no validation frame
#                      p$nfolds = 5               ## cross-validation
#                      p
#                    }
#   )
#   print(proj$H2O_tuned_GBM@model_id);
#   print(cvgbm@model$cross_validation_metrics_summary[5,]); #pick best row
# }
#--## *** PARAMS *** -- Don't forget to update addToLog call below##--##


#   h2o.gbm(x, y, train, nfolds = 0, seed = proj$seed,
#                    # max_depth = 15, ntrees = 500, sample_rate = 0.7,
#                    # mtries = 4, col_sample_rate_per_tree = 0.9,
#                    # stopping_metric = "logloss",
#                    # stopping_tolerance = 0,
#                    # stopping_rounds = 5,
#                    # score_tree_interval = 3,
#                    # class_sampling_factors = proj$weights
#   )
# });

#--------------------------------------------------------------------------------

##K_BEST_SCORE: ** 0.64374 ** ##LL  -- ACC 0.670714 -> 0.64374 K_score
##LL(\\) 1.25410479092238e-06 -- ACC(/)  0.999998745895995-> ? RF
##"##LL(\\) 0.00477143103683003 -- ACC(/)  0.995239934156975-> ?"
##LL(\) 0.4525024...      -- ACC(/)  0.6784129...       ->  ?
##LL(\\) 0.469620145678704 -- ACC(/)  0.670714043065471-> ?
##LL(\\) 0.469620145678704 -- ACC(/)  0.670714043065471-> ?
##LL(\\) 0.469900480693789 -- ACC(/)  0.670464462302636-> ?
##LL(\\) 0.472128101653741 -- ACC(/)  0.670468657672196-> ?
##LL(\) 0.4983319...      -- ACC(/)  0.6551379...       ->  ?
#LL(\\) 0.498248884362098 -- ACC(/)  0.654906420947601  ->  ?
##LL(\) NA                -- ACC(/)  0.66404            -> 1.39016 K_score
