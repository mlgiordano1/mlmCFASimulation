library(gimme)

args=commandArgs(TRUE)

print(args[1])

condition <- as.name(args[1])

in_path<-file.path('/proj','hng','hopfinger','GIMME','timecourse_data','analysis_with_fewer_ROIs','GIMME_comparisons',condition)
out_path<-file.path('/proj','hng','hopfinger','GIMME','GIMME_results','analysis_with_fewer_ROIs',paste0(condition,'_out'))

if(!file.exists(out_path)) dir.create(out_path)

print(paste0('in path = ',in_path))
print(paste0('out path = ',out_path))

gimmeSEM(data<-in_path,out<-out_path,sep = ",", plot = TRUE,header=FALSE,subgroup=FALSE)

#prelim_analysis_30_subjects analysis_with_fewer_ROIs