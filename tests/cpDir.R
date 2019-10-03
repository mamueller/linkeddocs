#------------------------------------------------------------------------------
cpDir<-function(sourceDirPath,targetDirPath){
  # copy a directory tree recursively  
	all_entries<-list.files(sourceDirPath)
	all_dirs<-all_entries[
    as.logical(lapply(all_entries,function(entry){dir.exists(file.path(sourceDirPath,entry))}))
    ]
	all_files<-setdiff(all_entries,all_dirs)
	
  if (!dir.exists(targetDirPath)){
    dir.create(targetDirPath)
  }
  # first we copy files (this would however not include the empty dirs))
	lapply(
		all_files,
		function(fp){
			file.copy(file.path(sourceDirPath,fp),file.path(targetDirPath,fp))
		}
	)
  
  lapply(
		all_dirs,
		function(subDirName){
			cpDir(file.path(sourceDirPath,subDirName),file.path(targetDirPath,subDirName))
		}
  )
}
