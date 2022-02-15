
if(Sys.info()['sysname']== "Windows"){
  
  Sys.setlocale("LC_ALL","Portuguese")
  dir_data <- "C:/repositaries/3.MUVA/MUVA-fwup/data/Follow-up/3.dashboard"
  
}


if(Sys.info()['sysname']=="Linux"){
  Sys.setlocale("LC_ALL","pt_BR.UTF-8")
  dir_data <-"/home/muvamel/MUVA-fwup/data/Follow-up/3.dashboard"
  
}