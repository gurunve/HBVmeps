#
#
#
#################################################################
#' @title Load meps flood data for a set of stations
#' @description Loads meps flood forecasting data for all available models
#'              This function concatenates the results of all other functions defined in the file "read_flood_forecasting_meps_data.R".
#'
#' @param regine_main Vector of station numbers (regine_area and main_no separated by a full stop: see example).
#'        Default is meta_data$regine_main from the packaged meta_data
#' @param HBV3h_det_2018_filename File with HBV3h_det_2018 results
#'  Default is the network location: '//hdata/drift/flom/HBV3h/utskrift2018/hbv_3hresOppdat.txt'
#' @param HBV3h_ens_2018_filename File with HBV3h_ens_2018 results
#'  Default is the network location: '//hdata/drift/flom/HBV3h/utskrift2018/ens_3hres_Oppdat.txt'
#'
#' @examples
#' # For a specific set of stations:
#' regine_main <- c('1.48','1.49','1.50')
#' load_flood_data(regine_main, save_rdata = FALSE)
#' # Or to run for every station:
#' # load_meps_flood_data()
#' # The following files should now be available in your working directory:
#' # data_all.RData, meta_data.rda, HBV3h_det_2018.RData, HBV3h_ens_2016.RData and flomtabell.RData
#' @return NULL The function saves several .rda files to the working directory
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' @importFrom tidyr %>%
#' @importFrom dplyr right_join
#' @importFrom dplyr filter
#' @importFrom reshape merge_all
#' @export
library(reshape2)

load_flood_data <- function(det_filename = '//hdata/drift/flom/HBV3h/utskrift2018/hbv_3hresOppdat.txt',
                            ens_filename = '//hdata/drift/flom/HBV3h/utskrift2018/ens_hbv3h_Oppdat.txt',
                            save_path = "/data", save_rdata = TRUE) {

  
  ## HBV3h meps DATA READ FROM THE NVE NETWORK
  print("HBV DATA reading")
  print(paste("HBV3h_ens_filename: ",det_filename,ens_filename,sep=" "))
  
  # Flood statistics data
  FlomTabell <- read_statistics_data()
  print("start read HBV3h")
  HBVmeps <- read_HBV3h_meps_data(det_filename,ens_filename, FlomTabell)
  print("Ready read HBV3h")
  
  ## Create a file with the date and time of last update
  update_time <- Sys.time()
  if (save_rdata ==TRUE) {
    save(HBVmeps, file = paste(getwd(), save_path,"/","HBVmeps.RData", sep = ""))
    save(update_time, file = paste(getwd(), save_path,"/","update_time.RData", sep = ""))
    save(FlomTabell, file = paste(getwd(), save_path,"/","FlomTabell.RData", sep = ""))
    
    group1 <-makeGroupData(HBVmeps,FlomTabell,firstdt,lastdt, 1)
    save(group1,file = paste(getwd(), save_path,"/","group1.RData", sep = ""))
    
    group2 <-makeGroupData(HBVmeps,FlomTabell,firstdt,lastdt, 2)
    save(group2,file = paste(getwd(), save_path,"/","group2.RData", sep = ""))
    
    group3 <-makeGroupData(HBVmeps,FlomTabell,firstdt,lastdt, 3)
    save(group3,file = paste(getwd(), save_path,"/","group3.RData", sep = ""))
    group4 <-makeGroupData(HBVmeps,FlomTabell,firstdt,lastdt, 4)
    save(group4,file = paste(getwd(), save_path,"/","group4.RData", sep = ""))
    group5 <-makeGroupData(HBVmeps,FlomTabell,firstdt,lastdt, 5)
    save(group5,file = paste(getwd(), save_path,"/","group5.RData", sep = ""))
    group6 <-makeGroupData(HBVmeps,FlomTabell,firstdt,lastdt, 6)
    save(group6,file = paste(getwd(), save_path,"/","group6.RData", sep = ""))
    
  }
}
#--------------------------------------------
# function - read_HBV3h_det
read_HBV3h_meps_data <- function(detfile,ensfile,FlomTabell){
  #
  #---Felt list 
  station_ref <- read.table(file="data/HbvFelt145.cfg")
  Felts <-paste(station_ref$V1,".",station_ref$V2,sep="")
  station_names <- station_ref$V5
  Felt_Names <- paste(Felts,station_names,sep="_")
  #--floodtable kul
   cr <-72
  cRows <- 864
  cStationName     <- c()
  cDateTime <- c()
  cMember <- c()
  cDischarge <- c()
  cMeps <- c()

  
  # Opne files
  #----det
  det_connect<- file(detfile,open='r')
  ens_connect<- file(ensfile,open='r')
  
  #---------for alle felt ----------------------------
  for(i in 1:145) {
    detline <-substring(readLines(det_connect,n=1),2)
    ensline <-substring(readLines(ens_connect,n=1),2)
    
    name <-substring(ensline,regexpr(" ",ensline)[1] +1)
    index <- which(station_names==name)
    
    if(length(index)>=1){
      FeltName <- capFirst(Felt_Names[index])
    }else{
      FeltName <- "NA"
    }
    
    #1. station name
    cStationName<- as.data.frame.vector(rep(FeltName,cRows))
    colnames(cStationName) <- "FeltName"
    
    #2. read det file
    dtemp <- as.data.frame(read.table(det_connect,nrows=cr,sep=' '))
    colnames(dtemp) <- c("Yr","mon","day","hr","p","t","e","swe","sco","sm",
                         "uz","lz","detsim","Obs","det")
    
    #3. read ens file 
    enstemp <- as.data.frame(read.table(ens_connect,nrows=cr,sep=' '))  # read 72 lines
    colnames(enstemp) <- c("Yr","mon","day","hr","Obs","Qs0","en0","Qs1","en1","Qs2","en2","Qs3","en3",
                           "Qs4","en4","Qs5","en5","Qs6","en6","Qs7","en7","Qs8","en8","Qs9","en9")
    #4. DateTime
    if(i==1){
      tempdt<-transform(enstemp, datetime = as.POSIXct(paste(paste(Yr,mon,day,sep="-"), paste(hr,"00",sep=":"))))
      dt<-as.data.frame.vector(tempdt$datetime)
      colnames(dt) <- "DateTime"
      cDateTime<-bind_rows(dt,dt,dt,dt,dt,dt,dt,dt,dt,dt,dt,dt)
      firstdt <-dt[1,]
      lastdt <-dt[72,]
    }
    
    #5. Member name
    cMember<-as.data.frame.vector(c(rep("Obs",cr),rep("det",cr),rep("en0",cr),rep("en1",cr),
                                    rep("en2",cr),rep("en3",cr),rep("en4",cr),rep("en5",cr),
                                    rep("en6",cr),rep("en7",cr),rep("en8",cr),rep("en9",cr)))
    colnames(cMember) <-"Member"
    
    #6 Discharge
    cDischarge<- as.data.frame.vector(c(dtemp$Obs,dtemp$det,enstemp$en0,enstemp$en1,
                                        enstemp$en2,enstemp$en3,enstemp$en4,enstemp$en5,
                                        enstemp$en6,enstemp$en7,enstemp$en8,enstemp$en9))
    
    colnames(cDischarge) <-"Discharge"
    cDischarge[cDischarge==-9999.00] <- NA
    
    stdata<-as.data.frame(bind_cols(cStationName, cDateTime,cMember,cDischarge))
    
    #7 kul flom
    ff<-dplyr::filter(FlomTabell,FeltName ==Felt_Names[index])
   
    cFlom <-c()
    cFlom$FeltName <- data.frame(FeltName=rep(ff$FeltName,6))

    cFlom$DateTime <- data.frame(DateTime=c(rep(firstdt,3),rep(lastdt,3)))
    cFlom$Member <- data.frame(Member=c("kul_Mean","kul_5Y","kul_50Y","kul_Mean","kul_5Y","kul_50Y"))
    
    cFlom$Discharge <- data.frame(Discharge=c(ff$kulMean,ff$kul5Y,ff$kul50Y,ff$kulMean,ff$kul5Y,ff$kul50Y))
    
    cFlom<-data.frame(cFlom)  
    
    cMeps <- bind_rows(cMeps,stdata,cFlom)
  }
  #6. data ens
  #####}
  
  close(det_connect)
  close(ens_connect)
  
  cMeps[cMeps == -9999.000] <-NA
  HBVmeps <- tbl_df(cMeps)
  invisible(HBVmeps)
}
#----------------------------------------------
# Uppcase the first letter in string
capFirst <- function(s){
  paste(toupper(substring(s,1,1)), substring(s,2), sep="")
}
#----------------------------------------------
#
#
read_statistics_data <-function (filename="//hdata/drift/flom/basedata/flomtabell145.rap"){
  ftab<-read.table(file=filename,sep=":",colClasses = (c(rep("character",3),rep("numeric",9))))
  FlomTabell <-c()
  fname <- paste(trim(ftab$V3),trim(ftab$V2),sep="_")
  FlomTabell$FeltName<- fname
  FlomTabell$kulMean <- ftab$V10
  FlomTabell$kul5Y <- ftab$V11
  FlomTabell$kul50Y <- ftab$V12
  FlomTabell <- dplyr::tbl_df(FlomTabell)
  
  FlomTabell[FlomTabell == -10000] <- NA
  invisible(FlomTabell)
}
#
#
#----------------------------------------------
makeGroupData <- function(HBVmeps,FlomTabell,firstdt,lastdt, fane){
  noffelt=25
  lineNo=870    # (12* 72 + 6)
  if(fane==6)noffelt=20
  
  offset=(fane-1)*25*lineNo
  i1<-(offset+1)
  i2<-offset+noffelt*lineNo
  print(paste("Fane ",fane,i1,i2,sep=" "))
  cGroup <-HBVmeps[c(i1:i2),c(1:4)]
  
  a<-c()
  for( i in 1:noffelt){ a<-c(a,cGroup$FeltName[i*lineNo])}
  cGroup$facet = factor(cGroup$FeltName,levels = a)
  
  cGroup <- tbl_df(cGroup)
  invisible(cGroup)
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
