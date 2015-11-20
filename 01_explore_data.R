folder.main <- '~/Dropbox/coursera/Bigdata_tracker/10_capstone_1015/'
folder.data <- paste(folder.main,'data/',sep = "")
files.names <- dir(folder.data)
data.files <-grep(".json",files.names,value = T)

# decide the lines number 
line.numbers<-sapply(data.files, function(x)
  system(paste("wc -l < ",paste(folder.data,x,sep="")),intern = T))

review.files <- grep("review",data.files,value = T)

# read the 100th line
con <- file(paste(folder.data,review.files,sep = ""),"r")
lines_100 <- readLines(con, 100) ## Read the first line of text 
library(stringr)
str_match(string = lines_100[100],pattern = "[0-9]* years")
close(con)

## 5 star reviews 
#install.packages("rjson")
library(rjson)
review.no <-as.numeric(str_match(string = grep("review",line.numbers,value = T)[[1]],pattern = " [0-9]*"))
con <- file(paste(folder.data,review.files,sep = ""),"r")
review.stars <- try(sapply(1:review.no, 
                           function(x) 
                             {print(x);fromJSON(readLines(con,1))$stars}))
close(con)

sum(review.stars == 5)/length(review.stars)*100
save.image(file="qz1.RData")


## business
business.no <-as.numeric(str_match(
  string = grep("business",line.numbers,value = T)[[1]],pattern = "[0-9]+"))
business.file <- grep("business",data.files,value = T)
con <- file(paste(folder.data,business.file,sep = ""),"r")
business.line_1 <- readLines(con, 1) ## Read the first line of text 
tmp<-fromJSON(business.line_1)$attributes$`Wi-Fi`
tmp
close(con)

con <- file(paste(folder.data,business.file,sep = ""),"r")
business.wifi <-try(sapply(1:business.no, 
                           function(x) 
                           {print(x);fromJSON(readLines(con,1))$attributes$`Wi-Fi`}))
close(con)
business.wifi_clean <- unlist(business.wifi)
business.wifi_summary <-summary(factor(business.wifi_clean))
business.wifi_summary["free"]/sum(business.wifi_summary)*100

## TIP files
tip.no <-as.numeric(str_match(
  string = grep("tip",line.numbers,value = T)[[1]],pattern = "[0-9]+"))
tip.file <- grep("tip",data.files,value = T)
con <- file(paste(folder.data,tip.file,sep = ""),"r")
tip.line_1000 <- readLines(con, 1000)[1000] ## Read the first line of text 
str_match(tip.line_1000,"terrible [a-zA-Z]+")

## users 
user.no <- as.numeric(str_match(
  string = grep("user",line.numbers,value = T)[[1]],pattern = "[0-9]+"
))
user.file <- grep("user",data.files,value = T)
con<-file(paste(folder.data,user.file,sep=""),"r")
for(u in 1:user.no){
  print(u)
  tmp<-with(fromJSON(readLines(con,1)),data.frame(name=name,funny=votes$funny))
  if(tmp$funny >10000 & tmp$name %in% c("Brian","Ira","Jeff","Roger")){
    print(tmp)
    break
  }
}
close(con)

# cross tabulation 
con<-file(paste(folder.data,user.file,sep=""),"r")
tmp<-fromJSON(readLines(con,1))
close (con)

install.packages("data.table")
library(data.table)
#system.time(tmp<-fread(input =paste(folder.data,user.file,sep=""),header=F,sep = "\n"))

con<-file(paste(folder.data,user.file,sep=""),"r")
system.time(
 user.funny_user_fans<-lapply(1:user.no,function(x){ #user.no
   print(x);
   with(fromJSON(readLines(con,1)),
        data.frame(name=name,funny=votes$funny,fans=fans,
                   stringsAsFactors = F))
   })
)
# user.funny_user_fans<-data.frame(name=character(user.no),
#                                  funny = numeric(user.no),
#                                  fans = numeric(user.no),
#                                  stringsAsFactors = F)
# for(i in 1:user.no){
#   print(i)
#   with(fromJSON(readLines(con,1)), {
#     user.funny_user_fans$name[i]<-name
#     user.funny_user_fans$funny[i]<-votes$funny
#     user.funny_user_fans$fans[i]<-fans
#   })
# }
close(con)

 user.funny_user_fans_clean<-data.frame(name=character(user.no),
                                  funny = numeric(user.no),
                                  fans = numeric(user.no),
                                  stringsAsFactors = F)

#library(plyr)
#user.funny_user_fans_clean<-do.call(rbind,user.funny_user_fans)

#system.time(do.call(rbind.data.frame,user.funny_user_fans[1:10000]))*user.no/10000

system.time(unlist(user.funny_user_fans[1:10000]))*user.no/10000

user.funny_user_fans_clean<-data.frame(matrix(unlist(
  user.funny_user_fans),nrow = user.no,byrow = T),stringsAsFactors = F)
#rm("user.funny_user_fans")
#rm(list = c("tmp3","tmp","tmp2","u","con","food","parser","i"))
#rm(list=c("sample_json"))
head(user.funny_user_fans_clean)
names(user.funny_user_fans_clean)<-c("name","funny","fans")
?table

user.fans_funny_tab<-with(user.funny_user_fans_clean,table(fans>1,funny>1))
fisher.test(user.fans_funny_tab)
with(airquality, table(cut(Temp, quantile(Temp)), Month))
sum(user.funny_user_fans_clean$funny==0)
