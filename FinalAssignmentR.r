#R Programming Assignment 2
#James Freemantle

#Importing from csv and tsv and assigning data to varaiables.
library(readr)
advertiser = read.csv('C:/College/yr31 Stats/advertiser.csv')
campaigns = read.csv('C:/College/yr31 Stats/campaigns.csv')
clicks = read_tsv('C:/College/yr31 Stats/clicks.tsv')
impressions = read_tsv('C:/College/yr31 Stats/impressions.tsv')


#A function that changes timezone if given dataset
converttimezone <- function(dataset)
{
    #As eachtimezone has different required editing, i have divided the dataset into (pacific, eastern and utc)
    pacific <- filter(dataset, timezone == 'Pacific time')
    dummy<-pacific                                          #I am about to edit pacific, so we store original data in dummy 
    pacific$time<-as.character(as.numeric(pacific$time))     #taking time variable out of timestamp so i can edit the time                                            
            
    for (i in seq_along(dummy$time))
    {
        strtime = gsub("[:]", "" ,dummy$time[i])            #removing ":" Example: 13:45 -> 1345
        strtime_split<-strsplit(strtime,"")[[1]]            #splitting characters. Example 1345 -> "1""3""4""5"
        first<- strtime_split[1]                            #taking first char. Example "1""3""4""5" -> "1"
        second<-strtime_split[2]                            #taking second char. Example "1""3""4""5" -> "3"
        hour<-paste(first,second,sep="")                    #joining the 2 chars. Example "13"
        if (as.numeric(hour)>15)                            #since pacific timezone adds 8 hours, if the time is 16:00 or more,
                                                            #I add 8 and subtract 24
        {
            hour=as.character(as.numeric(hour)+8-24)
            hour=paste("0",hour,sep="")                     #I must preserve the 0 in time. Example 4 -> 04
            strdate = gsub("[/]", "" ,pacific$date[i])      #Since the day has changed, I must change the date
                                                            #remove "/"" from the date string
            strdate_split<-strsplit(strdate,"")[[1]]        #similar to time, split into individual characters
            first<- as.numeric(strdate_split[1])            #store the first and second chars (the day of month) as numeric
            second<-as.numeric(strdate_split[2])
                if(second==9)                               #if second char is 9, we must add 1 to first char and make second char =0 (10/20)
                {
                    first<-first+1
                    strdate_split[1]<-as.character(first)
                    second<-0
                    strdate_split[2]<-as.character(second)
                }else
                {
                    second<-second+1                        #if not =9, add one to the second char
                    strdate_split[2]<-as.character(second)
                }
            a<-paste(strdate_split,collapse="")             #join chars back together again. Example: "1""2""1""1""2""0""1""8"-> "12112018"
            updateddate= gsub("(\\d\\d)(\\d\\d)(\\d\\d)", "\\1/\\2/\\3", a) #Using regular expression, 
                                                                        #look for pattern of 6 digits and insert a "/" between every second digit.
            pacific$date[i]<-updateddate
        }
        else if (as.numeric(hour)>1)                #simply add 8 to the hour, no need to change date
        {
            hour=as.character(as.numeric(hour)+8)
        }
        else
        {                                            #if hour is 1 or 0, we must be careful to preserve the 0 once 8 hours is added. Example 9 -> 09
            second = as.character(as.numeric(second)+8)
            hour=paste("0",second,sep="")                #insert the 0
        }
        updatedtime<-paste(hour,strtime_split[3],strtime_split[4],sep="") #insert mins our updated hour 
        updatedtime = gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", updatedtime)   #insert the ":" between hour and mins 
        pacific$time[i]<-updatedtime                     #update the pacific dataframe with our updated time

        pacific$timezone[i]<-"UTC"                           #change the timezone to UTC
    }

    eastern <- filter(dataset, timezone == 'Eastern time')     #repeat same process with eastern time
    dummy<-eastern
    eastern$time<-as.character(as.numeric(eastern$time))
    for (i in seq_along(dummy$time))
    {
        strtime = gsub("[:]", "" ,dummy$time[i])
        strtime_split<-strsplit(strtime,"")[[1]]
        firsttime<- strtime_split[1]
        secondtime<-strtime_split[2]
        hour<-paste(firsttime,secondtime,sep="")
        if (as.numeric(hour)>18)                         #converting eastern to utc requires addition of 5 hours
        {                                                #if 19:00 or more, then it becomes a new day
            hour=as.character(as.numeric(hour)+5-24)     #same process described for pacific
            hour=paste("0",hour,sep="")
            strdate = gsub("[/]", "" ,eastern$date[i])
            strdate_split<-strsplit(strdate,"")[[1]]
            firstdate<- as.numeric(strdate_split[1])
            seconddate<-as.numeric(strdate_split[2])
                if(seconddate==9)
                {
                    firstdate<-firstdate+1
                    strdate_split[1]<-as.character(firstdate)
                    seconddate<-0
                    strdate_split[2]<-as.character(seconddate)
                }else
                {
                    seconddate<-seconddate+1
                    strdate_split[2]<-as.character(seconddate)
                }
            a<-paste(strdate_split,collapse="")
            updateddate= gsub("(\\d\\d)(\\d\\d)(\\d\\d)", "\\1/\\2/\\3", a)
            eastern$date[i]<-updateddate
        }
        else if (as.numeric(hour)>4)
        {
            hour=as.character(as.numeric(hour)+5)
        }
        else
        {
            secondtime = as.character(as.numeric(secondtime)+5) #if hour is 4 or less, we must preserve the 0. ->04:00
            hour=paste("0",secondtime,sep="")
        }
        updatedtime<-paste(hour,strtime_split[3],strtime_split[4],sep="")
        updatedtime = gsub("([0-9]+)(\\d\\d)", "\\1:\\2", updatedtime)
        eastern$time[i]<-updatedtime
        
        eastern$timezone[i]<-"UTC"
    }

    utc <- filter(dataset, timezone == 'UTC')                 #for utc we dont have to change anything, 
                                                              #but we need to take utc out of timestamp and convert into charaacter
                                                              #this is because all of our other time are characters and we cant have timestamp and character in the same column
    utc$time<-as.character(utc$time)
    for (i in seq_along(utc$time))
    {
        strtime_split<-strsplit(utc$time[i],"")[[1]]
        utc$time[i]<-paste(strtime_split[1],strtime_split[2],strtime_split[3],strtime_split[4],strtime_split[5],sep="")
    }
    correctdata<-rbind(utc,eastern,pacific)                   #we bind all this dataframes into a single dataframe 
    correctdata = arrange(correctdata,desc(campaign_id))      #we order by campaign_id because this is the way our original dataset was ordered.
    return(correctdata)
}


names(campaigns)[names(campaigns) == "id"] <- "campaign_id"        #clarifying id and name for raedability in the dataset
names(campaigns)[names(campaigns) == "name"] <- "campaign_name"
names(advertiser)[names(advertiser) == "ID"] <- "advertiser_id"
names(advertiser)[names(advertiser) == "name"] <- "product_name"
nearly<-merge(x=advertiser,y=campaigns,by='advertiser_id')          #merging the two datasets by advertiser_id because advertiser_id is a variable in both datasets

correctclicks <- converttimezone(clicks)                           #calling the method converttimezones, this method returns "correctdata"

finalclicks<-merge(x=nearly,y=correctclicks,by='campaign_id')       #merging the dataset with all columns from campaign and advertiser into clicks by campaign_id
write.csv(finalclicks,file ="clicks_processed.csv")                 #write to csv


correctimpressions <- converttimezone(impressions)                  #repeate with impressions

finalimpressions<-merge(x=nearly,y=correctimpressions,by='campaign_id')
write.csv(finalimpressions,file ="impressions_processed.csv")


