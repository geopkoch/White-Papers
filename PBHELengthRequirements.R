

source("INTERNAL PATH")
if (!require("pacman")) install.packages("pacman")
p_load(dplyr,readr,tidyr,data.table,broom,dummies,purrr,RODBC,dbplyr,stringdist,quanteda)

mypath <- "INTERNAL PATH"

note_that("QueryData_begin")
mydata.master <- queryDB(paste0(mypath,"/PBHE Forum Post Analysis.sql"),
                  cache = "mydata.master",
                  odbc = "APUS_DW")
note_that("QueryData_end")

mydata <- mydata.master %>%
  mutate(SakaiThrdScrubbedBody = as.character(SakaiThrdScrubbedBody),
        ParentPost = as.character(ParentPost),
        RepliedTo = as.character(RepliedTo)
         ) 

#only want these sessions when testing begins

mysessions <- head(mydata %>% select(sesnstdt) %>% unique() %>% arrange(sesnstdt), 6)


# mydata.head <- head(mydata,100)

#removing forum posts with NULL values

mydata <- mydata %>%
  filter(SakaiThrdScrubbedBody != "") %>%
  filter(SakaiThrdScrubbedBody != "NA")


note_that("Readablity_begin")
readability.df <- textstat_readability(mydata$SakaiThrdScrubbedBody,
                                       measure = c("Flesch.Kincaid","SMOG",
                                                   "meanSentenceLength","meanWordSyllables")
)
note_that("Readability_end")

#get rid of text row names
rownames(readability.df) <- c()


note_that("LexicalDiversity_begin")
lexdiv.final <- textstat_lexdiv(dfm(mydata$SakaiThrdScrubbedBody),
                               measure =  c("TTR","C","R"))
note_that("LexicalDiversity_end")

mydata.final <- cbind(mydata,readability.df,lexdiv.final)


#measure = c("TTR","C","R")

#will join to get actual content later - writing to CSV was causing issues when commas were present
mydata.final <- mydata.final %>%
  select(-SakaiThrdScrubbedBody,-RepliedTo,-ParentPost)


note_that("WriteFile_begin")
fwrite(mydata.final,paste0(mypath,"/PBHEPosts.csv"))
note_that("WriteFile_end")

note_that("EndScript")



#BEGIN TESTS


#satisfaction in the course
mydata.satisfaction <- mydata.final %>%
  select(CrseOffrdId,LatestCrseNbr,isNoReqSection,SakaiThrdCrtdBy,class_satisfaction,design_satisfaction) %>%
  filter(!is.na(class_satisfaction)) %>%
  unique()

t.test(class_satisfaction ~ isNoReqSection , data = mydata.satisfaction)
t.test(design_satisfaction ~ isNoReqSection , data = mydata.satisfaction)

do.cohen.d(mydata.satisfaction %>% filter(isNoReqSection == 1),
           mydata.satisfaction %>% filter(isNoReqSection == 0),
           5,5)



#Posts per student in course by post type
mydata.posts <- mydata.final %>%
  filter(sesnstdt %in% mysessions$sesnstdt,
         grepl("^[0-9]+$", SakaiThrdCrtdBy, perl = T) == TRUE) %>% #only want students - anything that has alpha characters returns FALSE
  select(CrseOffrdId,LatestCrseNbr,isNoReqSection,SakaiThrdCrtdBy,SakaiThrdID,PostType) %>%
  group_by(LatestCrseNbr,PostType,isNoReqSection,SakaiThrdCrtdBy) %>%
  summarize(posts = n()) %>% #number of total thread posts by students
  ungroup() 


#run tests to determine if posts per students in course by post type truly differ across treatment groups
mydata.posts %>% 
  nest(-LatestCrseNbr,-PostType ) %>%
  mutate(test = map(data, ~ t.test(posts ~ isNoReqSection , data = .)),
       results = map(test, tidy)) %>%
  unnest(results)

#run tests to determine if various measures across treatment groups and post types are different
mydata.quality <- mydata.final %>%
  filter(sesnstdt %in% mysessions$sesnstdt,
         grepl("^[0-9]+$", SakaiThrdCrtdBy, perl = T) == TRUE) %>% #only want students - anything that has alpha characters returns FALSE
  select(CrseOffrdId,LatestCrseNbr,isNoReqSection,SakaiThrdID,PostType,meanSentenceLength,meanWordSyllables,
         Flesch.Kincaid,SMOG,TTR) 


qualitytests <- mydata.quality %>%
  select(-CrseOffrdId,-SakaiThrdID) %>%
  gather(variable,value,-LatestCrseNbr,-isNoReqSection,-PostType) %>%
  group_by(LatestCrseNbr,isNoReqSection,PostType,variable) %>%
  nest() %>%
  spread(isNoReqSection,data) %>%
  mutate(t_test = map2(`0`,`1`, ~{tidy(t.test(.x$value,.y$value))})) %>%
  unnest(t_test)


#run tests to determine if word length for posts is actually different
mydata.words <- mydata.final %>%
  filter(sesnstdt %in% mysessions$sesnstdt,
         grepl("^[0-9]+$", SakaiThrdCrtdBy, perl = T) == TRUE) %>% #only want students - anything that has alpha characters returns FALSE
  select(LatestCrseNbr,isNoReqSection,SakaiThrdID,PostType, SakaiThrdScrubbedBodyWordCnt) 

wordcounts.test <- mydata.words %>%
  nest(-LatestCrseNbr,-PostType) %>%
  mutate(test = map(data, ~ t.test(SakaiThrdScrubbedBodyWordCnt ~ isNoReqSection , data = .)),
         results = map(test, tidy)) %>%
  unnest(results) %>%
  select(-data,-test)
#estimate 2 is the pilot group
  
