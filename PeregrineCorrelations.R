#                                                                               
#                          Peregrine Inbound Test Correlations               
#                                                                               
# Purpose: Analyzing the predictability of inbound Perergrine tests on retention and student success
# Created by: Geoff Koch                                                        
# Created on: 11/24/2020                                          
# Validated by:                                               
# Validated on:                                                      
#################################################################################

# Loading Data -----------------------------------------------
source("INTERNAL PATH")
if (!require("pacman")) install.packages("pacman")
p_load(dplyr,tidyr,readr,stringr,RODBC,data.table,formattable,purrr,sparkline,readxl,janitor) 


mypath <- "INTERNAL PATH"

mysheets <- readxl::excel_sheets(paste0(mypath,"/Data/InboundMaster.xlsx"))

#return all sheets as one dataframe and then unnest into separate DFs
mydata <- purrr::map(mysheets, ~readxl::read_xlsx(path = paste0(mypath,"/Data/InboundMaster.xlsx"),
                                                  sheet = .)) 

for(i in 1:length(mydata)){
  assign(mysheets[i],data.frame(mydata[i]))
}

rm(mydata,mysheets)

#extract course and change completed to DateTime for each DF
Bachelors_ACFN <- Bachelors_ACFN %>%
  separate(col = "Course", 
           into = "Course", #don't care about description
           sep = " - ") %>%
  mutate(DateCompleted = lubridate::as_date(Completed)) %>%
  janitor::clean_names() %>%
  filter(is.na(abandonded)) #remove those that are abandoned. yes, this is mispelled in the data

Bachelors_BUSN <- Bachelors_BUSN %>%
  separate(col = "Course",
           into = "Course", 
           sep = " ") %>%
  mutate(DateCompleted = lubridate::as_date(Completed)) %>%
  janitor::clean_names() %>%
  filter(is.na(abandonded)) 

Bachelors_CRMJ <- Bachelors_CRMJ %>%
  mutate(DateCompleted = lubridate::as_date(Completed)) %>%
  janitor::clean_names() %>%
  filter(is.na(abandonded)) 

Masters_ACFN <- Masters_ACFN %>%
  separate(col = "Course",
           into = c("Prefix","Number"), 
           sep = " ") %>%
  mutate(DateCompleted = lubridate::as_date(Completed),
         Course = paste0(Prefix,Number)) %>%
  select(-Prefix,
         -Number) %>%
  janitor::clean_names() %>%
  filter(is.na(abandonded)) 

Masters_BUSN<- Masters_BUSN %>%
  mutate(DateCompleted = lubridate::as_date(Completed)) %>%
  janitor::clean_names() %>%
  filter(is.na(abandonded)) 

Masters_CRMJ <- Masters_CRMJ %>%
  mutate(DateCompleted = lubridate::as_date(Completed)) %>%
  janitor::clean_names() %>%
  filter(is.na(abandonded)) 

test.data <- rbind( Bachelors_ACFN %>% select(student_id,course,date_completed,program,program_subject),
                    Bachelors_BUSN %>% select(student_id,course,date_completed,program,program_subject),
                    Bachelors_CRMJ %>% select(student_id,course,date_completed,program,program_subject),
                    Masters_ACFN %>% select(student_id,course,date_completed,program,program_subject),
                    Masters_BUSN %>% select(student_id,course,date_completed,program,program_subject),
                    Masters_CRMJ %>% select(student_id,course,date_completed,program,program_subject)
) %>%
  mutate(student_id = as.numeric(student_id)) %>%
  group_by(student_id,course,program,program_subject) %>%
  summarize(date_completed = max(date_completed)) #taking the last inbound test the student took (if student retakes the course they get another exam)

unique(test.data %>% ungroup %>% select(course,program_subject,program))



student.data <- queryDB(paste0(mypath,"/Scripts/StudentData.sql"),
                        path = TRUE,
                        odbc = "APUS_DW")

student.data <- student.data %>%
  janitor::clean_names() %>%
  mutate(sesn_st_dt = lubridate::as_date(sesn_st_dt))

#give me totals per student
student.totals <- student.data %>%
  group_by(studid) %>%
  summarise(is_graduated_stud = max(is_graduated),
            courses_prog = max(course_complete_rank_prog),
            courses_stud = max(course_complete_rank_stud))

#get the latest record for each student per course
student.lastCourse <- student.data %>%
  group_by(studid,latest_crse_nbr) %>%
  summarize(sesn_st_dt = max(sesn_st_dt))

#now get the final dataframe
studentdata.master <- inner_join(student.data,student.totals, by = "studid") %>%
  inner_join(student.lastCourse, by = c("studid","latest_crse_nbr","sesn_st_dt")) %>%
  rename(student_id = "studid",
         course = "latest_crse_nbr") %>%
  mutate(student_id = as.character(student_id),
         course = as.character(course))

#now combine the totals data with each individual data frame to begin the modeling

Bachelors_ACFN <- inner_join(Bachelors_ACFN, studentdata.master, by = c("student_id","course"))
Bachelors_BUSN <- inner_join(Bachelors_BUSN, studentdata.master, by = c("student_id","course"))
Bachelors_CRMJ <- inner_join(Bachelors_CRMJ, studentdata.master, by = c("student_id","course"))
Masters_ACFN <- inner_join(Masters_ACFN, studentdata.master, by = c("student_id","course"))
Masters_BUSN <- inner_join(Masters_BUSN, studentdata.master, by = c("student_id","course"))
Masters_CRMJ <- inner_join(Masters_CRMJ, studentdata.master, by = c("student_id","course"))


#need this next function to embed histogram in a table
sparkline_hist <- function(x, breaks, type="bar"){
  as.character(
    htmltools::as.tags(
      sparkline::sparkline(
        hist(
          x,
          breaks=breaks,
          plot=FALSE
        )$density,
        type = type
      )
    )
  )
}


#create table with summarized data including histogram using formattable
coursedata.agg <- rbind(Bachelors_ACFN.course <- Bachelors_ACFN %>%
                          filter(!is.na(prog_gpa)) %>%
                          mutate("Level" = "Bachelors",
                                 "Subject" = program_subject) %>%
                          group_by(Level, Subject) %>%
                          summarize("Count" = comma(n(), digits =0),
                                    "Final Score (avg)" = round(mean(final_score), digits = 1),
                                    "GPA (avg)" = round(mean(prog_gpa), digits = 2),
                                    "Course when taken (avg)" = round(mean(course_complete_rank_prog), digits = 1),
                                    " " =sparkline_hist(
                                      course_complete_rank_prog,
                                      hist(Bachelors_ACFN$course_complete_rank_prog,plot=FALSE)$breaks
                                    ),
                                    "% Graduated" = percent(mean(is_graduated), digits = 0)
                                    
                          ),
                        
                        Bachelors_BUSN.course <- Bachelors_BUSN %>%
                          filter(!is.na(prog_gpa)) %>%
                          mutate("Level" = "Bachelors",
                                 "Subject" = program_subject) %>%
                          group_by(Level, Subject) %>%
                          summarize("Count" = comma(n(), digits =0),
                                    "Final Score (avg)" = round(mean(final_score), digits = 1),
                                    "GPA (avg)" = round(mean(prog_gpa), digits = 2),
                                    "Course when taken (avg)" = round(mean(course_complete_rank_prog), digits = 1),
                                    " "  =sparkline_hist(
                                      course_complete_rank_prog,
                                      hist(Bachelors_BUSN$course_complete_rank_prog,plot=FALSE)$breaks
                                    ),
                                    "% Graduated" = percent(mean(is_graduated), digits = 0)
                                    
                          ) %>%
                          formattable(),
                        
                        Bachelors_CRMJ.course <- Bachelors_CRMJ %>%
                          filter(!is.na(prog_gpa)) %>%
                          mutate("Level" = "Bachelors",
                                 "Subject" = program_subject) %>%
                          group_by(Level, Subject) %>%
                          summarize("Count" = comma(n(), digits =0),
                                    "Final Score (avg)" = round(mean(final_score), digits = 1),
                                    "GPA (avg)" = round(mean(prog_gpa), digits = 2),
                                    "Course when taken (avg)" = round(mean(course_complete_rank_prog), digits = 1),
                                    " "  =sparkline_hist(
                                      course_complete_rank_prog,
                                      hist(Bachelors_CRMJ$course_complete_rank_prog,plot=FALSE)$breaks
                                    ),
                                    "% Graduated" = percent(mean(is_graduated), digits = 0)
                                    
                          ),
                        
                        Masters_ACFN.course <- Masters_ACFN %>%
                          filter(!is.na(prog_gpa)) %>%
                          mutate("Level" = "Masters",
                                 "Subject" = program_subject) %>%
                          group_by(Level, Subject) %>%
                          summarize("Count" = comma(n(), digits =0),
                                    "Final Score (avg)" = round(mean(final_score), digits = 1),
                                    "GPA (avg)" = round(mean(prog_gpa), digits = 2),
                                    "Course when taken (avg)" = round(mean(course_complete_rank_prog), digits = 1),
                                    " "  =sparkline_hist(
                                      course_complete_rank_prog,
                                      hist(Masters_ACFN$course_complete_rank_prog,plot=FALSE)$breaks
                                    ),
                                    "% Graduated" = percent(mean(is_graduated), digits = 0)
                                    
                          ),
                        
                        Masters_BUSN.course <- Masters_BUSN %>%
                          filter(!is.na(prog_gpa)) %>%
                          mutate("Level" = "Masters",
                                 "Subject" = program_subject) %>%
                          group_by(Level, Subject) %>%
                          summarize("Count" = comma(n(), digits =0),
                                    "Final Score (avg)" = round(mean(final_score), digits = 1),
                                    "GPA (avg)" = round(mean(prog_gpa), digits = 2),
                                    "Course when taken (avg)" = round(mean(course_complete_rank_prog), digits = 1),
                                    " "  =sparkline_hist(
                                      course_complete_rank_prog,
                                      hist(Masters_BUSN$course_complete_rank_prog,plot=FALSE)$breaks
                                    ),
                                    "% Graduated" = percent(mean(is_graduated), digits = 0)
                                    
                          ),
                        
                        Masters_CRMJ.course <- Masters_CRMJ %>%
                          filter(!is.na(prog_gpa)) %>%
                          mutate("Level" = "Masters",
                                 "Subject" = program_subject) %>%
                          group_by(Level, Subject) %>%
                          summarize("Count" = comma(n(), digits =0),
                                    "Final Score (avg)" = round(mean(final_score), digits = 1),
                                    "GPA (avg)" = round(mean(prog_gpa), digits = 2),
                                    "Course when taken (avg)" = round(mean(course_complete_rank_prog), digits = 1),
                                    " "  =sparkline_hist(
                                      course_complete_rank_prog,
                                      hist(Masters_CRMJ$course_complete_rank_prog,plot=FALSE)$breaks
                                    ),
                                    "% Graduated" = percent(mean(is_graduated), digits = 0)
                                    
                          )
) %>%
  formattable(align =c("l","l","l","c","c", "r", "c"),
              list(
                "Level" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
                "Subject" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
                "Count" = formatter("span", style = ~ style(color = "slategrey", font.weight = "normal")),
                "Final Score (avg)" = formatter("span",
                                                style = x ~ style(display = "bar", 
                                                                  padding = "4px", 
                                                                  `color` = "white", 
                                                                  `border-radius` = "10px", 
                                                                  `background-color` = csscolor(gradient(x, min.color = "lightskyblue", max.color = "navy")))),
                "GPA (avg)" = formatter("span",
                                        style = x ~ style(display = "bar", 
                                                          padding = "4px", 
                                                          `color` = "white", 
                                                          `border-radius` = "10px", 
                                                          `background-color` = csscolor(gradient(x, min.color = "lightskyblue", max.color = "navy")))),
                
                #"Course when taken (avg)" = color_bar("gainsboro"),
                "% Graduated" = color_bar("gainsboro")
              )) %>%
  formattable::as.htmlwidget()

coursedata.agg$dependencies <- c(
  coursedata.agg$dependencies,
  htmlwidgets:::widget_dependencies("sparkline", "sparkline")
)

coursedata.agg

# #list of colors here: https://www.w3schools.com/cssref/css_colors.asp



#now, let's get only the variables that we want for each dataset

Bachelors_ACFN <- Bachelors_ACFN %>%
  select(11:29,
         course_complete_rank_prog,
         is_graduated,
         prog_gpa,
         courses_prog) %>%
  mutate(courses_after_test = courses_prog - course_complete_rank_prog) %>%
  mutate(courses_after_test_bin = case_when(courses_after_test > 0 ~ 1,
                                            TRUE ~ 0)) %>%
  na.omit()

Bachelors_BUSN <- Bachelors_BUSN %>%
  select(11:28,
         course_complete_rank_prog,
         is_graduated,
         prog_gpa,
         courses_prog) %>%
  mutate(courses_after_test = courses_prog - course_complete_rank_prog) %>%
  mutate(courses_after_test_bin = case_when(courses_after_test > 0 ~ 1,
                                            TRUE ~ 0)) %>%
  na.omit()

Bachelors_CRMJ <- Bachelors_CRMJ %>%
  select(11:20,
         course_complete_rank_prog,
         is_graduated,
         prog_gpa, 
         courses_prog) %>%
  mutate(courses_after_test = courses_prog - course_complete_rank_prog) %>%
  mutate(courses_after_test_bin = case_when(courses_after_test > 0 ~ 1,
                                            TRUE ~ 0)) %>%
  na.omit()

Masters_ACFN <- Masters_ACFN %>%
  select(10:28,
         course_complete_rank_prog,
         is_graduated,
         prog_gpa,
         courses_prog) %>%
  mutate(courses_after_test = courses_prog - course_complete_rank_prog) %>%
  mutate(courses_after_test_bin = case_when(courses_after_test > 0 ~ 1,
                                            TRUE ~ 0)) %>%
  na.omit()

Masters_BUSN <- Masters_BUSN %>%
  select(11:26,
         course_complete_rank_prog,
         is_graduated,
         prog_gpa,
         courses_prog) %>%
  mutate(courses_after_test = courses_prog - course_complete_rank_prog) %>%
  mutate(courses_after_test_bin = case_when(courses_after_test > 0 ~ 1,
                                            TRUE ~ 0)) %>%
  na.omit()

Masters_CRMJ <- Masters_CRMJ %>%
  select(11:20,
         course_complete_rank_prog,
         is_graduated,
         prog_gpa,
         courses_prog) %>%
  mutate(courses_after_test = courses_prog - course_complete_rank_prog) %>%
  mutate(courses_after_test_bin = case_when(courses_after_test > 0 ~ 1,
                                            TRUE ~ 0)) %>%
  na.omit()


#now, let's get to modeling, shall we? 
#TODO:function that allows you to run multiple linear regressions on as many dependent variables as you need and returns a dataframe as results (for formattable to show)
#I'm ashamed I'm doing it this way...


#model for coursesafter
test_coursesafter <- rbind(
  cbind(#broom::tidy(lm(courses_after_test ~ ., data = Bachelors_ACFN %>% select(1:19,24))),
    broom::glance(lm(courses_after_test ~ ., data = Bachelors_ACFN %>% select(1:18,24))),
    Bachelors_ACFN %>% count(),
    Level = "Bachelors",
    Subject = "ACFN"),
  cbind(#broom::tidy(lm(courses_after_test ~ ., data = Bachelors_BUSN %>% select(1:18,23))),
    broom::glance(lm(courses_after_test ~ ., data = Bachelors_BUSN %>% select(1:17,23))),
    Bachelors_BUSN %>% count(),
    Level = "Bachelors",
    Subject = "Business"),
  cbind(#broom::tidy(lm(courses_after_test ~ ., data = Bachelors_CRMJ %>% select(1:10,15))),
    broom::glance(lm(courses_after_test ~ ., data = Bachelors_CRMJ %>% select(1:9,15))),
    Bachelors_CRMJ %>% count(),
    Level = "Bachelors",
    Subject = "Criminal Justice"),
  cbind(#broom::tidy(lm(courses_after_test ~ ., data = Masters_ACFN %>% select(1:19,24))),
    broom::glance(lm(courses_after_test ~ ., data = Masters_ACFN %>% select(1:18,24))),
    Masters_ACFN %>% count(),
    Level = "Masters",
    Subject = "ACFN"),
  cbind(#broom::tidy(lm(courses_after_test ~ ., data = Masters_BUSN %>% select(1:16,21))),
    broom::glance(lm(courses_after_test ~ ., data = Masters_BUSN %>% select(1:15,21))),
    Masters_BUSN %>% count(),
    Level = "Masters",
    Subject = "Business"),
  cbind(#broom::tidy(lm(courses_after_test ~ ., data = Masters_CRMJ %>% select(1:10,15))),
    broom::glance(lm(courses_after_test ~ ., data = Masters_CRMJ %>% select(1:9,15))),
    Masters_CRMJ %>% count(),
    Level = "Masters",
    Subject = "Criminal Justice")
)

summary(lm(prog_gpa ~ ., data = Bachelors_ACFN %>% select(1:18,22)))

#models for gpa
test_gpa <- rbind(
  cbind(#broom::tidy(lm(prog_gpa ~ ., data = Bachelors_ACFN %>% select(1:19,22))),
    broom::glance(lm(prog_gpa ~ ., data = Bachelors_ACFN %>% select(1:18,22))),
    Bachelors_ACFN %>% count(),
    Level = "Bachelors",
    Subject = "ACFN"),
  cbind(#broom::tidy(lm(prog_gpa ~ ., data = Bachelors_BUSN %>% select(1:18,21))),
    broom::glance(lm(prog_gpa ~ ., data = Bachelors_BUSN %>% select(1:17,21))),
    Bachelors_BUSN %>% count(),
    Level = "Bachelors",
    Subject = "Business"),
  cbind(#broom::tidy(lm(prog_gpa ~ ., data = Bachelors_CRMJ %>% select(1:10,13))),
    broom::glance(lm(prog_gpa ~ ., data = Bachelors_CRMJ %>% select(1:9,13))),
    Bachelors_CRMJ %>% count(),
    Level = "Bachelors",
    Subject = "Criminal Justice"),
  cbind(#broom::tidy(lm(prog_gpa ~ ., data = Masters_ACFN %>% select(1:19,22))),
    broom::glance(lm(prog_gpa ~ ., data = Masters_ACFN %>% select(1:18,22))),
    Masters_ACFN %>% count(),
    Level = "Masters",
    Subject = "ACFN"),
  cbind(#broom::tidy(lm(prog_gpa ~ ., data = Masters_BUSN %>% select(1:16,19))),
    broom::glance(lm(prog_gpa ~ ., data = Masters_BUSN %>% select(1:15,19))),
    Masters_BUSN %>% count(),
    Level = "Masters",
    Subject = "Business"),
  cbind(#broom::tidy(lm(prog_gpa ~ ., data = Masters_CRMJ %>% select(1:10,13))),
    broom::glance(lm(prog_gpa ~ ., data = Masters_CRMJ %>% select(1:9,13))),
    Masters_CRMJ %>% count(),
    Level = "Masters",
    Subject = "Criminal Justice")
)

#models for if they ever had a course after
test_evercourseafter <- rbind(
  cbind(#broom::tidy(lm(courses_after_test_bin ~ ., data = Bachelors_ACFN %>% select(1:19,25))),
    broom::glance(lm(courses_after_test_bin ~ ., data = Bachelors_ACFN %>% select(1:18,25))),
    Bachelors_ACFN %>% count(),
    Level = "Bachelors",
    Subject = "ACFN"),
  cbind(#broom::tidy(lm(courses_after_test_bin ~ ., data = Bachelors_BUSN %>% select(1:18,24))),
    broom::glance(lm(courses_after_test_bin ~ ., data = Bachelors_BUSN %>% select(1:17,24))),
    Bachelors_BUSN %>% count(),
    Level = "Bachelors",
    Subject = "Business"),
  cbind(#broom::tidy(lm(courses_after_test_bin ~ ., data = Bachelors_CRMJ %>% select(1:10,16))),
    broom::glance(lm(courses_after_test_bin ~ ., data = Bachelors_CRMJ %>% select(1:9,16))),
    Bachelors_CRMJ %>% count(),
    Level = "Bachelors",
    Subject = "Criminal Justice"),
  cbind(#broom::tidy(lm(courses_after_test_bin ~ ., data = Masters_ACFN %>% select(1:19,25))),
    broom::glance(lm(courses_after_test_bin ~ ., data = Masters_ACFN %>% select(1:18,25))),
    Masters_ACFN %>% count(),
    Level = "Masters",
    Subject = "ACFN"),
  cbind(#broom::tidy(lm(courses_after_test_bin ~ ., data = Masters_BUSN %>% select(1:16,22))),
    broom::glance(lm(courses_after_test_bin ~ ., data = Masters_BUSN %>% select(1:15,22))),
    Masters_BUSN %>% count(),
    Level = "Masters",
    Subject = "Business"),
  cbind(#broom::tidy(lm(courses_after_test_bin ~ ., data = Masters_CRMJ %>% select(1:10,16))),
    broom::glance(lm(courses_after_test_bin ~ ., data = Masters_CRMJ %>% select(1:9,16))),
    Masters_CRMJ %>% count(),
    Level = "Masters",
    Subject = "Criminal Justice")
)


#also tested these with only final score as independent variable

test_coursesafter <- test_coursesafter %>%
  select(adj.r.squared,p.value,n,Level,Subject) %>%
  mutate(adj.r.squared = case_when(adj.r.squared < 0 ~ 0, #adjusted r squared less than 0 essentially means the predictors have no power
                                   TRUE ~ adj.r.squared),
         adj.r.squared = percent(adj.r.squared, digits = 1),
         n = comma(n, digits = 0),
         p.value = round(p.value, digits = 2),
         p.value = case_when(p.value < .05 ~ "< .05",
                             TRUE ~ as.character(p.value))) %>%
  rename("Adjusted R Squared" = adj.r.squared,
         "P Value" = p.value,
         "Count" = n) %>%
  relocate(Level,Subject,Count)

test_gpa <- test_gpa %>%
  select(adj.r.squared,p.value,n,Level,Subject) %>%
  mutate(adj.r.squared = case_when(adj.r.squared < 0 ~ 0, #adjusted r squared less than 0 essentially means the predictors have no power
                                   TRUE ~ adj.r.squared),
         adj.r.squared = percent(adj.r.squared, digits = 1),
         n = comma(n, digits = 0),
         p.value = round(p.value, digits = 2),
         p.value = case_when(p.value < .05 ~ "< .05",
                             TRUE ~ as.character(p.value))) %>%
  rename("Adjusted R Squared" = adj.r.squared,
         "P Value" = p.value,
         "Count" = n) %>%
  relocate(Level,Subject,Count)


test_evercourseafter <- test_evercourseafter %>%
  select(adj.r.squared,p.value,n,Level,Subject) %>%
  mutate(adj.r.squared = case_when(adj.r.squared < 0 ~ 0, #adjusted r squared less than 0 essentially means the predictors have no power
                                   TRUE ~ adj.r.squared),
         adj.r.squared = percent(adj.r.squared, digits = 1),
         n = comma(n, digits = 0),
         p.value = round(p.value, digits = 2),
         p.value = case_when(p.value < .05 ~ "< .05",
                             TRUE ~ as.character(p.value))) %>%
  rename("Adjusted R Squared" = adj.r.squared,
         "P Value" = p.value,
         "Count" = n) %>%
  relocate(Level,Subject,Count)


#tables for results of above models
formattable(test_coursesafter, 
            align =c("l","l","l","c","c"),
            list(
              "Level" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
              "Subject" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
              "Count" = formatter("span", style = ~ style(color = "slategrey", font.weight = "normal")),
              "Adjusted R Squared" = formatter("span",
                                               style = x ~ style(display = "bar",
                                                                 padding = "4px",
                                                                 `color` = "white",
                                                                 `border-radius` = "10px",
                                                                 `background-color` = csscolor(gradient(x, min.color = "lightskyblue", max.color = "navy")))),
              "P Value" = formatter("span", 
                                    style = x ~ style(
                                      font.weight = ifelse(x =="< .05" , "bold", "normal"), 
                                      font.style = ifelse(x =="< .05" , "italic", "normal"),
                                      color = ifelse(x < .05 , "black", ifelse(x >= .05, "slategrey", "black"))))
            ))


formattable(test_gpa, 
            align =c("l","l","l","c","c"),
            list(
              "Level" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
              "Subject" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
              "Count" = formatter("span", style = ~ style(color = "slategrey", font.weight = "normal")),
              "Adjusted R Squared" = formatter("span",
                                               style = x ~ style(display = "bar",
                                                                 padding = "4px",
                                                                 `color` = "white",
                                                                 `border-radius` = "10px",
                                                                 `background-color` = csscolor(gradient(x, min.color = "lightskyblue", max.color = "navy")))),
              "P Value" = formatter("span", 
                                    style = x ~ style(
                                      font.weight = ifelse(x =="< .05" , "bold", "normal"), 
                                      font.style = ifelse(x =="< .05" , "italic", "normal"),
                                      color = ifelse(x < .05 , "black", ifelse(x >= .05, "slategrey", "black"))))
            ))

formattable(test_evercourseafter, 
            align =c("l","l","l","c","c"),
            list(
              "Level" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
              "Subject" = formatter("span", style = ~ style(color = "slategrey", font.style = "normal")),
              "Count" = formatter("span", style = ~ style(color = "slategrey", font.weight = "normal")),
              "Adjusted R Squared" = formatter("span",
                                               style = x ~ style(display = "bar",
                                                                 padding = "4px",
                                                                 `color` = "white",
                                                                 `border-radius` = "10px",
                                                                 `background-color` = csscolor(gradient(x, min.color = "lightskyblue", max.color = "navy")))),
              "P Value" = formatter("span", 
                                    style = x ~ style(
                                      font.weight = ifelse(x =="< .05" , "bold", "normal"), 
                                      font.style = ifelse(x =="< .05" , "italic", "normal"),
                                      color = ifelse(x < .05 , "black", ifelse(x >= .05, "slategrey", "black"))))
            ))


