library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

setwd(".")
df<- read.csv("../data/coursera_data.csv")

numerify_nos<-function(course_students_enrolled){
    if(grepl("k",course_students_enrolled)){
        return(as.double(strsplit(course_students_enrolled,"k")[[1]][1])*1000)
    }
    else if(grepl("m",course_students_enrolled)){
        return(as.double(strsplit(course_students_enrolled,"m")[[1]][1])*1000000)
    }}

no_of_students<-apply(df["course_students_enrolled"], 1, numerify_nos)
df$no_of_students<-c(no_of_students)
drops<-c("course_students_enrolled","X")
df2<-df[,!(names(df) %in% drops)]
df3<-arrange(df2,no_of_students) # Sort by no_of_students

# df_sum_students<-rowsum(df3$no_of_students,df3$course_organization)

# Count by University 
df_sum_university<- df3 %>%
    group_by(course_organization) %>%
    summarise(no_of_students=sum(no_of_students))


df_sum_university<-arrange(df_sum_university,desc(no_of_students))
df_sum_university<- head(df_sum_university,n = 20)

co <- ggplot(df_sum_university,aes(x=reorder(course_organization,-no_of_students),y=no_of_students)) + geom_bar(stat = "identity", fill = "#0000FF", width = 0.5) + scale_x_discrete(name="Organization name",labels = function(x) str_wrap(x, width = 10))+scale_y_continuous(name="Number of students", labels = scales::comma)

ggplotly(co)


# Count by course_Certificate_type
df_sum_course_type_students<- df3 %>%
    group_by(course_Certificate_type) %>%
    summarise(no_of_students=sum(no_of_students))

cc <- ggplot(df_sum_course_type_students,aes(x=reorder(course_Certificate_type,-no_of_students),y=no_of_students)) + geom_bar(stat = "identity", fill = "#0000FF", width = 0.5) + scale_x_discrete(name="Course Certificate Type",labels = function(x) str_wrap(x, width = 10))+scale_y_continuous(name="Number of students", labels = scales::comma)

ggplotly(cc)

# Count by course_rating
df_course_rating <- df3 %>%
    group_by(course_rating) %>%
    summarise(no_of_students=sum(no_of_students))

cr <- ggplot(df_course_rating,aes(x=reorder(course_rating,-no_of_students),y=no_of_students)) + geom_bar(stat = "identity", fill = "#0000FF", width = 0.5) + scale_x_discrete(name="Course Rating",labels = function(x) str_wrap(x, width = 10))+scale_y_continuous(name="Number of students", labels = scales::comma)

ggplotly(cr)

# Count by course difficulty
df_course_difficulty <- df3 %>%
    group_by(course_difficulty) %>%
    summarise(no_of_students=sum(no_of_students))

cd<-ggplot(df_course_difficulty,aes(x=reorder(course_difficulty,-no_of_students),y=no_of_students)) + geom_bar(stat = "identity", fill = "#0000FF", width = 0.5) + scale_x_discrete(name="Course Difficulty",labels = function(x) str_wrap(x, width = 10))+scale_y_continuous(name="Number of students", labels = scales::comma)

ggplotly(cd)

# Count by Title 
df_course_title <- df3 %>%
    group_by(course_title) %>%
    summarise(no_of_students=sum(no_of_students))

df_course_title<-arrange(df_course_title,desc(no_of_students))
df_course_title<- head(df_course_title,n = 20)

ct <- ggplot(df_course_title,aes(x=reorder(course_title,-no_of_students),y=no_of_students)) + geom_bar(stat = "identity", fill = "#0000FF", width = 0.5) + scale_x_discrete(name="Course Title",labels = function(x) str_wrap(x, width = 10))+scale_y_continuous(name="Number of students", labels = scales::comma)

ggplotly(ct)


