#Movielens quiz

#Q1
nrow(edx)
ncol(edx)
#Q2
edx%>%filter(rating==0)%>%nrow()
edx%>%filter(rating==3)%>%nrow()
#Q3
edx%>%select(movieId)%>%unique()%>%nrow()
#Q4
edx%>%select(userId)%>%unique()%>%nrow()
#Q5
edx%>%filter(str_detect(.$genres,"Drama"))%>%nrow()
edx%>%filter(str_detect(.$genres,"Comedy"))%>%nrow()
edx%>%filter(str_detect(.$genres,"Thriller"))%>%nrow()
edx%>%filter(str_detect(.$genres,"Romance"))%>%nrow()
#Q6
edx%>%group_by(title)%>%summarise(n=n())%>%arrange(desc(n))%>%head()
#Q7
edx%>%group_by(rating)%>%summarise(n=n())%>%arrange(desc(n))%>%head()
#Q8
edx%>%mutate(whole = rating %%1 == 0)%>%group_by(whole)%>%summarise(n=n())
