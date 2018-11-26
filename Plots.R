#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------PLOTS-------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#

#Create a histogram
hist(historical_hr_churn_satisfaction$MonthlyIncome)

#Create a scatter plot

#X = Income vs Y = Number of years working 
plot(historical_hr_churn_satisfaction$MonthlyIncome, historical_hr_churn_satisfaction$TenureWorking)
# X = Income vs Y = Number of years working at the company
plot(historical_hr_churn_satisfaction$MonthlyIncome, historical_hr_churn_satisfaction$TenureCompany)

#JUNTAR 3 VARIAVEIS NUM UNICO GRAFICO
ggplot(data = historical_hr_churn_satisfaction, aes(x=MaritalStatus, fill = Churn))+ geom_bar(stat = "count",, position=position_dodge())+facet_grid(Gender ~ .) +scale_fill_manual(values=c("#062f70", "#5170a0", "#66CC99"))#+ theme_void()

ggplot(data = historical_hr_churn_satisfaction, aes(x=BalanceWork.Life, fill = Churn))+ geom_bar(stat = "count",, position=position_dodge())+facet_grid(DepartmentName ~ Churn) +scale_fill_manual(values=c("#062f70", "#5170a0", "#66CC99"))#+ theme_void()

ggplot(data = historical_hr_churn_satisfaction, aes(x=JobPerformance, fill = Churn))+ geom_bar(stat = "count",, position=position_dodge())+facet_grid(MonthlyIncome ~ Churn) +scale_fill_manual(values=c("#062f70", "#5170a0", "#66CC99"))#+ theme_void()

#TWO VARIABLE PLOT
ggplot(data = historical_hr_churn_satisfaction, aes(x=JobPerformance, fill = Churn))+ geom_bar(stat = "count",, position=position_dodge()) +scale_fill_manual(values=c("#062f70", "#5170a0", "#66CC99"))#+ theme_void()
ggplot(data = historical_hr_churn_satisfaction, aes(x=MonthlyIncome, fill = Churn))+ geom_bar(stat = "count",, position=position_dodge()) +scale_fill_manual(values=c("#062f70", "#ed502d"))#+ theme_void()

ggplot(data = historical_hr_churn_satisfaction, aes(x=BalanceWork.Life, y=Churn, fill=Churn), ann= FALSE) + 
  geom_bar(stat="identity") +
  xlab("\nBalance Work-Life") +
  ylab("Count\n") + theme_minimal()
#------------------------------------------------------ONE VARABLE PLOTs - categorical-------------------------------
#BALANCE WORK LIFE
ggplot(data = historical_hr_churn_satisfaction, aes(x=BalanceWork.Life))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica"))+
  scale_y_continuous(expand = c(0,0))
#GENDER
#ggplot(data = historical_hr_churn_satisfaction, aes(x=Gender))+ geom_bar(stat = "count",, position=position_dodge()) +scale_fill_manual(values=c("#062f70")) + theme()
ggplot(data = historical_hr_churn_satisfaction, aes(x=Gender))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica"))+
  scale_y_continuous(expand = c(0,0))
#MARITAL STATUS
ggplot(data = historical_hr_churn_satisfaction, aes(x=MaritalStatus))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#JOBTYPE
ggplot(data = historical_hr_churn_satisfaction, aes(x=JobType))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) + 
  scale_y_continuous(expand = c(0,0))
#DEPARTMENT
ggplot(data = historical_hr_churn_satisfaction, aes(x=DepartmentName))+
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4"))+
  xlab('Department') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#JOB LEVEL
ggplot(data = historical_hr_churn_satisfaction, aes(x=JobLevel))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6", "#d0e7ea"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#JobROle
ggplot(data = historical_hr_churn_satisfaction, aes(x=JobRole))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6", "#d0e7ea", "#638A8D", "#25545A", "#73A4A4", "#93C1C6"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#Education
ggplot(data = historical_hr_churn_satisfaction, aes(x=Education))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6", "#d0e7ea"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#EducationArea
ggplot(data = historical_hr_churn_satisfaction, aes(x=EducationArea))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6", "#d0e7ea", "#638A8D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica Bold") , axis.title = element_text(size=14, family = "Helvetica Bold")) +
  scale_y_continuous(expand = c(0,0))
#FacilitiesSatisfaction
ggplot(data = historical_hr_churn_satisfaction, aes(x=FacilitiesSatisfaction))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#RoleSatisfaction
ggplot(data = historical_hr_churn_satisfaction, aes(x=RoleSatisfaction))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica") , axis.title = element_text(size=14, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#hierarchySatisfaction
ggplot(data = historical_hr_churn_satisfaction, aes(x=HierarchySatisfaction))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A", "#73A4A4", "#93C1C6"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica Bold") , axis.title = element_text(size=14, family = "Helvetica Bold")) +
  scale_y_continuous(expand = c(0,0))
#Churn
ggplot(data = historical_hr_churn_satisfaction, aes(x=Churn))+ 
  geom_bar(stat = "count", position=position_dodge(), fill= c("#638A8D", "#25545A"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size=14, family = "Helvetica Bold") , axis.title = element_text(size=14, family = "Helvetica Bold")) +
  scale_y_continuous(expand = c(0,0))

#FAZER GRAFICO DE NUVEM COM PONTINHOS
ggplot(data = historical_hr_churn_satisfaction) + aes(x=MonthlyIncome , y =DistanceHomeOffice ) + geom_point()  #OU + scale_fill_brewer(palette="Spectral")

#------------------------------------------------------ONE VARABLE PLOTs - NUMERICAL-------------------------------
hist(historical_hr_churn_satisfaction$MonthlyIncome, main = "Distribution of Monthly Income", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$Dependents, main = "Distribution of Nr. of Dependents", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$NumCompaniesWorked, main = "Distribution of Nr. of Companies Worked", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$NumberProjectsLastYear, main = "Distribution of Nr. of Projects Last Year", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$TenureWorking, main = "Distribution of Tenure Working", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$TenureCompany, main = "Distribution of Tenure Company", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$TenureRole, main = "Distribution of Tenure Role", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$TenureManager, main = "Distribution of Tenure Manager", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$DistanceHomeOffice, main = "Distribution of Distance Home-Office", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$`SalaryRise(%)`, main = "Distribution of Salary Rise", xlab='', col =c("#93C1C6"))
hist(historical_hr_churn_satisfaction$LastPromotion, main = "Distribution of Last Promotion", xlab='', col =c("#93C1C6"))



#CHURN BY GENDER
counts <- table(historical_hr_churn_satisfaction$Churn, historical_hr_churn_satisfaction$Gender)
ggplot(historical_hr_churn_satisfaction, aes(x=Gender, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Gender") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0)) 
#By facilities satisfaction
ggplot(historical_hr_churn_satisfaction, aes(x=FacilitiesSatisfaction, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Facilities Satisfaction") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By role satisfaction
ggplot(historical_hr_churn_satisfaction, aes(x=RoleSatisfaction, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Role Satisfaction") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By hierarchy satisfaction
ggplot(historical_hr_churn_satisfaction, aes(x=HierarchySatisfaction, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Hierarchy Satisfaction") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By job level
ggplot(historical_hr_churn_satisfaction, aes(x=JobLevel, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Job Level") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By department
ggplot(historical_hr_churn_satisfaction, aes(x=DepartmentName, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Department") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By Educaton
ggplot(historical_hr_churn_satisfaction, aes(x=Education, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Education") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By job dedication
ggplot(historical_hr_churn_satisfaction, aes(x=JobDedication, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Job Dedication") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By balance work life
ggplot(historical_hr_churn_satisfaction, aes(x=JobLevel, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Job Level") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#By balance work life
ggplot(historical_hr_churn_satisfaction, aes(x=BalanceWork.Life, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Balance Work-Life") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#TenureManager
ggplot(historical_hr_churn_satisfaction, aes(x=TenureManager, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Tenure Manager") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))

#By AfterHours
ggplot(historical_hr_churn_satisfaction, aes(x=AfterHours, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("After Hours") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))

#By total satisfaction
ggplot(historical_hr_churn_satisfaction, aes(x=totalSatisfaction, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Total Satisfaction") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))

#By percentage of years
ggplot(historical_hr_churn_satisfaction, aes(x=totalJobRewarding, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Total Job Rewarding") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))

#By percentage of years
ggplot(historical_hr_churn_satisfaction, aes(x=NewEducation, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Education") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))


ggplot(historical_hr_churn_satisfaction, aes(x=YearPerc, fill=Churn)) +
  geom_histogram(binwidth=.05, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))




# Overlaid histograms

#in curves
ggplot(historical_hr_churn_satisfaction, aes(x=YearPerc, fill=Churn)) + 
  geom_density(alpha=0.2)


#monthly income 
ggplot(historical_hr_churn_satisfaction, aes(MonthlyIncome, fill = Churn)) + geom_histogram(alpha = 0.8, aes(y = ..density..), position = 'identity') + 
  scale_fill_manual(values=c( "#9ca1a3", "#7bc9d1"))

ggplot(historical_hr_churn_satisfaction, aes(x=MonthlyIncome, fill=Churn)) +
  geom_histogram(binwidth=80, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))

ggplot(historical_hr_churn_satisfaction,aes(x=MonthlyIncome)) +
  geom_histogram(data=subset(historical_hr_churn_satisfaction,yy == 'a'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(dat,yy == 'b'),fill = "blue", alpha = 0.2)

#NrProjsKLAstYear
ggplot(historical_hr_churn_satisfaction, aes(x=NumberProjectsLastYear, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Number of Projects Last Year") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#TenureWorking
ggplot(historical_hr_churn_satisfaction, aes(x=NewTenureWorking, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Tenure Working") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#DistanceHomeOffice
ggplot(historical_hr_churn_satisfaction, aes(x=NewDistanceHomeOffice, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Distance Home Office") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))

#tenureCompany
ggplot(historical_hr_churn_satisfaction, aes(x=NewTenureCompany, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Tenure Company") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))

ggplot(historical_hr_churn_satisfaction, aes(x=NewTenureCompany)) + ggtitle("Tenure Company") + xlab("Tenure Company") +
  geom_bar(fill="#25545A", aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

ggplot(historical_hr_churn_satisfaction, aes(x=NewTenureWorking)) + ggtitle("Tenure Working") + xlab("Tenure Working") +
  geom_bar(fill="#25545A", aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()


#Monthly Income
ggplot(historical_hr_churn_satisfaction, aes(x=NewMonthlyIncome, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Monthly Income") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))
#SalaryRise
ggplot(historical_hr_churn_satisfaction, aes(x=NewSalaryRise, fill = Churn)) +
  geom_bar(width = 0.5) +
  xlab("Salary Rise (%)") + 
  ylab("Total Count") + 
  labs(fill = "Churn") + 
  scale_fill_manual(values=c( "#25545A", "#73A4A4")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=10, family = "Helvetica")) +
  scale_y_continuous(expand = c(0,0))

counts1 <- table(historical_hr_churn_satisfaction$Churn, historical_hr_churn_satisfaction$MaritalStatus)
barplot(counts1, main="Churn by Gender",
        xlab="Gender", col=c("#92C29C","#7C756A"),
        legend = rownames(counts1))
#BY BALANCE WORK LIFE
counts1 <- table(historical_hr_churn_satisfaction$Churn, historical_hr_churn_satisfaction$BalanceWork.Life)
barplot(counts1, main="Churn by Gender",
        xlab="Gender", col=c("#062f70","#5170a0"),
        legend = rownames(counts1))

#Create a bagplot
library(aplpack) # NOT IMPORTING !!!!!
bagplot(cbind(raw_data$Grade1,raw_data$Grade2),cex=2)
attach(historical_hr_churn_satisfaction)
table(NewChurn)
#-------------------------------------------------------------------------------------------------------#
#------------------------------------------ OVERLAPPING PLOTS WITH CHURN----------------------------#
#-------------------------------------------------------------------------------------------------------#
# Overlaid histograms
#INCOME
ggplot(historical_hr_churn_satisfaction, aes(x=MonthlyIncome, fill=Churn)) +
  geom_histogram(binwidth=80, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
#tENURE COMPANY
ggplot(historical_hr_churn_satisfaction, aes(x=TenureCompany, fill=Churn)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
#tENURE working
ggplot(historical_hr_churn_satisfaction, aes(x=TenureWorking, fill=Churn)) +
  geom_histogram(binwidth=2, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
#tENURE manager
ggplot(historical_hr_churn_satisfaction, aes(x=TenureManager, fill=Churn)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
#Num projs last year
ggplot(historical_hr_churn_satisfaction, aes(x=NumberProjectsLastYear, fill=Churn)) +
  geom_histogram(binwidth=.95, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
theme_minimal()
#salary rise
ggplot(historical_hr_churn_satisfaction, aes(x=DistanceHomeOffice, fill=Churn)) +
  geom_histogram(binwidth=2, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
#tenure role
ggplot(historical_hr_churn_satisfaction, aes(x=TenureRole, fill=Churn)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
#last promo
ggplot(historical_hr_churn_satisfaction, aes(x=LastPromotion, fill=Churn)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()
#dependents
ggplot(historical_hr_churn_satisfaction, aes(x=NumCompaniesWorked, fill=Churn)) +
  geom_histogram(binwidth=.95, alpha=.5, position="identity") + scale_fill_manual(values=c( "#73A4A4", "#25545A"))+
  ylab("Count")+
  theme_minimal()

#-------------------------------------------------------------------------------------------------------#
#------------------------------------------PLOT --SPLIT DATAFRAMES PER CHURN----------------------------#
#-------------------------------------------------------------------------------------------------------#
historical_hr_churn_satisfaction$NewChurn <- as.numeric(historical_hr_churn_satisfaction$NewChurn)
levels(historical_hr_churn_satisfaction$NewChurn)
plot(historical_hr_churn_satisfaction$NewChurn)
df_split_churn <- split(historical_hr_churn_satisfaction, historical_hr_churn_satisfaction$NewChurn)

hist(df_split_churn[["0"]]$MonthlyIncome, main = "Distribution of Monthly Income (By No Churn)", col =c("#93C1C6"))
hist(x, main="Distribution of Income (without Outliers)", xlab="", col = c("#93C1C6"))
hist(df_split_churn[["0"]]$Dependents, main = "Distribution of Dependents (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$NumCompaniesWorked, main = "Distribution of Number of Companies Worked (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$`SalaryRise(%)`, main = "Distribution of Salary Rise (By No Churn)", col =c("#93C1C6"))
#hist(df_split_churn[["0"]]$WeekHours, main = "Distribution of Week Hours (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$TenureWorking, main = "Distribution of Tenure Working (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$NumberProjectsLastYear, main = "Distribution of Nr. of Projects Last Year (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$TenureCompany, main = "Distribution of Tenure Company (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$TenureRole, main = "Distribution of Tenure Role (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$LastPromotion, main = "Distribution of Last Promotion (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$TenureManager, main = "Distribution of Tenure Manager (By No Churn)", col =c("#93C1C6"))
hist(df_split_churn[["0"]]$DistanceHomeOffice, main = "Distribution of Distance Home-Office (By No Churn)", col =c("#93C1C6"))
summary(df_split_churn[["0"]])

table(df_split_churn[["0"]]$JobDedication)


hist(df_split_churn[["1"]]$MonthlyIncome, main = "Distribution of Monthly Income (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$Dependents, main = "Distribution of Dependents (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$NumCompaniesWorked, main = "Distribution of Nr. of Companies Worked (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$`SalaryRise(%)`, main = "Distribution of Salary Rise (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$TenureWorking, main = "Distribution of Tenure Working (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$NumberProjectsLastYear, main = "Distribution of Nr. of Projects Last Year (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$TenureCompany, main = "Distribution of Tenure Company (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$TenureRole, main = "Distribution of Tenure Role (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$LastPromotion, main = "Distribution of Last Promotion (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$TenureManager, main = "Distribution of Tenure Manager (By Churn)", col =c("#25545A"))
hist(df_split_churn[["1"]]$DistanceHomeOffice, main = "Distribution of Distance Home-Office (By Churn)", col =c("#25545A"))
summary(df_split_churn[["1"]])

table(df_split_churn[["1"]]$JobDedication)
#-------------------------------------------------------------------------------------------------------#
#------------------------------------------PLOTS CROSSING CHURN----------------------------#
#-------------------------------------------------------------------------------------------------------#

#BY CATEGORICAL VARIABLES
ggplot(historical_hr_churn_satisfaction, aes(BalanceWork.Life, fill = NewChurn)) + geom_bar(position = "fill") + labs(x = "Area code", y = "") + theme(legend.position = "none")
#BY CONTINUOUS VARIABLES
ggplot(historical_hr_churn_satisfaction, aes(MonthlyIncome,NewChurn, fill=NewChurn )) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")


#--------------------------------------CORRELATION MATRIX---------------------------------------------------

new_historical_df <- historical_hr_churn_satisfaction

new_historical_df$AfterHours <- NULL
new_historical_df$Churn <- NULL
new_historical_df$BalanceWork.Life <- NULL
new_historical_df$EducationArea <- NULL
new_historical_df$Education <- NULL
new_historical_df$TypeContract <- NULL
new_historical_df$JobRole <- NULL
new_historical_df$Department <- NULL
new_historical_df$JobType <- NULL
new_historical_df$MaritalStatus <- NULL
new_historical_df$Gender <- NULL
new_historical_df$BirthDate <- NULL
new_historical_df$`Employee ID` <- NULL
new_historical_df$DepartmentName <- NULL
new_historical_df$WeekHours <- NULL
new_historical_df$NewChurn <- NULL
new_historical_df$YearPerc <- NULL


new_historical_df$Churn <- historical_hr_churn_satisfaction$NewChurn
new_historical_df$Churn <- NULL


res <- cor(historical_hr_churn_satisfaction$DistanceHomeOffice, historical_hr_churn_satisfaction$JobDedication, historical_hr_churn_satisfaction$JobPerformance)
install.packages("Hmisc")

library("Hmisc")
res2 <- rcorr(as.matrix(historical_hr_churn_satisfaction$DistanceHomeOffice, historical_hr_churn_satisfaction$JobDedication, historical_hr_churn_satisfaction$JobPerformance))
res2
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

dim(new_historical_df)
str(new_historical_df)
new_historical_df$NewAfterHours <- as.numeric(as.character(new_historical_df$NewAfterHours))
new_historical_df$RoleSatisfaction <- as.numeric(as.character(new_historical_df$RoleSatisfaction))

numerical_df$NewChurn <- as.numeric(as.character(numerical_df$NewChurn))
head(new_historical_df)
str(numerical_df)

M <- cor(new_historical_df)
M <- round(cor(M), 2)

corrplot(M, method="circle")
corrplot(M, method="color")
corrplot(M, method="number")

library(reshape2)
melted_cormat <- melt(M)
head(melted_cormat)
#not working
library(ggplot2)

ggplot(date = melted_cormat, aes(x = Var1, y=Var2, fill = value)) + geom_tile()
