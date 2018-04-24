install.packages("lme4")
install.packages("sqldf")
library(lme4)
library(languageR)
library(sqldf)
library(Matrix)
load("~/Desktop/R/ws.Rdata")

cpu2006_fprate_2017_full <- read.csv("/Users/sun/Desktop/R/cpu2006_fprate_2017_full.csv", header = TRUE)

str(cpu2006_fprate_2017_full)
#change stupid name to normal
aa <- cpu2006_fprate_2017_full
aa <- ir2
#check data set
str(aa)

#dependent variable: Base
#possible predictors: EnabledCores-numeric, max_ghz-num, base_ghz-num, cache_mb-num

#cache_mb needs to be number
aa$cache_mb <- as.numeric(aa$l3)
aa$cache_mb <- NULL
#check 
str(aa) #good
summary(aa)

#play around

m1 <- lm(Base ~ EnabledCores + max_ghz + base_ghz + cache_mb, data = aa)
summary(m1)

m2 <- lm(Base ~ EnabledCores * max_ghz * base_ghz * cache_mb, data = aa)
summary(m2)

anova(m1, m2)

#max_ghz and base_ghz might have collinearity, remove max_ghz
m3 <- lm(Base ~ EnabledCores + base_ghz + cache_mb, data = aa)
summary(m3)

m4 <- lm(Base ~ EnabledCores * base_ghz * cache_mb, data = aa)
summary(m4)

anova(m3,m4)

m5 <- lm(Base ~ EnabledCores * base_ghz + cache_mb, data = aa)
summary(m5)

#remove bigger cache_mb values
aa1 <- aa[aa$cache_mb<100,]
#recheck
m3 <- lm(Base ~ EnabledCores * base_ghz + cache_mb, data = aa1)
summary(m3)

m4 <- lm(Base ~ EnabledCores * base_ghz * cache_mb, data = aa1)
summary(m4)

summary(aa1)
str(aa1)

anova(m3,m4)

anova(m4, m5)

#check collinearity between the predictors
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

#check VIF scores
vif.mer(m1)



str(aa1)
aa$cache_mb <- NULL
aa$cache_mb <- NULL

m5 <- lmer(Base ~ EnabledCores * base_ghz + cache_mb + (1|Company), data = aa1)
summary(m5)

m6 <- lmer(Base ~ EnabledCores * base_ghz + cache_mb + (1|Company), data = aa1)
summary(m6)

m7 <- lmer(Base ~ EnabledCores * ThreadsPerCore * base_ghz + cache_mb + (1|Company)+(1|System), data = aa1)
summary(m7)

m8 <- lmer(Base ~ Cores * ThreadsPerCore * base_ghz + cache_mb + (1|Company)+(1|System), data = aa1)
summary(m8)

m9 <- lmer(Base ~ Chips * Cores * bhz + l3 + (1|Company)+(1|sys), data = aa1)
summary(m9)

m10 <- lmer(Base ~ Chips * Cores * bhz + mhz + l3 + (1|Company)+(1|sys), data = aa1)
summary(m10)

m11 <- lmer(Base ~ Chips * Cores * bhz + mhz + l3 + (1|Company)+(1|sys)+(1|cpu), data = aa1)
summary(m11)

m12 <- lmer(Base ~ Cores * bhz + mhz + l3 + (1|Company)+(1|sys)+(1|cpu), data = aa1)
summary(m12)

str(aa1)


plotLMER.fnc(m12, pred = "mhz", xlabel = "mhz", ylabel = "Base", cexsize = 1.0, addlines=TRUE)
plotLMER.fnc(m12, pred = "Cores", intr=list("bhz", round(quantile(aa1$bhz),3),"end",list(c("red","yellow","green","blue","purple"), rep(1,5))), addlines = T, lwd=2, cexsize=1) 


anova(m12,m11)
#best is m10

str(aa1)

Chips<-c(2)
Cores<-c(56)
ThreadsPerCore<-c(2)
bhz<- c(2500)
mhz<-c(3800)
l3<-c(38.5)
cpu<-c("Intel Xeon Platinum 8180")
Company<-c("Lenovo Global Technology")
sys<-c("ThinkSystem SR630")
newcpu = data.frame(Chips,Cores,bhz,mhz,l3,cpu,Company,sys)
predict(m11, newdata = newcpu, allow.new.levels = TRUE)

#############################################################################################


cpu2006_fprate_2017s$Date <- as.factor(cpu2006_fprate_2017s$Date)

m1 <- lmer(Base ~ EnabledCores + (1|Date), data = cpu2006_fprate_2017s)
summary(m1)

?lmer



cpu2006_intrate_2017s_new <- sqldf("select *, substr(System,instr(System,'(')+1 ,instr(System, ')')-instr(System,'(')-1) as cpu from cpu2006_intrate_2017_full")
cpu2006_intspeed_2017s_new <- sqldf("select *, substr(System,instr(System,'(')+1 ,instr(System, ')')-instr(System,'(')-1) as cpu from cpu2006_intrate_2017s")
cpu2006_fprate_2017s_new <- sqldf("select *, substr(System,instr(System,'(')+1 ,instr(System, ')')-instr(System,'(')-1) as cpu from cpu2006_fprate_2017s")
cpu2006_fpspeed_2017s_new <- sqldf("select *, substr(System,instr(System,'(')+1 ,instr(System, ')')-instr(System,'(')-1) as cpu from cpu2006_fpspeed_2017")

rm(sql)
rm(cpu2006_intrate_2017s,cpu2006_intspeed_2017s,cpu2006_fprate_2017s,cpu2006_fpspeed_2017)

cpus <- sqldf("select distinct cpu from cpu2006_intrate_2017s_new")

cpus0 <- sqldf("select distinct cpu from cpu2006_intrate_2017s_new where instr(cpu,'Intel')<1 ")

sqldf("update cpu2006_intrate_2017s_new set cpu='AMD EPYC 7601, 2.20 GHz' where cpu='2.20 GHz, AMD EPYC 7601' ")

write.csv(cpu2006_fprate_2017s_new, file="cpu2006_fprate_2017.csv")
write.csv(cpu2006_fpspeed_2017s_new, file="cpu2006_fpspeed_2017.csv")
write.csv(cpu2006_intrate_2017s_new, file="cpu2006_intrate_2017.csv")
write.csv(cpu2006_intspeed_2017s_new, file="cpu2006_intspeed_2017.csv")

cpu2006_intrate_2017s_hz <- sqldf("select *, substr(cpu,1,instr(cpu,'G')-1) as hz from cpu2006_intrate_2017s_new")
rm(cpu2006_intrate_2017s_hz)

cpu2006_intrate_2017_new <- sqldf("select *, trim(cpu) as cpu1 from cpu2006_intrate_2017s_new")
cpu2006_intrate_2017_new$cpu <- NULL
cpu2006_intrate_2017_new$cpu <- cpu2006_intrate_2017_new$cpu1
cpu2006_intrate_2017_new$cpu1 <- NULL
cpu2006_intrate_2017 <- sqldf("
select *, case 
  when cpu='' then substr(System,instr(System,'Intel')+1)
  when substr(cpu,1,1)='I' or substr(cpu,1,1)='S' or substr(cpu,1,1)='A' or substr(cpu,1,1)=' E' then cpu
  when substr(cpu,1,1)='X' or substr(cpu,1,1)='1' or substr(cpu,1,1)='2' or substr(cpu,1,1)='3' 
    then substr(cpu,instr(cpu,'Intel'))
  else 'zzzzzzz' end cpu0
from cpu2006_intrate_2017_new")

cpu2006_intrate_2017_err <- sqldf("select * from cpu2006_intrate_2017 where cpu0 = 'zzzzzzz' ")

cpu2006_intrate_2017 = read.csv("cpu2006_intrate_2017.csv")
rm(cpu2006_intrate_2017s_new)
cpu2006_intrate_2017$cpu <- NULL
cpu2006_intrate_2017_s <- sqldf("
select *, case 
  when instr(cpu0,',')>0 then substr(cpu0,1,instr(cpu0,',')-1)
  when instr(cpu0,'.')>0 then substr(cpu0,1,instr(cpu0,'.')-2)
  else cpu0 end cpu
  from cpu2006_intrate_2017")
cpu2006_intrate_2017_s$cpu0 <- NULL
write.csv(cpu2006_intrate_2017_s, file="cpu2006_intrate_2017.csv")
cpu2006_intrate_2017 = read.csv("cpu2006_intrate_2017.csv")
cpus <- sqldf("select distinct cpu from cpu2006_intrate_2017")
write.csv(cpus, file="cpus.csv")

cpus = read.csv("cpus.csv")
cpus0 <- sqldf("select distinct trim(cpu) from cpus order by cpu")
write.csv(cpus0, file="cpus.csv")

cpu2006_intrate_2017_s <- sqldf("select *, trim(cpu) as cpu0 from cpu2006_intrate_2017")
cpu2006_intrate_2017_s$X.1<- NULL
cpu2006_intrate_2017_s$X <- NULL
cpu2006_intrate_2017_s$cpu <- NULL
cpu2006_intrate_2017_s$cpu <- cpu2006_intrate_2017_s$cpu0
cpu2006_intrate_2017_s$cpu0 <- NULL
rm(cpu2006_intrate_2017_s)
cpu2006_intrate_2017 <- cpu2006_intrate_2017_s

all_cpus_t <- sqldf("
 select 
    c.name, c.max_ghz, c.base_ghz, 
    substr(c.cache_mb,0,instr(c.cache_mb,'MB')) as cache_mb 
  from all_cpus c")
write.csv(all_cpus_t, file="all_cpus_t.csv")

all_cpus <- all_cpus_t
rm(all_cpus_t)

cpu2006_intrate_2017_full <- sqldf("
 select a.Company, a.System, a.BaseCopies, a.EnabledCores, a.EnabledChips, a.CoresPerChip, a.ThreadsPerCore, a.Base, a.Peak, a.cpu, 
   c.name, c.max_ghz, c.base_ghz, c.cache_mb
   from all_cpus c, cpu2006_intrate_2017 a 
   where c.name = a.cpu")

write.csv(cpu2006_intrate_2017_full, file="cpu2006_intrate_2017_full.csv")


cpu2006_intrate_2017_s = read.csv("cpu2006_intrate_2017.csv")
fprate_no_cpu <- sqldf(" select * from cpu2006_fprate_2017 where length(cpu0)<2 ")


cpu2006_fprate_2017 <- sqldf("
select *, case 
  when length(cpu)=0 then substr(System,instr(System,',')+1)
  when substr(cpu,1,1)='S' or substr(cpu,1,1)='A' then cpu
  when instr(cpu,'AMD')>0 then substr(cpu,instr(cpu,'AMD')-1)
  when substr(cpu,1,1)!='I' then substr(cpu,instr(cpu,'Intel')-1)
  when instr(cpu,',')>0 then substr(cpu,1,instr(cpu,',')-1)
  else cpu end cpu0
  from cpu2006_fprate_2017s_new")
rm(cpu2006_fprate_2017s_new)
cpu2006_fprate_2017$cpu <- NULL
cpu2006_fprate_2017_s <- sqldf("
select *, case
 when instr(cpu0,',')>0 then substr(cpu0,0,instr(cpu0,','))
 when instr(cpu0,'Hz')>0 then substr(cpu0,0,instr(cpu0,'Hz')-6)
 else cpu0 end cpu
 from cpu2006_fprate_2017")
cpu2006_fprate_2017_s$cpu0 <- NULL
rm(cpu2006_fprate_2017)
cpu2006_fprate_2017 <- sqldf("
select Company,System, BaseCopies, EnabledCores, EnabledChips, CoresPerChip, ThreadsPerCore, Base, Peak, trim(cpu) as cpu
 from cpu2006_fprate_2017_s")

cpu2006_fprate_2017_s <- sqldf("
select *, case
 when instr(cpu,'CPU')>0 then replace(cpu,' CPU','')
 else cpu end cpu0
 from cpu2006_fprate_2017")
cpu2006_fprate_2017_s$cpu <- NULL

cpu2006_fprate_2017_full <- sqldf("
select a.Company, a.System, a.BaseCopies, a.EnabledCores, a.EnabledChips, a.CoresPerChip, a.ThreadsPerCore, a.Base, a.Peak, a.cpu0, 
c.name, c.max_ghz, c.base_ghz, c.cache_mb
from all_cpus c, cpu2006_fprate_2017_s a
where c.name = a.cpu0")

sqldf("select * from cpu2006_fprate_2017 where cpu not in (select name from all_cpus)")

rm(cpu2006_fprate_2017,cpu2006_fprate_2017_s)

write.csv(cpu2006_fprate_2017_full, file="cpu2006_fprate_2017_full.csv")

##############################################################
cpu2006_fpspeed_2017 <- sqldf("
select *, case 
 when length(cpu)=0 then substr(System,instr(System,',')+1)
 when substr(cpu,1,1)='S' or substr(cpu,1,1)='A' then cpu
 when instr(cpu,'AMD')>0 then substr(cpu,instr(cpu,'AMD')-1)
 when substr(cpu,1,1)!='I' then substr(cpu,instr(cpu,'Intel')-1)
 when instr(cpu,',')>0 then substr(cpu,1,instr(cpu,',')-1)
 else cpu end cpu0
 from cpu2006_fpspeed_2017s_new")
cpu2006_fpspeed_2017$cpu <- NULL
cpu2006_fpspeed_2017_s <- sqldf("
 select *, case
 when instr(cpu0,',')>0 then substr(cpu0,0,instr(cpu0,','))
 when instr(cpu0,'Hz')>0 then substr(cpu0,0,instr(cpu0,'Hz')-6)
 else cpu0 end cpu
 from cpu2006_fpspeed_2017")
cpu2006_fpspeed_2017_s$cpu0 <- NULL
cpu2006_fpspeed_2017 <- sqldf("
select *, case
 when instr(cpu,'CPU')>0 then replace(cpu,' CPU','')
 else cpu end cpu0
 from cpu2006_fpspeed_2017_s")
cpu2006_fpspeed_2017$cpu <- NULL
rm(cpu2006_fpspeed_2017_s,cpu2006_fpspeed_2017s_new)
str(cpu2006_fpspeed_2017)
cpu2006_fpspeed_2017_s <- sqldf("
select Company,System, AutoParallel, EnabledCores, EnabledChips, CoresPerChip, ThreadsPerCore, Base, Peak, Date, trim(cpu0) as cpu
  from cpu2006_fpspeed_2017")
rm(cpu2006_fpspeed_2017_s)

#Company,System, , EnabledCores, EnabledChips, CoresPerChip, ThreadsPerCore, Base, Peak, Date, trim(cpu0) as cpu
cpu2006_fpspeed_2017_full <- sqldf("
select a.Company, a.System, AutoParallel, a.EnabledCores, a.EnabledChips, a.CoresPerChip, a.ThreadsPerCore, a.Base, a.Peak, Date, a.cpu, 
  c.name, c.max_ghz, c.base_ghz, c.cache_mb
  from all_cpus c, cpu2006_fpspeed_2017_s a
  where c.name = a.cpu")
sqldf("select * from cpu2006_fpspeed_2017_s where cpu not in (select name from all_cpus)")

all_cpus = read.csv("all_cpus.csv")

write.csv(cpu2006_intrate_2017_full, file="cpu2006_intrate_2017_full.csv")
write.csv(cpu2006_intspeed_2017_full, file="cpu2006_intspeed_2017_full.csv")
write.csv(cpu2006_fprate_2017_full, file="cpu2006_fprate_2017_full.csv")
write.csv(cpu2006_fpspeed_2017_full, file="cpu2006_fpspeed_2017_full.csv")

