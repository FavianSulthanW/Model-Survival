install.packages("survminer")
library(survival)
library(KMsurv)
library(memisc)
library(survminer) #buat ggsurvplot

#import data
pbc <- read.csv("D:\\Downloads\\pbc.csv") #download csvnya dari kaggle dulu
view(pbc) #liat datasetnya
dim(pbc) #cek jumlah kolom n baris
head(pbc)

#preprocessing
#cek total missing values kolom yang dianalisis
sum(is.na(pbc$stage)) ; sum(is.na(pbc$status))  ; sum(is.na(pbc$time))

 
#semisal ada missing values, bisa didelete pake ini
pbc <- pbc[!(is.na(pbc$stage)), ] ; pbc <- pbc[!(is.na(pbc$stage)), ] ; pbc <- pbc[!(is.na(pbc$stage)), ]

#convert time dari days ke years biar angkanya ga kegedean
pbc$time <- pbc$time / 365


#status transplant jadiin tersensor, karena masih hidup sampe dapet transplant
pbc$status[pbc$status == 1] <- 0
pbc$status[pbc$status == 2] <- 1
table(pbc$status)

attach(pbc)

#analisis
#pengin liat perbedaan survival experience dari tiap stage pbc
#dan pengin liat apakah ada tren dari fungsi hazard tiap stagenya

#statistika deskriptif
str(pbc)
summary(pbc)

#cek perbandingan yang hidup dan meninggal dari tiap stage
table(pbc$status,pbc$stage)

#cek boxplotnya dulu sblm uji hipotesis
boxplot(time~stage,data=pbc,xlab="Stadium penyakit PBC", ylab="Waktu survival (tahun)")

#bikin estimasi fungsi survival kaplan-meier
st.stage <- survfit(Surv(time,status)~stage,data=pbc, conf.type="log-log")
summary(st.stage)

#analisis deskriptif (grafik kaplan-meier)
ggsurvplot(st.stage, conf.int=TRUE,pval=TRUE, risk.table=TRUE,xlab="Waktu survival (tahun)",ylab = "Fungsi survival",legend.title="stadium(stage)",title="Estimasi Kaplan-Meier",risk.table.height=.35)

#uji k sampel utk ngeliat ada ga perbedaan survival experience dari tiap stage
survdiff(Surv(time,status)~stage, data=pbc)

#uji tren menggunakan regresi cox ph metode breslow utk ngeliat ada tren dari fungsi hazardnya 
summary(coxph(Surv(time,status)~factor(stage), data=pbc, method="breslow"))

#cek asumsi cox ph
ggsurvplot(st.stage,xlab="Waktu survival (tahun)", fun = "cloglog",legend.title='stadium(stage)')

# A positive regression 
# coefficient for an explanatory variable means that the hazard
# is higher, and thus the prognosis worse. Conversely, a negative
# regression coefficient implies a better prognosis for patients
# with higher values of that variable.
