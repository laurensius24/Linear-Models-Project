library(psych)
library(DescTools)
library(moments)
library(fastDummies)

#impor data
df<-read.csv('C:/Users/ASUS/Documents/R/Life Expectancy Data.csv')
#objektif: memprediksi Lefe expectancy

#eksplorasi data
plot(df) 
#tapi ini gaakan render, terlalu banyak kolom
View(df)
str(df)
#data mencakup observasi dari 183 negara selama 7 tahun (2001-2015)
#terhadap 20 variabel observasi

#agar data tidak mengandung runtun waktu, hanya akan dipakai data tahun
#terakhir, yaitu thn 2015
df1<-df[which(df$Year==2015),]
df1 = subset(df1, select = -c(Country,Year))

str(df1)
View(df1)

#menghapus kolom dengan nilai kosong
nonempty<-colSums(is.na(df1) | df1 == "")>0
df2<-df1[,!nonempty]

View(df2)
str(df2)

#pemilihan variabel
mod0<-lm(Life.expectancy~1,data=df2)
modn<-lm(Life.expectancy~.,data=df2)

mod1<-step(mod0, direction = 'forward',scope=formula(modn),trace=1)
summary(mod1)
VIF(mod1)

#membuat dummy var kategori
df3<-dummy_cols(df2,select_columns = 'Status')
df3<-subset(df3,select=-c(Status,Status_Developing))

#dataframe yang digunakan di mod1:
df4<-subset(df3,select = c(Adult.Mortality, Diphtheria, Status_Developed,
                           HIV.AIDS, Polio, under.five.deaths,
                           infant.deaths, Life.expectancy))

corPlot(df4)
#Terlihat bahwa kolom under.five.deaths dan infant.deaths berkorelasi tinggi

VIF(mod1)
#Terlihat nilai VIF under.five.deaths dan infant.deaths sangat tinggi
#akan dibuang kolom under.five.deaths

df5<-subset(df4,select=-under.five.deaths)
corPlot(df5)

mod2<-lm(Life.expectancy~.,data=df5)
summary(mod2)
#hampir semua variabel signifikan, Pvalue infant.deaths mendekati 0.05
VIF(mod2)
#seluruh nilai VIF dibawah 4 (tidak ada multiko)

#pemilihan model interaksi
modm<-lm(Life.expectancy~.^2,data=df5)
mod3<-step(mod2, direction = 'forward',scope=formula(modm),trace=1)

summary(mod3)
#variabel Diphteria tidak signifikan, namun interaksinya dengan var lain signifikan
#ada beberapa variabel interaksi Adult.Mortality:Polio tidak signifikan

#model tanpa interaksi Adult.Mortality:Polio
modp<-lm(Life.expectancy~.+Adult.Mortality:Diphtheria+Adult.Mortality:HIV.AIDS+
           Diphtheria:Polio,data=df5)

anova(modp,mod3)
#hasil pengujian perbandingan model tidak signifikan, sehingga model dengan
#interaksi Adult.Mortality:Polio lebih baik.


#analisis residual
#cheching asumsi heterosedasdisitas

plot(mod3,1)
#secara garis besar, variansi residual cenderung konstan

#cheching outlier

st<-rstandard(mod3)
st<-st[which(st>2|st< -2)]
#didapat ada 10 nilai outlier. namun mengingat setiap observasi adalah
#data dari sebuah negara yang sangat mungkin jika kondisinya sangat bervariasi
#maka outlier masih dapat ditoleransi

#cek asumsi normalitas

ks.test(rstandard(mod3),'pnorm')
#melalui tes koglomorov smirnov, didapat 
#H0: error berdistribusi normal tidak ditolak 

#analisis multikolinearitas

VIF(mod3)
#terlihat nilai VIF dibawah 4 untuk variabel yang tidak terlibat dalam
#term interaksi