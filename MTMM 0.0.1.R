library(tcltk2)
library(gWidgets)
library(gWidgetstcltk)
library(zipfR)
library(tm)
library(zipfR)
library(pander)
library(wordcloud)
library(tau)
library(igraph)
library(FactoMineR)
library(network)
library(FactoClass)

MTMM<-function(){

  mi<- new.env()

  ##Library
  options("guiToolkit"="tcltk")

  ##Screen
  w<- gwindow("MTMM",visible=FALSE,width = 800,height= 510)
  g<- ggroup(horizontal=FALSE, spacing=0, container = w)

  nb <- gnotebook(container=g,width = 800,height= 500)
  g1<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "MTMM")
  g2<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Descriptive general")
  g3<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Analysis of similarity")
  g4<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "AFC")
  g5<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Reinert")
  
  #GLOBAL VARIABLES
  assign("gdata",NULL,envir=mi)
  assign("gdata2",NULL,envir=mi)

  ##MENU - OPEN
  #Open csv
  abrirc<-function(h,...){
    data<-tk_choose.files()
     data1<-read.csv(data,encoding = "latin1")
     assign("gdata",data1, envir =mi)
   }

  ##View
  ver<-function(h,...){
    gdata<-get("gdata",envir =mi)
    print("The text is the following:",quote=FALSE)
    print(gdata)
  }

  ##Re-start
  inicio<-function(h,...){
    dispose(w)
    MTMM()
  }
  ##Close
  cerrar<-function(h,...){
    dispose(w)
  }

  ## Filter 
  filter<-function(h,...){
    data1<-get("gdata",envir =mi)
    text<-as.vector(data1[,1])
    text<-gsub("\u0093"," ",text)
    text<-gsub("\u0094"," ",text)
    text<-gsub("/","",text)
    text<-gsub("%","",text)
    text<-gsub(")","",text) 

    num<-as.character(c(0:9))
    lnum<-length(num)
    for(i in 1:lnum){
      text<-gsub(num[i],"",text)
    }
    text<-gsub(","," ",text)
    text<-gsub(":"," ",text)
    text<-gsub(";"," ",text)
    remover<-c("dijo","soy","a","eso","estos","además","cuál","donde","él","ante","bajo","cabe","con","contra","de","desde","durante","en","entre","hacia","hasta","mediante","para","por",
      "según","sin","so","sobre","tras","versus","vía","fue","hasta","lleva","han","cabo","parte","algún","cabo","corta","queda","lleva","estoy","ahora","así","que","pero",
      "los","hay","como","mientras","será","mas","tiene","son","solo","asi","sus","del","tal","para","por","una","con","les","esta","hemos","puede","todo","tanto","tan","mucho","día","cómo","entonces","estamos","cada","hace","antes",
      "veces","algún","ahora","esa","aunque","allá","sin","mucho","parte","este","muy","poco","manera","todos","sobre","momento","bueno","está","cuando","ellos","creo","donde","hacer",
      "porque","más","qué","las","nos","ese","seres","que","pero","los","hay","como","del","tal","para","por","una","con","les","esta","pues")
    text<- remove_stopwords(text, words = remover, lines = TRUE)

    CorpusC1 = Corpus(VectorSource(text))
    tmC1 = TermDocumentMatrix(CorpusC1, control = list(minWordLength = 1))
    mC1 = as.matrix(tmC1)
    vC1 = sort(rowSums(mC1), decreasing = TRUE)
    bC1<-data.frame(word=names(vC1), freq=vC1)
    rownames(bC1)<-c(1:nrow(bC1))
    colnames(bC1)<-c("Word","Frecuency")
    assign("gdata2",bC1, envir =mi)
  }

  #-------------------------------------------------------------------------------------------
  #Law Zipf
  zipf<-function(h,...){
    w1 <- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    g <- ggroup(container = w1)
    tbl[1,1] <- ""
    tbl[2,1] <- ""
    tbl[3,1] <- ""
    tbl[5,1] <- (a <- gbutton("help", container=tbl))
    tbl[5,2] <- (b <- gbutton("cancel", container=tbl))
    tbl[5,3] <- (c <- gbutton("ok", container=tbl))

    # Botón Descripción
    addHandlerClicked(a, handler=function(h,...) {
      w2 <- gwindow("Description",visible=FALSE,width = 400,height= 350)
      tbl <- glayout(container=w2, horizontal=FALSE)
      g <- ggroup(container = w2)
      tbl[1,1] <- "Zipf Law"
      tbl[1,2] <- ""
      tbl[1,3] <- ""
      tbl[3,1] <- "It was developed by Professor George K. Zipf in 1932,
      this law also denoted as the principle of least effort, where 
      rpf = c where p is the slope of the data vector,r the vector 
      of words, f the frequency and c is defined by a constant,
      this empirical law determines the distribution
      of word frequencies of texts"
      font(tbl[1,1]) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
      font(tbl[3,1]) <- list(weight="bold",size= 10,family="sans",align ="center",spacing = 5)
      visible(w2) <- TRUE
      })

    # Botón Cancelar
    addHandlerClicked(b, handler=function(h,...) {
      dispose(w1)
      })

    # Botón Ok
    addHandlerClicked(c, handler=function(h,...) {
      dispose(w1)
      ZIPF()
      })

    visible(w1) <- TRUE
  }

      #-------------------------------------------------------------------------------------------
  #WordCloud
  cloud<-function(h,...){
    w1 <- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    g <- ggroup(container = w1)
    tbl[1,1] <- ""
    tbl[2,1] <- ""
    tbl[3,1] <- ""
    tbl[5,1] <- (a <- gbutton("help", container=tbl))
    tbl[5,2] <- (b <- gbutton("cancel", container=tbl))
    tbl[5,3] <- (c <- gbutton("ok", container=tbl))

    # Botón Descripción
    addHandlerClicked(a, handler=function(h,...) {
      w2 <- gwindow("Description",visible=FALSE,width = 400,height= 350)
      tbl <- glayout(container=w2, horizontal=FALSE)
      g <- ggroup(container = w2)
      tbl[1,1] <- "Word Cloud "
      tbl[1,2] <- ""
      tbl[1,3] <- ""
      tbl[3,1] <- "This analysis is based on frequency tables criteria,
      which were shaped by a flat text vector, said tables is graphed 
      forming a set of texts, where the larger the size of the source, 
      the weight or frequency of that word is higher, the frequency 
      matrix is ​​ordered from higher to lower weight"
      font(tbl[1,1]) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
      font(tbl[3,1]) <- list(weight="bold",size= 10,family="sans",align ="center",spacing = 5)
      visible(w2) <- TRUE
      })

    # Botón Cancelar
    addHandlerClicked(b, handler=function(h,...) {
      dispose(w1)
      })

    # Botón Ok
    addHandlerClicked(c, handler=function(h,...) {
      dispose(w1)
      CLOUD()
      })

    visible(w1) <- TRUE
  }

  #Similitude
  simi<-function(h,...){
    w1 <- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    g <- ggroup(container = w1)
    tbl[1,1] <- ""
    tbl[2,1] <- ""
    tbl[3,1] <- ""
    tbl[5,1] <- (a <- gbutton("help", container=tbl))
    tbl[5,2] <- (b <- gbutton("cancel", container=tbl))
    tbl[5,3] <- (c <- gbutton("ok", container=tbl))

    # Botón Descripción
    addHandlerClicked(a, handler=function(h,...) {
      w2 <- gwindow("Description",visible=FALSE,width = 400,height= 350)
      tbl <- glayout(container=w2, horizontal=FALSE)
      g <- ggroup(container = w2)
      tbl[1,1] <- "Similitude "
      tbl[1,2] <- ""
      tbl[1,3] <- ""
      tbl[3,1] <- "Represents the structure of a corpus by means of its specificity,
      this technique based on graph theory is used to make social representations,
      with the objective of studying the proximity and the relations between the 
      elements of the set in the form of maximum trees, seeking to reduce these 
      links to a connected graphic without cycles."
      font(tbl[1,1]) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
      font(tbl[3,1]) <- list(weight="bold",size= 10,family="sans",align ="center",spacing = 5)
      visible(w2) <- TRUE
      })

    # Botón Cancelar
    addHandlerClicked(b, handler=function(h,...) {
      dispose(w1)
      })

    # Botón Ok
    addHandlerClicked(c, handler=function(h,...) {
      dispose(w1)
      SIMI()
      })

    visible(w1) <- TRUE
  }

  #AFC
  afc<-function(h,...){
    w1 <- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    g <- ggroup(container = w1)
    tbl[1,1] <- ""
    tbl[2,1] <- ""
    tbl[3,1] <- ""
    tbl[5,1] <- (a <- gbutton("help", container=tbl))
    tbl[5,2] <- (b <- gbutton("cancel", container=tbl))
    tbl[5,3] <- (c <- gbutton("ok", container=tbl))

    # Botón Descripción
    addHandlerClicked(a, handler=function(h,...) {
      w2 <- gwindow("Description",visible=FALSE,width = 400,height= 350)
      tbl <- glayout(container=w2, horizontal=FALSE)
      g <- ggroup(container = w2)
      tbl[1,1] <- "AFC "
      tbl[1,2] <- ""
      tbl[1,3] <- ""
      tbl[3,1] <- "Find the relationship of the rows and columns, which can be expressed
      symmetrically with the profiles and their similarities, you are defined by chi-square 
      distances and obtaining the profile clouds, which they are adjusted so that the rows 
      are invertible so that the projection is maximum,considering that the factorial planes
      are crossed by their profiles rows and column profiles."
      font(tbl[1,1]) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
      font(tbl[3,1]) <- list(weight="bold",size= 10,family="sans",align ="center",spacing = 5)
      visible(w2) <- TRUE
      })

    # Botón Cancelar
    addHandlerClicked(b, handler=function(h,...) {
      dispose(w1)
      })

    # Botón Ok
    addHandlerClicked(c, handler=function(h,...) {
      dispose(w1)
      AFC()
      })

    visible(w1) <- TRUE
  }

#Reinert
clust<-function(h,...){
  w1 <- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
  tbl <- glayout(container=w1, horizontal=FALSE)
  g <- ggroup(container = w1)
  tbl[1,1] <- ""
  tbl[2,1] <- ""
  tbl[3,1] <- ""
  tbl[5,1] <- (a <- gbutton("help", container=tbl))
  tbl[5,2] <- (b <- gbutton("cancel", container=tbl))
  tbl[5,3] <- (c <- gbutton("ok", container=tbl))

    # Botón Descripción
    addHandlerClicked(a, handler=function(h,...) {
      w2 <- gwindow("Description",visible=FALSE,width = 400,height= 350)
      tbl <- glayout(container=w2, horizontal=FALSE)
      g <- ggroup(container = w2)
      tbl[1,1] <- "Reinert "
      tbl[1,2] <- ""
      tbl[1,3] <- ""
      tbl[3,1] <- "It is a technique that form groups with homogeneous 
      characteristic between the elements by which they are formed, 
      but in turn they are different from other groups."
      font(tbl[1,1]) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
      font(tbl[3,1]) <- list(weight="bold",size= 10,family="sans",align ="center",spacing = 5)
      visible(w2) <- TRUE
      })

    # Botón Cancelar
    addHandlerClicked(b, handler=function(h,...) {
      dispose(w1)
      })

    # Botón Ok
    addHandlerClicked(c, handler=function(h,...) {
      dispose(w1)
      CLUST()
      })

    visible(w1) <- TRUE
  }

  #-------------------------------------------------------------------------------------------
  #Ley de Zipf
  ZIPF<-function(h,...){
    gdata<-get("gdata",envir =mi)
    gdata<-filter(gdata)
    
      # General
      zipf_law<-function(gdata){
        print("------------------------------",quote=FALSE)
        print(" ",quote=FALSE)
        print("Frecuency Words Table",quote=FALSE)
        print(" ",quote=FALSE)
        print("------------------------------",quote=FALSE)
        pandoc.table(gdata, style = "grid")


        color<-c("#FFC300","darkorange","#FF5733","#C70039","#900C3F","#E91E63","#9C27B0","#673AB7","darkorchid4","darkblue","#3F51B5","#03A9F4","#00BCD4","#009688","darkgreen","#4CAF50","#CDDC39","gray","black","#FF9999","#99FF99","#99CCFF","#9999FF","#CC99FF","#FF99CC")
        Vm<-gdata$Frecuency
        n<-sum(table(gdata$Frecuency))
        m<-seq(1:n)
        f.spc<-data.frame(cbind(m,Vm))
        dev.new()
        plot(f.spc,log="x",main="Distribucion Zipf - Mandelbrot",ylab="log(frecuency)",xlab="log(rangs)",col=color,pch=16)

      }

    ##g2
    tbl<-glayout(container=g2)
    gseparator(horizontal=TRUE, container=g2)
    outputArea <- gtext(container=g2, expand=TRUE,width = 780,height= 480)
    out <- capture.output(zipf_law(gdata))
    dispose(outputArea)
    if(length(out)>0)
    add(outputArea, out)
  }
  #-------------------------------------------------------------------------------------------

 #Nube de palabras
 CLOUD<-function(h,...){
  gdata<-get("gdata",envir =mi)
  gdata<-filter(gdata)

      # General
      word_cloud<-function(gdata){
        print("------------------------------",quote=FALSE)
        print(" ",quote=FALSE)
        print("Frecuency Words Table",quote=FALSE)
        print(" ",quote=FALSE)
        print("------------------------------",quote=FALSE)
        pandoc.table(gdata, style = "grid")

        color<-c("#FFC300","darkorange","#FF5733","#C70039","#900C3F","#E91E63","#9C27B0","#673AB7","darkorchid4","darkblue","#3F51B5","#03A9F4","#00BCD4","#009688","darkgreen","#4CAF50","#CDDC39","gray","black","#FF9999","#99FF99","#99CCFF","#9999FF","#CC99FF","#FF99CC")
        set.seed(4363)
        dev.new()
        wordcloud(gdata$Word,gdata$Frecuency,main="Word Cloud",min.freq =3,scale=c(2,.1),max.words=100,colors=brewer.pal(6,"Dark2"), rot.per=0.5,random.order=FALSE, width=20,height=8,res=300)
      }

    ##g2
    tbl<-glayout(container=g2)
    gseparator(horizontal=TRUE, container=g2)
    outputArea <- gtext(container=g2, expand=TRUE,width = 780,height= 480)
    out <- capture.output(word_cloud(gdata))
    dispose(outputArea)
    if(length(out)>0)
    add(outputArea, out)
  }
  #-------------------------------------------------------------------------------------------

  #Analisis de similitud
  SIMI<-function(h,...){
    gdata<-get("gdata",envir =mi)
    gdata<-filter(gdata)

      # General
      ana_simi<-function(gdata){
        tl<-t(gdata[,2])
        colnames(tl)<-gdata[,1]  
        nam<-colnames(tl) 
      E1 <- t(tl)%*%tl #productos escalares 
      colnames(E1)<-c(nam)
      rownames(E1)<-c(nam)   
      fmin<-5
      cmin<-5
      E1 <- E1[tl>=fmin,tl>=fmin] # Suma de cada columna mayor al mímino OJO CON ESTO
      E2 <- ifelse(E1<cmin,0,E1) # OJO CON ESTE 

      di <- diag(E2) # diagonal de la matriz
      dm <- solve(diag(di))# Generalised Inverse Solution con una diagonalizacion

      E <- dm%*%(E2)%*%dm #

      rownames(E) <- colnames(tl)[tl>=fmin]
      colnames(E) <- colnames(tl)[tl>=fmin]
      rownames(E1) <-colnames(tl)[tl>=fmin]
      colnames(E1) <-colnames(tl)[tl>=fmin]
      nombres <- colnames(tl)[tl>=fmin]

      contar.si<-function(x, n)
      {
        j <- 0
        for(i in 1:(length(x))){if(x[i]==n) j <- j+1}
        j}
#función "reemplazar.si" todos los que sean iguales
#a n dentro de un vector los reemplaza por p
reemplazar.si<-function(x, n, p)
{
  for(i in 1:(length(x))){if(x[i]==n) x[i] <- p}
  x}

  palabras=colnames(E)

  tmax=10

  m <- nrow(E)                           
  n <- (m-1)*m/2        #n es el número de parejas
#extracción de la triangular superior de E en un vector
  Evec <- E[1,2:m]      #Evec: vector que contendrá la triangular superior de E
  for(i in 2:(m-1)){
    Evec <- c(Evec,E[i,(i+1):m])
  }         #VECTOR DE LOS ELEMENTOS DE LE LA TRIANGULACIÓN

#creación de la matriz de subíndices, dos columnas que registran 
#las parejas de palabras (i, j) a la que se le asociará una entrada de Evec
ind <- c(0,0)
for(i in 1:(m-1)){
  for(j in (i+1):m){
    if(E[i,j]!=0) {
      ind <- rbind(ind, c(i, j))
    }
  }
}
ind <- as.matrix(ind[2:nrow(ind),1:2])
  Evec <- Evec[Evec!=0] #  

#creación de la nueva matriz Enuevo, en columnas: subíndices, coef. de asoc y su rango
  Enuevo <- cbind(ind,Evec,rank(Evec,ties.method=c("first"))) # se calcula el rango para poder ordenar las asociaciones
#Eord: reorganización de los coeficientes de forma decreciente
Eord <- c(0,0,0,0)
for(j in 0:(length(Evec)-1)){
  for(i in 1:length(Evec)){
    if(Enuevo[i,4]==(length(Evec)-j))
    Eord <- rbind(Eord,Enuevo[i,1:4])
    }}
    Eord <- Eord[2:(length(Evec)+1),1:4]

#clasificación
    clases <- rep(0,times=m)  # clases es el vector que identificará la clase a la cual pertenece cada palabra
  for(i in 1:length(Evec)){   1:10       # i recorrerá cada pareja dentro de Eord
    if(Eord[i,3]!=0){     # No nula primero se asegura que la sociación entre dos palabras no es nula

      if(clases[Eord[i,1]]==0 && clases[Eord[i,2]]==0){       # si las dos palabras de la pareja tienen clase cero
        clases[Eord[i,1]] <- max(clases)+1                      # significa que conforma una nueva clase
        clases[Eord[i,2]] <- max(clases)+1}                     # e.d. max(clases)+1

  if(clases[Eord[i,1]]==0 && (contar.si(clases,clases[Eord[i,2]])<tmax)){  # si una de las palabras tiene clase cero 
        clases[Eord[i,1]] <- clases[Eord[i,2]]}                                # y la otra palabra pertenece a una clase

  #print(contar.si(clases,clases[Eord[i,1]]))                                                                    # cuyo tamaño es menor a tmax
      if(clases[Eord[i,2]]==0 && (contar.si(clases,clases[Eord[i,1]])<tmax)){  # entonces a la palabra de clase cero
        clases[Eord[i,2]] <- clases[Eord[i,1]]}                                # se le asigna la clase existente 

      if((contar.si(clases,clases[Eord[i,1]])+contar.si(clases,clases[Eord[i,2]]))<=tmax){          # cuando las dos palabras de la pareja ya pertenecen 
       clases <- reemplazar.si(clases,clases[Eord[i,1]],min(clases[Eord[i,1]],clases[Eord[i,2]]))  # a una clase diferente de cero y la suma del número 
        clases <- reemplazar.si(clases,clases[Eord[i,2]],min(clases[Eord[i,1]],clases[Eord[i,2]]))}  # de palabras de cada clase es menor que tmax entonces
                                                                                                    # todas las palabras quedan en una sola clase, la menor
                                                                                                  }                                                                                               
                                                                                                }

#cálculo del tamaño, la densidad y la centralidad de cada clase
  Cluster <- seq(1:(max(clases)))                      #Cluster: vector que identifica cada clase existente
  Nombre <- rep(0,times=(length(Cluster)))             #Nombre: vector que contendrá el nombre de cada clase
  size <- rep(0,times=(length(Cluster)))               #size: vector que contendrá los tamaños de las clases
  Densidad <- rep(0,times=(length(Cluster)))           #Densidad: vector que contendrá la densidad de cada clase
  Centralidad <- rep(0,times=(length(Cluster)))        #Centralidad:             "        centralidad     "
  
#-------------------------------------------------------------------------
  for(i in 1:(max(clases))){                           # i recorrerá cada clase

    pal.dentro <- seq(1:m)                             # pal.dentro: secuencia desde 1 hasta m, más adelante sólo contendrá las palabras de la clase i
    npal.dentro <- contar.si(clases,i)                 # npal.dentro: número de palabras de la clase i
    if(npal.dentro!=0){
      # la siguiente instrucción le coloca NA a las palabras que en pal.dentro no pertenecen a la clase i
      pal.dentro <- ifelse(clases==i, pal.dentro, NA)     # si la palabra no pertenece a la clase i
                                                        # se le asigna NA
      size[i] <- npal.dentro                         # se registra en size el número de palabras de la clase i

      E1 <- E[!is.na(pal.dentro),!is.na(pal.dentro)]                                # E1, submatriz de E con sólo las asociaciones de la clase i
      if(npal.dentro>1) E1 <- E1-diag(1,nrow(E1),nrow(E1))                          # para poder promediar las asociaciones, se debe quitar los 1's
      if(npal.dentro>1) Densidad[i] <- mean(ifelse(E1==0,NA,E1),na.rm=TRUE)         # de la diagonal de E1, al hacer los
                        #promedios se ignoran los NA's

      #asignación del nombre del cluster
      nom <- 0
      if(!is.null(palabras)){
        suma <- rowSums(E1, na.rm=TRUE)                       # suma: vector de sumas de relaciones internas de la clase i
        pal <- palabras[!is.na(pal.dentro)]                
        nom <- pal[suma==max(suma)]                        # nom: vector que contiene a las palabras que tienen suma de asociaciones máxima
        Nombre[i] <- nom[1]}                               # por defecto se elige la primera palabra de las que tienen la suma máxima


      E2 <- E[is.na(pal.dentro),!is.na(pal.dentro)]        # E2, submatriz de E con las asociaciones externas de E
      cen <- mean(ifelse(E2==0,NA,E2),na.rm=TRUE)          # nuevamente para calcular el promedio se ignoran los NA's
      if(!is.nan(cen)) Centralidad[i] <- cen
      else Centralidad[i] <- 0                                                                         
    }                          
  }

#-----------------------------------------------------------------------------------------------

Cluster <- Cluster[size!=0]
Densidad <- Densidad[size!=0]
Centralidad <- Centralidad[size!=0]
Nombre <- Nombre[size!=0]
size <- size[size!=0]

cluster1 <- c(1:length(Cluster))
for(i in 1:length(Cluster)) clases <- reemplazar.si(clases, Cluster[i], cluster1[i])

Cluster <- cluster1

    # MPA: lista que contiene las clases, su tamaño, su densidad y centralidad
    if(!is.null(palabras))
    MPA <- list(Clases=clases,Nombres=Nombre,Resumen=cbind(Cluster,size,Densidad,Centralidad))
    else
    MPA <- list(Clases=clases,Resumen=cbind(Cluster,size,Densidad,Centralidad))
    print("------------------------------",quote=FALSE)
    print(" ",quote=FALSE)
    print("Similarity groups",quote=FALSE)
    print(" ",quote=FALSE)
    print("------------------------------",quote=FALSE)
    print(MPA)    

    plotmpa <- function(clase, E, mpa, fpond= 6, tit= NULL, tam.fuente=1)
    {
      m <- nrow(E)
      pal.dentro <- seq(1:m)
      npal.dentro <- contar.si(mpa$Clases, clase)
      pal.dentro <- ifelse(mpa$Clases == clase, pal.dentro, NA)
      E1 <- E[!is.na(pal.dentro),!is.na(pal.dentro)]
      if(npal.dentro>1) E1 <- E1-diag(1,nrow(E1),nrow(E1))
      nom <- colnames(E)
      nomE1 <- nom[!is.na(pal.dentro)]
      rownames(E1) <- nomE1
      colnames(E1) <- nomE1
      En <- network(E1)
      color <- rep("blue", times=nrow(E1))
      color1 <- ifelse(nomE1==mpa$Nombres[clase],"red",color)
      plot(En, vertex.cex=2, vertex.label.color="gray40",vertex.sides=100,usearrows=FALSE, displaylabels =TRUE, boxed.label=FALSE, edge.lwd = fpond*E1,
        vertex.col=color1, main=tit, label.cex=tam.fuente,edge.lty =3,edge.color="gray85",edge.width=.3)

    }

    for(i in 1:length(Cluster)){ 
      dev.new()
      plotmpa(i, E, MPA) 
    }

  }

    ##g2
    tbl<-glayout(container=g3)
    gseparator(horizontal=TRUE, container=g3)
    outputArea <- gtext(container=g3, expand=TRUE,width = 780,height= 480)
    out <- capture.output(ana_simi(gdata))
    dispose(outputArea)
    if(length(out)>0)
    add(outputArea, out)
  }
#-------------------------------------------------------------------------------------------

  #AFC
  AFC<-function(h,...){
    gdata<-get("gdata",envir =mi)
    gdata<-filter(gdata)

      # General
      ana_fc<-function(gdata){
        tl<-t(gdata[,2])
        colnames(tl)<-gdata[,1]  
        nam<-colnames(tl) 
        E1 <- t(tl)%*%tl #productos escalares 
        colnames(E1)<-c(nam)
        rownames(E1)<-c(nam)   
        fmin<-5
        cmin<-5
        E1 <- E1[tl>=fmin,tl>=fmin] # Suma de cada columna mayor al mímino OJO CON ESTO
        E2 <- ifelse(E1<cmin,0,E1) # OJO CON ESTE  
        di <- diag(E2) # diagonal de la matriz
        dm <- solve(diag(di))# Generalised Inverse Solution con una diagonalizacion
        E <- dm%*%(E2)%*%dm #
        rownames(E) <- colnames(tl)[tl>=fmin]
        colnames(E) <- colnames(tl)[tl>=fmin]
        rownames(E1) <-colnames(tl)[tl>=fmin]
        colnames(E1) <-colnames(tl)[tl>=fmin]
        nombres <- colnames(tl)[tl>=fmin]
        #------------------------------------
        res.ca <- CA(E, 
        #row.sup = 15:18,  # Lignes supplémentaires
        #col.sup = 6:8,    # Colonnes supplémentaires
        graph = FALSE)
        eig.val <- res.ca$eig
        # Valeurs propres
        d<-res.ca$eig
        # Résultats pour les lignes
        res.row <- res.ca$row
        res.row$coord          # Coordonnées
        res.row$contrib        # Contributions
        res.row$cos2           # Qualité de représentation 
        # Résultats pour les colonnes
        res.col <- res.ca$col
        res.col$coord          # Coordonnées
        res.col$contrib        # Contributions to the PCs
        res.col$cos2           # Qualité de représentation 

        print("------------------------------",quote=FALSE)
        print(" ",quote=FALSE)
        print("Summary",quote=FALSE)
        print(" ",quote=FALSE)
        print("------------------------------",quote=FALSE)
        pandoc.table(d)

        print("------------------------------",quote=FALSE)
        print(" ",quote=FALSE)
        print("Dimensions per rows",quote=FALSE)
        print(" ",quote=FALSE)
        print("------------------------------",quote=FALSE)
        pandoc.table(res.row$coord)

        print("------------------------------",quote=FALSE)
        print(" ",quote=FALSE)
        print("Dimensions per columns",quote=FALSE)
        print(" ",quote=FALSE)
        print("------------------------------",quote=FALSE)
        pandoc.table(res.col$coord)
        dev.new()
        barplot(eig.val[, 2], 
          names.arg = 1:nrow(eig.val), 
          main = "Variances Explained by Dimensions (%)",
          xlab = "Principal Dimensions",
          ylab = "Percentage of variances",
          col ="steelblue")
        # Add connected line segments to the plot
        lines(x = 1:nrow(eig.val), eig.val[, 2], 
          type = "b", pch = 19, col = "red")
        dev.new()
        plot(res.ca, autoLab = "yes",cex=0.8)
      }

        ##g2
        tbl<-glayout(container=g4)
        gseparator(horizontal=TRUE, container=g4)
        outputArea <- gtext(container=g4, expand=TRUE,width = 780,height= 480)
        out <- capture.output(ana_fc(gdata))
        dispose(outputArea)
        if(length(out)>0)
        add(outputArea, out)
      }
  #-------------------------------------------------------------------------------------------
  #Reinert
  CLUST<-function(h,...){
    gdata<-get("gdata",envir =mi)
    gdata<-filter(gdata)

      # General
      ana_clust<-function(gdata){
        tl<-t(gdata[,2])
        colnames(tl)<-gdata[,1]  
        nam<-colnames(tl) 
        E1 <- t(tl)%*%tl #productos escalares 
        colnames(E1)<-c(nam)
        rownames(E1)<-c(nam)   
        fmin<-5
        cmin<-5
        E1 <- E1[tl>=fmin,tl>=fmin] # Suma de cada columna mayor al mímino OJO CON ESTO
        E2 <- ifelse(E1<cmin,0,E1) # OJO CON ESTE 
        di <- diag(E2) # diagonal de la matriz
        dm <- solve(diag(di))# Generalised Inverse Solution con una diagonalizacion

        E <- dm%*%(E2)%*%dm #
        rownames(E) <- colnames(tl)[tl>=fmin]
        colnames(E) <- colnames(tl)[tl>=fmin]
        rownames(E1) <-colnames(tl)[tl>=fmin]
        colnames(E1) <-colnames(tl)[tl>=fmin]
        nombres <- colnames(tl)[tl>=fmin]
        E<-as.data.frame(E)

        #Ward                                                                    ###
        ward.cluster <- function(dista, peso = NULL , plots = FALSE, h.clust = 2, n.indi = 25 ){


n <- as.integer(attr(dista, "Size"))        # Cantidad de elementos dados por dista
distaM <- as.matrix(dista)                  # dista como matriz


if(is.null(peso)==TRUE){ peso <- rep(1,n) } # Pesos iguales cuando (peso = NULL)

peso=peso/sum(peso)                         # ponderacion de suma 1


fw <-function(a,b){(a*b)/(a+b)}             # funcion ponderación pesos inicial de Ward

distW <- distaM^2 * outer(peso,peso,"fw")   # Matriz inicial en metodo de Ward
distW <- as.dist(distW)             # Matriz inicial en metodo de Ward tipo dist


HW    <- hclust(distW, method="ward.D", members=peso)

#-------------------

if(h.clust==1){return(HW)}

#-------------------

   Nodo <- ( 1:(n-1) ) + n    # Nodo
  Prim <- HW$merge[,1]       # Primogenito
  Benj <- HW$merge[,2]       # Benjamín
  
  
  SALIDA <- data.frame(Nodo,Prim,Benj)
  
  SALIDA[SALIDA[,2]>0,2] <- SALIDA[SALIDA[,2]>0,2] + n 
  SALIDA[SALIDA[,2]<0,2] <- abs(SALIDA[SALIDA[,2]<0,2] )  # Arreglo  Primogenito
  
  SALIDA[SALIDA[,3]>0,3] <- SALIDA[SALIDA[,3]>0,3] + n
  SALIDA[SALIDA[,3]<0,3] <- abs(SALIDA[SALIDA[,3]<0,3] )  # Arreglo  Benjamín
  
  SALIDA[,1] <- factor(SALIDA[,1])
  SALIDA[,2] <- factor(SALIDA[,2])                        # Arreglo a factores
  SALIDA[,3] <- factor(SALIDA[,3])
  
  SALIDA <- data.frame(SALIDA, Indice = HW$height )       # Agregando indice
  
  
  if(h.clust==0){return(list(HW=HW,INDICES=SALIDA))}
  if(h.clust==2){return(SALIDA)}

}
#-----------------------------------------------------------

# factoclass

FactoClass<-function( dfact, metodo,dfilu = NULL , nf = 2, nfcl = 10, k.clust = 3, 
                      scanFC = TRUE , n.max = 5000 , n.clus = 1000 ,sign = 2.0,
                      conso=TRUE , n.indi = 25,row.w = rep(1, nrow(dfact)))
{

  n <- dim(dfact)[1]

  n.act  <- deparse(substitute(dfact))  ### Tipo caracter nombre de dfact
  metodo <- deparse(substitute(metodo)) ### Tipo caracter nombre de la función
  row.w <- row.w/sum(row.w) # asegurar que los pesos suman 1
  if(metodo=="dudi.coa") call1 <- call(metodo,df = as.name(n.act), nf = nf , scannf = scanFC)
     else call1 <- call(metodo,df = as.name(n.act), nf = nf , scannf = scanFC,row.w=row.w) 
                                                                                                                                             
  DuDi1 <- eval(call1) # evaluación del llamado función dudi.*
  nf    <- DuDi1$nf

  if(scanFC==TRUE){  #### Selecciona numero de ejes para realizar el proceso de clasificación
    cat("Select the number of axes for clustering: ")
    nfcl <- as.integer(readLines(n = 1))
  }

  DuDi2 <- redo.dudi( DuDi1, newnf = nfcl ) ### objeto dudi para clasificación
  nfcl <- DuDi2$nf
            
  objetos   <- DuDi2$li  ### ejes factoriales de filas para clasificación
  pesos     <- DuDi2$lw  ### pesos de filas para clasificación
  obj.clasf <- objetos   ### elementos que entran a la clasificación


  if(n >= n.max){       
    prev.kmeans <- kmeansW(x = obj.clasf, centers = n.clus, weight = pesos)
    obj.clasf   <- prev.kmeans$centers
    pesos       <- tapply(pesos, prev.kmeans$cluster, sum)
    prev.size   <- prev.kmeans$size
  }

  dend <- ward.cluster( dista= dist(obj.clasf), peso=pesos ,h.clust = 0, n.indi = n.indi)
  
  if(scanFC == TRUE){### Selecciona numero el número de clases
    cat("Select the number of clusters: ")       
    k.clust <- as.integer(readLines(n = 1))
  }
  
  cluster1 <- cutree(dend$HW, k = k.clust) ### Clasificacion generada por WARD

  if(n >= n.max){
    d1 <- data.frame(prev = prev.kmeans$cluster, id = 1:n)
    d2 <- data.frame(prev = names(cluster1), cl2 = cutree(dend$HW, k = k.clust))
    
    dd <- merge(d1, d2, all.x = TRUE)
    dd <- dd[order(dd$id), ]
    cluster1 <- dd$cl2
  }

  ft  <- function(x){data.frame(t(x))}  # funcion transpone y convierte en data.frame
  pes <- function(x){x/sum(x)}          # funcion para convertir en peso de cada clase

###----------------------------------------------------------------------------------

  p.clust1 <- DuDi2$lw
  for (k in 1:k.clust){ p.clust1[cluster1==k] <- pes(p.clust1[cluster1==k]) } ## Pesos de los individuos 
                                                                              ## para cluster 1.                                                                            
  center1 <- lapply(by( p.clust1 * DuDi2$li , cluster1, colSums ),ft)         ## Centros de la clasificación 
  center1 <- list.to.data(center1)[-1]                                        ## generada por WARD 

 
 if(conso){    ########################################### con consolidación   
    clus.summ <- NULL 
    cluster2 <- kmeansW( x = objetos , centers = center1 , weight = pesos )$cluster    
  for(k in 1:k.clust ){
    clus.summ <- rbind( clus.summ , analisis.clus(DuDi2$li[cluster2==k,],DuDi2$lw[cluster2==k]) ) 
  }
  
  clus.summ <- rbind( clus.summ , apply(clus.summ,2,sum)  )
  clus.summ[k.clust + 1,4] = NA
  rownames(clus.summ)[k.clust + 1] <- "TOTAL"

  clus.summ1 <- NULL

    for( k in 1:k.clust ){
      clus.summ1 <- rbind( clus.summ1 , analisis.clus(DuDi2$li[cluster1==k,],DuDi2$lw[cluster1==k]) )  
    }
      
      clus.summ1 <- rbind( clus.summ1 , apply(clus.summ1,2,sum)  )
      clus.summ1[k.clust + 1,4] <- NA  
      clus.summ <- data.frame( Bef.Size       =   clus.summ1$Size     ,
                               Aft.Size       =   clus.summ$Size      ,
                               Bef.Inertia    =   clus.summ1$Inertia  ,
                               Aft.Inertia    =   clus.summ$Inertia   ,
                               Bef.Weight     =   clus.summ1$Weight   ,
                               Aft.Weight     =   clus.summ$Weight    ,
                               Bef.Dist_2     =   clus.summ1$Dist_2   ,
                               Aft.Dist_2     =   clus.summ$Dist_2)

  rownames(clus.summ)[k.clust + 1] <- "TOTAL"  
    
  } # fin consolidación                                                 

  if(!conso){########### --------------------- sin consolidación
    clus.summ1 <- NULL
               ## Tabla de comportamiento de inercia de las clases 1 y 2
      for(k in 1:k.clust){
        clus.summ1 <- rbind(clus.summ1 , analisis.clus(DuDi2$li[cluster1 == k, ], DuDi2$lw[cluster1 == k]))
      }
                                      
      clus.summ <- data.frame( Size             =   clus.summ1$Size     ,
                               Inertia          =   clus.summ1$Inertia  ,
                               Weight           =   clus.summ1$Weight   ,
                               Dist_2           =   clus.summ1$Dist_2)
                         
      clus.summ <- rbind( clus.summ1 , c( sum(clus.summ[1]) ,
                                          sum(clus.summ[2]) ,
                                          sum(clus.summ[3]) ,
                                          NA))
      rownames(clus.summ)[k.clust + 1] <- "TOTAL"
    
  }
  if (!conso) cluster2 <- cluster1             
    
  p.clust <- DuDi1$lw 
  for (k in 1:k.clust)p.clust[cluster2==k] <- pes(p.clust[cluster2==k]) 

  cor.clus <- lapply(by( p.clust * DuDi1$li , cluster2, colSums ),ft)
  cor.clus <- list.to.data(cor.clus)[-1]

  base0 <- dfact
  
  if( is.null(dfilu) == FALSE ){ 
   if(class(dfilu)!="data.frame"){ return(cat("\n\n ERROR: Illustrative Variables should be 'data.frame'\n")) }
   if(dim(dfilu)[1]!= n ){ return(cat("\n\n ERROR: Active and  Illustrative Variables 
                           should have the same number of elements\n")) }
   base0 <- data.frame(base0,dfilu) 
  }

  base0 <- Fac.Num(base0)

  carac.cont = NULL
  carac.cate = NULL
  carac.frec = NULL
  carac.fril = NULL

  if(is.null(base0$numeric)==FALSE){ carac.cont <- cluster.carac( base0$numeric, cluster2 ,"co", sign) }
  if(is.null(base0$factor )==FALSE){ carac.cate <- cluster.carac( base0$factor , cluster2 ,"ca", sign) } 
  if(is.null(base0$integer)==FALSE){ carac.frec <- cluster.carac( base0$integer , cluster2 ,"fr", sign) }
  
  if(class(DuDi1)[1] == "coa" ){
    if(is.null(dfilu)==FALSE) dfact <- data.frame(dfact,dfilu)
    carac.frec <- cluster.carac(dfact,cluster2,"fr",sign)
  }

  cluster2 <- factor(cluster2)

  SALIDA <- list( indices    = dend$INDICES,
                  valor= dend$HW,
                  kclust1=k.clust,
                  clus.summ  = clus.summ)

  return(SALIDA)

}
FC.E<-FactoClass(E,dudi.coa,nf=2,nfcl=5,k.clust=2,scanFC=FALSE)

print("------------------------------",quote=FALSE)
print(" ",quote=FALSE)
print("Summary per groups",quote=FALSE)
print(" ",quote=FALSE)
print("------------------------------",quote=FALSE)
pandoc.table(FC.E$clus.summ)

print("------------------------------",quote=FALSE)
print(" ",quote=FALSE)
print("Index per groups",quote=FALSE)
print(" ",quote=FALSE)
print("------------------------------",quote=FALSE)
pandoc.table(FC.E$indices)
        dev.new()     ### Dendograma con clasificación
        plot(FC.E$valor,las=1,sub="",xlab="",ylab="Index",main="Dendrogram")
        rect.hclust(FC.E$valor, FC.E$kclust1, border="blue")
      }
    ##g2
    tbl<-glayout(container=g5)
    gseparator(horizontal=TRUE, container=g5)
    outputArea <- gtext(container=g5, expand=TRUE,width = 780,height= 480)
    out <- capture.output(ana_clust(gdata))
    dispose(outputArea)
    if(length(out)>0)
    add(outputArea, out)
  }
  #-------------------------------------------------------------------------------------------
  # MENUS
  abrir2<-list(una=gaction("csv",handler=abrirc))
  menulistaA<-list(Open=abrir2,u2=gaction("View",handler=ver),u3=gaction("Refresh",handler=inicio),u4=gaction("Close",handler=cerrar))
  imp2<-list(u0=gaction("Zipf Law",handler=zipf),u1=gaction("Word Cloud",handler=cloud))
  menulistaZ<-list(Descriptive.general=imp2,u2=gaction("Analysis of similarity",handler=simi),u3=gaction("AFC",handler=afc),u4=gaction("Reinert",handler=clust))
  #-------------------------------------------------------------------------------------------
  ##MENU - HELP
  #Manual
  y1<- function(h,..) gmessage("http://www.uv.mx/personal/nehuerta/mtmm/",title="Link")

  menulistaY<-list(u0=gaction("Information",handler=y1))
  #-------------------------------------------------------------------------------------------
  ##MENU
  mb_list <-list(File=menulistaA,Method=menulistaZ,Help=menulistaY)
  gmenu(mb_list, container=g)
  #-------------------------------------------------------------------------------------------
  ##g1
  #Information
  tmp1 <- gframe("", container=g1, expand=TRUE,horizontal=FALSE)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                  Multivariate Text Mining Module                  ",container=tmp1)
  font(tg) <- list(weight="bold",size= 28,family="sans",align ="center",spacing = 5)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                UV                                                ",container=tmp1)
  font(tg) <- list(weight="bold",size= 24,family="sans",align ="center",spacing = 5)
  tg<-glabel("                                                                                        Universidad Veracruzana                                                                                        ",container=tmp1)
  font(tg) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  visible(w) <- TRUE
}

MTMM()
