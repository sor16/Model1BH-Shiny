library(ggplot2)
library(RCmodels)
library(Cairo)
options(shiny.usecairo=T)
suppressPackageStartupMessages(library(googleVis))


shinyServer(function(input, output) {
    
        data <- reactive({
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        
        else if (inFile$type =="text/plain"){
            qvdata=read.table(inFile$datapath,skip=3,sep="|",dec=",")
            qvdata=qvdata[,c(2:4,7)]
            qvdata[,3:4]=qvdata[,4:3]
            names(qvdata)=c("Date","Time","W","Q")
            qvdata$Time=as.character(qvdata$Time)
            qvdata$Date=as.Date(gsub("\\.","-",qvdata$Date),"%d-%m-%Y")
            if(input$checkboxA==TRUE){
                years=as.numeric(format(qvdata$Date, "%Y"))
                qvdata=qvdata[which(years<=input$slider[2] & years >= input$slider[1]),]
                 
             }
            qvdata=qvdata[with(qvdata,order(W)),]
            wq=as.matrix(qvdata[,3:4])
            
      
            
        }
        else if(inFile$type=="text/csv"){
            qvdata=read.csv(inFile$datapath,skip=16)
            qvdata=qvdata[,c(1:3,6)]
            qvdata=data.frame(lapply(qvdata, as.character), stringsAsFactors=FALSE)
            qvdata[,3:4]=apply(qvdata[,c(3,4)],2, function(x) as.numeric(gsub(",",".",x)))
            qvdata[,3:4]=qvdata[,4:3]
            names(qvdata)=c("Date","Time","W","Q")
            qvdata$Date=as.Date(gsub("\\.","-",qvdata$Date),"%d-%m-%Y")
            wq=as.matrix(qvdata[,3:4])
            
        }
        else {
            qvdata=read.table(inFile$datapath)
            names(qvdata)=c("W","Q")
            qvdata$W=100*qvdata$W
            wq=as.matrix(qvdata[,2:1])
        }
        return(list("wq"=wq,"qvdata"=qvdata))
        })
        
      ## MODEL1 ##  Begin
    
    model1 <-eventReactive(input$go,{
        if("mdl1" %in% input$checkbox2){
            if(!is.null(data())){
                wq=data()$wq
                qvdata=data()$qvdata
                withProgress(message = 'Making plot', value = 0, {
                    RC=priors("Iceland")
                    
                    RC$y=as.matrix(log(wq[,2]));
                    RC$w=0.01*wq[,1]; #to meters 
                    RC$w_tild=RC$w-min(RC$w);
                    RC$n=length(RC$y);
                    
                    
                    Dens <- function(th){ Densevalm11(th,RC)$pmin}
                    Densmin=optim(par=c(0,0),Dens,hessian=TRUE)
                    t_m=as.matrix(Densmin$par)
                    H=Densmin$hessian
                    
                    
                    l_m=as.matrix(log(RC$w_tild+exp(t_m[1,]))) 
                    
                    X_m=cbind(matrix(1,nrow(l_m),ncol(l_m)),l_m) 
                    
                    L=t(chol(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2,]))) 
                    
                    mu=solve(t(L),(solve(L,(RC$Sinvmu+t(X_m)%*%RC$y/exp(t_m[2,])))))
                    
                    v_temp=X_m%*%solve(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2,]))%*%t(X_m) 
                    
                    varappr=mean(as.matrix(diag(v_temp)+exp(t_m[2,])))
                    
                    RC$fit=X_m%*%mu
                    
                    RC$confinterval= cbind(X_m%*%mu+qnorm(0.025,0,sqrt(varappr)),X_m%*%mu+qnorm(0.975,0,sqrt(varappr))) 
                    
                    data=data.frame(W=RC$w,Q=RC$y)
                    data$l_m=l_m
                    data$fit=RC$fit
                    simdata=data.frame(l_m=seq(min(data$l_m),max(data$l_m),length.out=1000))
                    c_hat=min(data$W)-exp(t_m[1,]) 
                    simdata$Wfit = exp(simdata$l_m)+c_hat
                    simdata$fit=mu[1,]+mu[2,]*simdata$l_m
                    #simdata$fitreal=exp(mu[1,])*(simdata$Wfit-c_hat)^mu[2,]
                    data$upper=RC$confinterval[,2]
                    data$lower=RC$confinterval[,1]
                    simdata$upper=simdata$fit+qnorm(0.975,0,sqrt(varappr))
                    simdata$lower=simdata$fit+qnorm(0.025,0,sqrt(varappr))
                    
                    
                    
                    return(list("RC"=RC,"varappr"=varappr,"t_m"=t_m,"qvdata"=qvdata,"simdata"=simdata,"data"=data,"mu"=mu))
            
                })
            }
        }
    })
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    plotratingcurve1 <- reactive({
        plotlist=model1()
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        tafla=NULL
        outputlist=list()
        if(!is.null(plotlist$qvdata)) {
            data=plotlist$data
            simdata=plotlist$simdata
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(simdata)+theme_bw()+geom_point(data=data,aes(exp(Q),W))+geom_line(aes(exp(fit),Wfit))+
                    geom_line(aes(exp(lower),Wfit),linetype="dashed")+geom_line(aes(exp(upper),Wfit),linetype="dashed")+
                    ggtitle(paste("Rating curve for",input$name))+ylab("W  [m]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))+coord_cartesian(xlim = ranges$x, ylim = ranges$y)
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(simdata)+geom_line(mapping=aes(fit,l_m))+theme_bw()+geom_point(data=data,mapping=aes(Q,l_m))+geom_line(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_line(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",input$name,"(log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                data$residraun=(exp(data$Q)-exp(data$fit))
                simdata$residupper=exp(simdata$upper)-exp(simdata$fit)
                simdata$residlower=exp(simdata$lower)-exp(simdata$fit)
                rcleifraun=ggplot(simdata)+geom_point(data=data,aes(W,residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_line(aes(Wfit,residupper),linetype="dashed")+geom_line(aes(Wfit,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residual plot")+xlab("W  [m]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                data$residlog=(data$Q-data$fit)/sqrt(exp(plotlist$t_m[2,]))
                rcleiflog=ggplot(data)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot (log scale)")+xlab(expression(log(W-hat(c))))+
                    theme(plot.title = element_text(vjust=2))
                
                
                outputlist$rcleiflog=rcleiflog
            }
            
            tafla=plotlist$qvdata
            tafla$W=0.01*tafla$W
            tafla$Q=tafla$Q
            tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
            tafla$lower=as.numeric(format(round(exp(data$lower),3)))
            tafla$upper=as.numeric(format(round(exp(data$upper),3)))
            tafla$diffQ=tafla$Q-tafla$Qfit
            names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
            tafla=tafla[with(tafla,order(Date)),]
            outputlist$tafla=tafla
            
            
            return(outputlist)
        }
    })
    
    model2 <- eventReactive(input$go,{
        if("mdl2" %in% input$checkbox2){
            if(!is.null(data())){
            wq=data()$wq
            qvdata=data()$qvdata
                withProgress(message = 'Making plot', value = 0, {
                    Nit=20000
                    
                    RC=priors("Iceland")
                    
                    RC$nugget=10^-8
                    RC$mu_sb=0.5
                    RC$mu_pb=0.5
                    RC$tau_pb2=0.25^2
                    RC$s=3
                    RC$v=5
                    
                    RC$y=rbind(as.matrix(log(wq[,2])),0)
                    RC$w=as.matrix(0.01*wq[,1])
                    RC$w_tild=RC$w-min(RC$w)
                    
                    Adist1 <- Adist(sort(RC$w))
                    RC$A=Adist1$A
                    RC$dist=Adist1$dist
                    RC$n=Adist1$n
                    RC$N=Adist1$N
                    
                    RC$P=diag(nrow=5,ncol=5,6)-matrix(nrow=5,ncol=5,1)
                    RC$Sig_ab= rbind(c(RC$sig_a^2, RC$p_ab*RC$sig_a*RC$sig_b), c(RC$p_ab*RC$sig_a*RC$sig_b, RC$sig_b^2))
                    RC$mu_x=as.matrix(c(RC$mu_a,RC$mu_b, rep(0,RC$n))) #Setja i RC
                    
                    RC$B=B_splines(t(RC$w_tild)/RC$w_tild[length(RC$w_tild)])
                    RC$Z=cbind(t(rep(0,2)),t(rep(1,RC$n)))
                    
                    RC$m1=matrix(0,nrow=2,ncol=RC$n)
                    RC$m2=matrix(0,nrow=RC$n,ncol=2)
                    theta.init=rep(0,9)
                    
                    Dens = function(th) {-Densevalm22(th,RC)$p}
                    Densmin=optim(par=theta.init,Dens,method="BFGS",hessian=TRUE)
                    t_m =Densmin$par
                    H=Densmin$hessian
                    phi_b=t_m[3]
                    sig_b2=t_m[2]
                    zeta=t_m[1]
                    lambda=t_m[4:9]
                    l=log(RC$w_tild+exp(t_m[1])) #as.matrix
                    varr_m=exp(RC$B%*%lambda)
                    Sig_eps=diag(as.numeric(rbind(varr_m,0)))
                    R_Beta=(1+sqrt(5)*RC$dist/exp(phi_b)+5*RC$dist^2/(3*exp(phi_b)^2))*exp(-sqrt(5)*RC$dist/exp(phi_b))+diag(1,RC$n,RC$n)*RC$nugget
                    Sig_x=rbind(cbind(RC$Sig_ab,matrix(0,nrow=2,ncol=RC$n)),cbind(matrix(0,nrow=RC$n,ncol=2),exp(sig_b2)*R_Beta))
                    
                    X=Matrix(rbind(cbind(matrix(1,dim(l)),l,Matrix(diag(as.numeric(l)),sparse=TRUE)%*%RC$A),RC$Z),sparse=TRUE)
                    
                    
                    L=t(chol(as.matrix(X%*%Sig_x%*%t(X)+Sig_eps)))
                    
                    w=solve(L,(-RC$y+X%*%RC$mu_x))
                    mu=RC$mu_x-Sig_x%*%(t(X)%*%(solve(t(L),w)))
                    LH=t(chol(H))/0.8
                    
                    cl <- makeCluster(4)
                    # Register cluster
                    registerDoParallel(cl)
                    #Find out how many
                    
                    MCMC <- foreach(i=1:4, .combine=cbind,.export=c("Densevalm22")) %dopar% {
                      ypo=matrix(0,nrow=nrow(wq)+9,ncol=Nit)
                      
                      t_old=as.matrix(t_m)
                      Dens<-Densevalm22(t_old,RC)
                      p_old=Dens$p
                      ypo_old=Dens$ypo
                      
                      for(j in 1:Nit){
                        t_new=t_old+solve(t(LH),rnorm(9,0,1))
                        Densnew<-Densevalm22(t_new,RC)
                        ypo_new=Densnew$ypo
                        p_new=Densnew$p
                        logR=p_new-p_old
                        
                        if (logR>log(runif(1))){
                          t_old=t_new
                          p_old=p_new
                          ypo_old=ypo_new
                          
                        }
                        ypo[,j]=rbind(ypo_old,t_old)
                      }
                      
                      seq=seq(2000,Nit,5)
                      ypo=ypo[,seq]
                      
                      return(ypo)
                    }
                    quantmatrix=head(MCMC,nrow(MCMC)-9)
                    t=tail(MCMC,9)
                return(list("RC"=RC,"l"=l,"t_m"=t_m,"varr_m"=varr_m,"qvdata"=qvdata,"quantmatrix"=quantmatrix,"t"=t))
                })
            }
        }
    })
    
    plotratingcurve2 <- eventReactive(input$go,{
        plotlist=model2()
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        tafla=NULL
        outputlist=list()
        if(!is.null(plotlist$qvdata)) {
            
            data=data.frame(W=plotlist$RC$w,Q=plotlist$RC$y[1:plotlist$RC$N,])
            data$l_m=plotlist$l
            data$c_hat=rep(min(data$W)-exp(plotlist$t_m[1]),length(data$l_m))
            
            prctile=t(apply(plotlist$quantmatrix, 1, quantile, probs = c(0.025,0.5, 0.975),  na.rm = TRUE))
            data$lower=prctile[,1]
            data$fit=prctile[,2]
            data$upper=prctile[,3]
            
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(data)+theme_bw()+geom_point(aes(exp(Q),W))+geom_line(aes(exp(fit),W))+
                    geom_line(aes(exp(lower),W),linetype="dashed")+geom_line(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for",input$name))+ylab("W  [cm]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(data)+geom_line(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_line(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_line(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",input$name,"(log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                data$residraun=(exp(data$Q)-exp(data$fit))
                data$residupper=exp(data$upper)-exp(data$fit)
                data$residlower=exp(data$lower)-exp(data$fit)
                rcleifraun=ggplot(data)+geom_point(aes(W,residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_line(aes(W,residupper),linetype="dashed")+geom_line(aes(W,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residual plot")+xlab("W  [cm]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                data$residlog=(data$Q-data$fit)/sqrt(plotlist$varr_m)
                rcleiflog=ggplot(data)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot (log scale)")+xlab(expression(log(W-hat(c))))+
                    theme(plot.title = element_text(vjust=2))
                
                
                outputlist$rcleiflog=rcleiflog
            }
            
            tafla=plotlist$qvdata
            tafla$W=0.01*tafla$W
            tafla$Q=as.numeric(format(round(tafla$Q,1)))
            tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
            tafla$lower=as.numeric(format(round(exp(data$lower),3)))
            tafla$upper=as.numeric(format(round(exp(data$upper),3)))
            tafla$diffQ=tafla$Q-tafla$Qfit
            names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
            tafla=tafla[with(tafla,order(Date)),]
            outputlist$tafla=tafla
            
            
            return(outputlist)
        } 
        
        
       
        
    })


    output$callreactive <- renderPrint({
        plotlist=model1()
        plotlist2=model2()
        
    })


    
    output$plots <- renderUI({
      if(length(plotratingcurve1())!=0){
        plot_output_list <- lapply(1:(length(plotratingcurve1())-1), function(i) {
          plotname=paste("plot", i, sep="")
          hovername=paste("plot",i,"_hover",sep="")
          clickname=paste("plot",i,"_dblclick",sep="")
          brushname=paste("plot",i,"_brush",sep="")
          plotOutput(plotname,hover = hoverOpts(id = hovername ),dblclick = dblclickOpts(
          id = clickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
        })
        
        do.call(tagList, plot_output_list)
      }
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.

    
    output$hover_info <- renderPrint({
      data=model1()$qvdata
      data$W=data$W*0.01
      if(!is.null(input$plot1_hover)){
        hover=input$plot1_hover
        dist=sqrt((hover$x-data$Q)^2+(hover$y-data$W)^2)
        cat("Measurement:\n")
        if(min(dist) < 0.3)
          data[which.min(dist),]
      }
      
      
    },width=100)
    
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    

    output$plot1<-renderPlot({
        if(length(plotratingcurve1())!=0)
            plotratingcurve1()[[1]]
    },height=400,width=600)
    output$plot2<-renderPlot({
        if(length(plotratingcurve1()) >= 2)
            plotratingcurve1()[[2]]   
    },height=400,width=600)
    output$plot3<-renderPlot({
        if(length(plotratingcurve1())>=3)
            plotratingcurve1()[[3]]    
    },height=400,width=600)
    output$plot4<-renderPlot({
        if(length(plotratingcurve1())>=4)
            plotratingcurve1()[[4]]     
    },height=400,width=600)
    output$tafla <- renderGvis({
       #if(!is.null(plotratingcurve1()$tafla)){
            table=as.data.frame(plotratingcurve1()$tafla)
            gvisTable(table,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
    })
    



#Model2

output$plots2 <- renderUI({
    if(length(plotratingcurve2())!=0){
        plot_output_list <- lapply(1:(length(plotratingcurve2())-1), function(i) {
            plotname=paste("plot", 4+i, sep="")
            plotOutput(plotname)
        })
        
        do.call(tagList, plot_output_list)
    }
})

output$plot5<-renderPlot({
    if(length(plotratingcurve2())!=0)
        plotratingcurve2()[[1]]
},height=400,width=600)
output$plot6<-renderPlot({
    if(length(plotratingcurve2()) >= 2)
        plotratingcurve2()[[2]]   
},height=400,width=600)
output$plot7<-renderPlot({
    if(length(plotratingcurve2())>=3)
        plotratingcurve2()[[3]]    
},height=400,width=600)
output$plot8<-renderPlot({
    if(length(plotratingcurve2())>=4)
        plotratingcurve2()[[4]]     
},height=400,width=600)
output$tafla2 <- renderGvis({
    #if(!is.null(plotratingcurve2()$tafla)){
        table=as.data.frame(plotratingcurve2()$tafla)
        gvisTable(table,options=list(
            page='enable',
            pageSize=30,
            width=550
        ))
        
    
})

    output$downloadReport <- downloadHandler(
        filename = function() {
            filename=gsub("\\.[^.]*$","",input$file1$name)
            paste(filename, sep=".",'pdf')
        },
        content <- function(file) {
            src <- normalizePath('myreport.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            #permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'myreport.Rmd')
            
            library(rmarkdown)
            out <- render('myreport.Rmd',pdf_document())
            file.rename(out, file)
        }
       
       
    )
    
    
})





