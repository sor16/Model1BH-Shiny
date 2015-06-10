library(shiny)
library(ggplot2)
library(RCmodels)
library(googleVis)
shinyServer(function(input, output) {
    
    model1<-reactive({
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        
        else if (inFile$type =="text/plain"){
            qvdata=read.table(inFile$datapath,skip=3,sep="|")
            qvdata=qvdata[,c(2:4,7)]
            qvdata=data.frame(lapply(qvdata, as.character), stringsAsFactors=FALSE)
            qvdata[,3:4]=apply(qvdata[,3:4],2, function(x) as.numeric(gsub(",",".",x)))
            qvdata[,3:4]=qvdata[,4:3]
            names(qvdata)=c("Date","Time","W","Q")
            qvdata$Date=as.Date(gsub("\\.","-",qvdata$Date),"%d-%m-%Y")
            wq=as.matrix(qvdata[,3:4])
            
        }
        else if(inFile$type=="text/csv"){
            qvdata=read.table(inFile$datapath,skip=16)
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
      ## MODEL1 ##  Begin
      withProgress(message = 'Making plot', value = 0, {
      
    
      RC=priors(input$select)
      
      RC$y=as.matrix(log(wq[,2]));
      RC$w=0.01*wq[,1]; #to meters 
      RC$w_tild=RC$w-min(RC$w);
      RC$n=length(RC$y);
      
      
      Dens <- function(th){ Densevalm11(th,RC)$pmin}
      Densmin=optim(par=c(0,0),Dens,hessian=TRUE)
      t_m=as.matrix(Densmin$par)
      H=Densmin$hessian
      

      l_m=as.matrix(log(RC$w_tild+exp(t_m[1,]))); #samanburdur stodst
      
      X_m=cbind(matrix(1,nrow(l_m),ncol(l_m)),l_m); #samanburdur stodst
      
      L=t(chol(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2,]))); #samanburdur stodst
      
      mu=solve(t(L),(solve(L,(RC$Sinvmu+t(X_m)%*%RC$y/exp(t_m[2,]))))); #samanburdur stodst
      
      RC$fit=X_m%*%mu
      
      
      v_temp=X_m%*%solve(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2,]))%*%t(X_m) #samanburdur stodst
      
      
      varappr=as.matrix(diag(v_temp)+exp(t_m[2,])); #samanburdur stodst
      
      
      confinterval= cbind(X_m%*%mu+qnorm(0.025,0,sqrt(varappr)),X_m%*%mu+qnorm(0.975,0,sqrt(varappr))) #samanburdur stodst
      
      LH=t(chol(H))/(2.38/sqrt(2)) #Hvadan kemur thessi tala?? 2.38
      
      
      Nit=20000
      
      t1=matrix(0,4,Nit)
      t2=matrix(0,4,Nit)
      t3=matrix(0,4,Nit)
      t4=matrix(0,4,Nit)
      
      
      
      for(j in 1:4){
          incProgress(1/4, detail = paste("Doing part", j))
          
          t_old=t_m
          t=matrix(0,nrow=4,ncol=Nit)
          yp=matrix(0,nrow=nrow(wq),ncol=Nit)
          ypo=matrix(0,nrow=nrow(wq),ncol=Nit)
          
          D=c()
          
          
          Dens<-Densevalm11(t_old,RC)
          p_old=Dens$p
          x_old=Dens$x
          yp_old=Dens$yp
          ypo_old=Dens$ypo
          D_old=Dens$D
          
          for(i in 1:Nit){
              
              t_new=t_old+solve(t(LH),as.matrix(rnorm(2,0,1)))
              
              Densnew<-Densevalm11(t_new,RC)
              p_new=Densnew$p
              x_new=Densnew$x
              yp_new=Densnew$yp
              ypo_new=Densnew$ypo
              D_new=Densnew$D
              
              logR=p_new-p_old
              if (logR>log(runif(1))){
                  t_old=t_new
                  x_old=x_new
                  p_old=p_new
                  yp_old=yp_new
                  ypo_old=ypo_new
                  D_old=D_new
              }
              
              t[,i]=rbind(t_m,x_old)
              yp[,i]=yp_old
              ypo[,i]=ypo_old
              
              D[i]=D_old
          }
          
          if(j==1){
              t1=t
              yp1=yp
              ypo1=ypo
              D1=D
          } else if(j==2){
              t2=t
              yp2=yp
              ypo2=ypo
              D2=D
          } else if(j==3){
              t3=t
              yp3=yp
              ypo3=ypo
              D3=D
          } else if(j==4){
              t4=t
              yp4=yp
              ypo4=ypo
              D4=D
          }
          
      }
      
      
      Dhat=-2*sum(log(dlnorm(exp(RC$y),X_m%*%mu,sqrt(exp(t_m[2])))))
      seq=seq(2000,20000,5)
      Davg=mean(c(D1[seq],D2[seq],D3[seq],D4[seq]))
      pd=Davg-Dhat
      DIC=Dhat+2*pd
      B=1/(mean(0.5*c(D1[seq],D2[seq],D3[seq],D4[seq])))
      
      #c(Dhat, Davg, DIC, pd, B) #afhverju thessi vigur?
     
      
     return(list("RC"=RC,"ypo1"=ypo1,"ypo2"=ypo2,"ypo3"=ypo3,"ypo4"=ypo4,"l_m"=l_m,"t_m"=t_m,"qvdata"=qvdata,"fit"=X_m%*%mu, "mu"=mu))
      ## MODEL 1 ## End
      })
    })    
    
    
    plotratingcurve <- eventReactive(input$go,{
        plotlist=model1()
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        tafla=NULL
        outputlist=list()
        if(!is.null(plotlist$qvdata)) {
            
            data=data.frame(W=plotlist$RC$w,Q=plotlist$RC$y)
            data$l_m=plotlist$l_m
            data$fit=plotlist$fit
            
            #simdata=data.frame(fit=as.matrix(seq(min(data$fit),max(data$fit),length.out=1000)))
            #simdata$fitl_m = as.matrix(seq(min(data$l_m),max(data$l_m),length.out=1000))
            #simdata$Wfit=seq(min(log(data$W)),max(log(data$W)),length.out=1000)
            data$c_hat=rep(min(data$W)-exp(plotlist$t_m[1,]),length(data$l_m))
            #simdata$Wfit = exp(simdata$fitl_m) + c_hat
            seq=seq(2000,20000,5)
            quantypo1=plotlist$ypo1[,seq]
            quantypo2=plotlist$ypo2[,seq]
            quantypo3=plotlist$ypo3[,seq]
            quantypo4=plotlist$ypo4[,seq]
            quantmatrix=t(cbind(quantypo1,quantypo2,quantypo3,quantypo4))
            prctile=t(apply(quantmatrix, 2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE))
            data$lower=prctile[,1]
            data$upper=prctile[,2]
            
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(data)+theme_bw()+geom_point(aes(exp(Q),W))+geom_line(aes(exp(fit),W))+
                    geom_line(aes(exp(lower),W),linetype="dashed")+geom_line(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for",input$name))+ylab("W (cm)")+xlab(paste("Q",expression(m^3/s)))
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(data)+geom_line(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_line(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_line(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",input$name,"(log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                data$residraun=(exp(data$Q)-exp(data$fit))
                rcleifraun=ggplot(data)+geom_point(aes(exp(l_m)+c_hat,residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot")+xlab("W (cm)")
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                data$residlog=(data$Q-data$fit)/sqrt(exp(plotlist$t_m[2,]))
                rcleiflog=ggplot(data)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot (log scale)")+xlab(expression(log(W-hat(c))))
                
                
                outputlist$rcleiflog=rcleiflog
            }
            
            tafla=plotlist$qvdata
            tafla$W=tafla$W
            tafla$Qfit=as.numeric(format(round(as.vector(exp(plotlist$fit)),3)))
            tafla$lower=as.numeric(format(round(exp(data$lower),3)))
            tafla$upper=as.numeric(format(round(exp(data$upper),3)))
            names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper")
            outputlist$tafla=tafla
            
            
            return(outputlist)
        } 
        
        
    })
    output$callreactive <- renderPrint({
        plotlist=model1()
    })
    
    output$plots <- renderUI({
        if(length(plotratingcurve())!=0){
            plot_output_list <- lapply(1:(length(plotratingcurve())-1), function(i) {
                plotname=paste("plot", i, sep="")
                plotOutput(plotname)
            })
            
            do.call(tagList, plot_output_list)
        }
    })
    
    
    
    #     for(i in 1:5){
    #             my_i=i
    #             local({
    #                 name=paste("plot", my_i, sep="")
    #                 output[[name]]<-renderPlot({
    #                     plotratingcurve()[[i]]    
    #                 })
    #             })
    #     }
    output$plot1<-renderPlot({
        if(length(plotratingcurve())!=0)
            plotratingcurve()[[1]]
    },height=400,width=600)
    output$plot2<-renderPlot({
        if(length(plotratingcurve()) >= 2)
            plotratingcurve()[[2]]   
    },height=400,width=600)
    output$plot3<-renderPlot({
        if(length(plotratingcurve())>=3)
            plotratingcurve()[[3]]    
    },height=400,width=600)
    output$plot4<-renderPlot({
        if(length(plotratingcurve())>=4)
            plotratingcurve()[[4]]     
    },height=400,width=600)
    output$tafla <- renderGvis({
    #       if(!is.null(plotratingcurve()$tafla))
                table=as.data.frame(plotratingcurve()$tafla)
                gvisTable(table,options=list(
                    page='enable',
                    pageSize=30,
                    width=550
                ))
                
        
    })



    output$downloadReport <- downloadHandler(
        filename = function() {
            paste(input$file1$name, sep=".",'pdf')
        },
        
        content = function(file) {
            src <- normalizePath('myreport.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'myreport.Rmd')
            
            library(rmarkdown)
            out <- render('myreport.Rmd', pdf_document())
            file.rename(out, file)
        }
    )
    
})


