library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
    
    model1<-reactive({
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        #output$text<-renderPrint({
        #    inFile$type
        #})
        
        if (inFile$type =="text/plain"){
            qvdata=read.table(inFile$datapath,skip=3,sep="|")
            qvdata=qvdata[,c(2:4,7)]
            qvdata=data.frame(lapply(qvdata, as.character), stringsAsFactors=FALSE)
            qvdata[,3:4]=apply(qvdata[,c(3,4)],2, function(x) as.numeric(gsub(",",".",x)))
            names(qvdata)=c("Date","Time","Q","H")
            wq=as.matrix(qvdata[,3:4])
            
        }
        else if(inFile$type=="text/csv"){
            qvdata=read.table(inFile$datapath,skip=16)
            qvdata=qvdata[,c(1:3,6)]
            qvdata=data.frame(lapply(qvdata, as.character), stringsAsFactors=FALSE)
            qvdata[,3:4]=apply(qvdata[,c(3,4)],2, function(x) as.numeric(gsub(",",".",x)))
            names(qvdata)=c("Date","Time","Q","H")
            wq=as.matrix(qvdata[,3:4])
            
        }
        else {
            qvdata=read.table(inFile$datapath)
            names(qvdata)=c("H","Q")
            qvdata$H=100*qvdata$H
            wq=as.matrix(qvdata[,2:1])
        }
        
      ## DENSEVAL11 ## Begin
      
      Denseval11 <- function(th,RC){
          
          #hugsanlega onnur breytunofn
          l=log(RC$w_tild+exp(th[1]))
          X=cbind(rep(1,length(l)),l)
          
          L=t(chol(RC$Sig_xinv + (t(X) %*% X)/exp(th[2])))
          
          q=solve(L,(RC$Sinvmu+t(X)%*%RC$y/exp(th[2])))
          #Solvi end
          
          #Solvi begin 27.mai
          p=0.5*sum(q^2)+log(L[1,1])+log(L[2,2])- 
              0.5*sum(RC$y^2)/exp(th[2])-RC$n*th[2]/2 +
              th[1]-exp(th[1])*RC$mu_c-th[2]
          
          x=solve(t(L),(q+as.matrix(rnorm(2))))
          
          yp=X %*% x
          
          ypo=yp+as.matrix(rnorm(RC$n,sd=sqrt(exp(th[2]))))
          
          D=-2*sum(log(dlnorm(exp(RC$y),X%*%x,sqrt(exp(th[2])))))
          
          return(list("pmin"=-p,"p"=p,"x"=x,"yp"=yp,"ypo"=ypo,"D"=D))
      }
      
      ## DENSEVAL11 ## End
      
      
      ## MODEL1 ##  Begin
      
      Nit=20000
      dataset=15
      
      #Prior Parameters
      RC=list()
      RC$mu_a=3.20;
      RC$mu_b=2.29;
      RC$sig_a=sqrt(1.21);
      RC$sig_b=sqrt(0.48);
      RC$p_ab=-0.61;
      RC$Sig_x=rbind(c(RC$sig_a^2, RC$p_ab*RC$sig_a*RC$sig_b), c(RC$p_ab*RC$sig_a*RC$sig_b, RC$sig_b^2))
      
      
      RC$mu_x=as.matrix(c(RC$mu_a, RC$mu_b))
      
      
      RC$Sig_xinv=solve(RC$Sig_x);
      RC$Lx=chol(RC$Sig_x);
      RC$mu_c=1.9;
      RC$Sinvmu=RC$Sig_xinv%*%RC$mu_x;
  
      
      #%import data from text file that has water level measurements in cm in left
      #%column and corresponding discharge measurements in m^3/s in right column
      
      #axel: 
      
      #wq = as.matrix(read.table('15.txt'))
      
      
      
      RC$y=as.matrix(log(wq[,1]));
      RC$w=0.01*wq[,2]; #to meters 
      RC$w_tild=RC$w-RC$w[1];
      RC$n=length(RC$y);
      
      
      Dens <- function(th){ Denseval11(th,RC)$pmin}
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
      
      
      
      t1=matrix(0,4,Nit)
      t2=matrix(0,4,Nit)
      t3=matrix(0,4,Nit)
      t4=matrix(0,4,Nit)
      
      
      
      for(j in 1:4){
          t_old=t_m
          t=matrix(0,nrow=4,ncol=Nit)
          yp=matrix(0,nrow=nrow(wq),ncol=Nit)
          ypo=matrix(0,nrow=nrow(wq),ncol=Nit)
          
          D=c()
          
          
          Dens<-Denseval11(t_old,RC)
          p_old=Dens$p
          x_old=Dens$x
          yp_old=Dens$yp
          ypo_old=Dens$ypo
          D_old=Dens$D
          
          for(i in 1:Nit){
              t_new=t_old+solve(t(LH),as.matrix(rnorm(2,0,1)))
              
              Densnew<-Denseval11(t_new,RC)
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
     
      
     return(list("RC"=RC,"ypo1"=ypo1,"ypo2"=ypo2,"ypo3"=ypo3,"ypo4"=ypo4,"l_m"=l_m,"t_m"=t_m,"wq"=wq,"fit"=X_m%*%mu))
      ## MODEL 1 ## End
    })
#     output$text<-renderPrint({
#         model1()$RC
#     })
    
    plotratingcurve <- eventReactive(input$go,{
        plotlist=model1()
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        tafla=NULL
        outputlist=list()
        if(!is.null(plotlist$wq)) {
            
            data=data.frame(W=plotlist$RC$w, Q=plotlist$RC$y)
            data$l_m=plotlist$l_m
            data$fit=plotlist$fit
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
                rcraun=ggplot(data)+theme_bw()+geom_point(aes(exp(Q),W))+geom_line(aes(exp(fit),W))+geom_line(aes(exp(lower),W),linetype="dashed")+
                    geom_line(aes(exp(upper),W),linetype="dashed")
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(data)+geom_line(mapping=aes(l_m,fit))+theme_bw()+geom_point(mapping=aes(l_m,Q))+geom_line(mapping=aes(l_m,lower),linetype="dashed")+
                    geom_line(mapping=aes(l_m,upper),linetype="dashed")
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                data$residraun=(exp(plotlist$RC$y)-exp(plotlist$RC$fit))/sqrt(exp(plotlist$t_m[2,]))
                rcleifraun=ggplot(data)+geom_point(aes(exp(l_m),residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = exp(2), slope = 0,linetype="dashed")+geom_abline(intercept = -exp(2), slope = 0,linetype="dashed")+
                    ylim(-8,8)+ylab(expression(epsilon[i]))+xlim(0,4)
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                data$residlog=(plotlist$RC$y-plotlist$RC$fit)/sqrt(exp(plotlist$t_m[2,]))
                
                rcleiflog=ggplot(data)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
                    ylab(expression(epsilon[i]))
                outputlist$rcleiflog=rcleiflog
            }
            if("tafla" %in% input$checkbox) {
                tafla=as.data.frame(plotlist$wq)
                outputlist$tafla=tafla
            }
            
            return(outputlist)
        } 
        
        
    })
    
    output$plots <- renderUI({
        if(length(plotratingcurve())!=0){
            plot_output_list <- lapply(1:length(plotratingcurve()), function(i) {
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
    })
    output$plot2<-renderPlot({
        if(length(plotratingcurve()) >= 2)
            plotratingcurve()[[2]]   
    })
    output$plot3<-renderPlot({
        if(length(plotratingcurve())>=3)
            plotratingcurve()[[3]]    
    })
    output$plot4<-renderPlot({
        if(length(plotratingcurve())>=4)
            plotratingcurve()[[4]]     
    })
    output$tafla <- renderTable({
        if('tafla' %in% input$checkbox)
            plotratingcurve()$tafla
        
        
    })
    
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste(input$file1$name, sep=".", switch(
                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
        },
        
        content = function(file) {
            src <- normalizePath('myreport.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'myreport.Rmd')
            
            library(rmarkdown)
            out <- render('myreport.Rmd', switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )
    
})


