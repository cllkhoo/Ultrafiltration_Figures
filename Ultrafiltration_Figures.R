--------------------------------------------------Ultrafiltration Figures---------------------------------------------
  
  UFData <- read.csv("UF_Data_ForCoding.csv")
  str(UFData)
  library(ggplot2)
  library(plyr)
  library(ggpubr)
  library(RColorBrewer)
  
  #Size-fractioned DOC
  Bargraph1 <- ggplot(UFData, aes(x=factor(Salinity), y=DOC_uM, fill=Size_Fraction)) + geom_bar(stat = "identity")
  fill <- c("dodgerblue4","lightcyan4","burlywood4" )
  B1 <- Bargraph1 + theme_classic() + xlab("Salinity (psu)") + ylab(expression(DOC~(paste(mu,M)))) + scale_fill_manual(values = fill) + theme(legend.position=c(0.8,0.8)) +
    labs(fill="Size Fraction") + ylim(0,600)
  
  #Size-fractioned TDN
  Bargraph2 <- ggplot(UFData, aes(x=factor(Salinity), y=TDN_uM, fill=Size_Fraction)) + geom_bar(stat = "identity")
  fill <- c("dodgerblue4","lightcyan4","burlywood4" )
  B2 <- Bargraph2 + theme_classic() + xlab("Salinity (psu)") + ylab(expression(TDN~(paste(mu,M)))) + scale_fill_manual(values = fill) + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    labs(fill="Size Fraction") + ylim(0,20)
  
  #Size-fractioned % DOC with numbers in the bars
  fill <- c("dodgerblue4","lightcyan4","burlywood4" )
  Bargraph3a <- ggplot(UFData, aes(x=factor(Salinity), y=DOC_., fill=Size_Fraction)) + geom_bar(stat = "identity") + geom_text(aes(y=X.DOC, label=round(X.DOC,0)), position=position_stack(vjust = 0.5))
  B3a <- Bargraph3a + theme_classic() + xlab("Salinity (psu)") + ylab("DOC (%)") + scale_fill_manual(values = fill) + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  
  #Size-fractioned % DOC
  fill <- c("dodgerblue4","lightcyan4","burlywood4" )
  Bargraph3b <- ggplot(UFData, aes(x=factor(Salinity), y=DOC_., fill=Size_Fraction)) + geom_bar(stat = "identity")
  B3b <- Bargraph3b + theme_classic() + xlab("Salinity (psu)") + ylab("DOC (%)") + scale_fill_manual(values = fill) + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  
  #Size-fractioned % TDN
  fill <- c("dodgerblue4","lightcyan4","burlywood4" )
  Bargraph4 <- ggplot(UFData, aes(x=factor(Salinity), y=TDN_., fill=Size_Fraction)) + geom_bar(stat = "identity")
  B4 <- Bargraph4 + theme_classic() + xlab("Salinity (psu)") + ylab("TDN (%)") + scale_fill_manual(values = fill) + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  B4
  
  # Combining the graphs
  Figure_UFBargraphs <- ggarrange (B1,B3b,B2,B4, ncol=2, nrow=2, common.legend = TRUE,legend = "right")
  Figure_UFBargraphs #Save as PNG 900 x 700
  
  Figure_UFDOC <- ggarrange (B1,B3b, ncol=2, nrow=1, common.legend = TRUE, legend = "top") #Save as PNG 700 x 400
  Figure_UFDOC
  
  Figure_UFTDN <- ggarrange (B2,B4, ncol=2, nrow=1, common.legend = TRUE, legend = "top") #Save as PNG 700 x 400
  Figure_UFTDN
  
  #SUVA Connected scatterplot
  S1 <- ggplot(UFData, aes(Salinity, SUVA)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,5) + xlim (0,30)+ xlab("Salinity") + ylab(expression(SUVA[254]))
  UF_Suva <-S1 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position=c(0.8,0.8), legend.title = element_blank())
  UF_Suva
  #For sub scripts, use []
  #For super scripts, use ^
  #Save as PNG 500 x 400
  
  #S(275-295) Connected scatterplot
  S2 <- ggplot(UFData, aes(Salinity, S275.296)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.02) + xlim (0,30)+ xlab("Salinity") + ylab(expression(S[275-296]))
  UF_S1 <-S2 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position=c(0.8,0.2), legend.title = element_blank())
  UF_S1
  #Save as PNG 500 x 400
  
  #Size-fractioned Fe
  BargraphFe <- ggplot(UFData, aes(x=factor(Salinity), y=Fe_uM, fill=Size_Fraction)) + geom_bar(stat = "identity") 
  fill <- c("dodgerblue4","lightcyan4","burlywood4" )
  BFe <- BargraphFe + theme_classic() + xlab("Salinity (psu)") + ylab(expression(Fe~(paste(mu,M)))) + scale_fill_manual(values = fill) + theme(legend.position=c(0.8,0.8)) +
    labs(fill="Size Fraction") + ylim(0,3) #Save as PNG 500 x 400
  BFe
  
  #Fe:DOC Connected scatterplot
  SFe.DOC <- ggplot(UFData, aes(Salinity, Fe.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.05) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression(Fe:DOC))
  UF_Fe.DOC <-SFe.DOC + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position=c(0.8,0.8), legend.title = element_blank())
  UF_Fe.DOC #Save as PNG 500 x 400
  
  ## Plotting Ultrafiltration score outputs from PARAFAC Analysis ##
  
  # Fmax1 vs. Salinity
  fmax1 <- ggplot(UFData, aes(Salinity, Fmax1.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.08) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression("Fmax1/DOC"))
  UFFluor1<-fmax1 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                          panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position="top", legend.title = element_blank()) + scale_color_manual(values = fill)
  UFFluor1 
  
  # Fmax2 vs. Salinity
  fmax2 <- ggplot(UFData, aes(Salinity, Fmax2.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.08) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression("Fmax2/DOC"))
  UFFluor2<-fmax2 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                          panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position="top", legend.title = element_blank()) + scale_color_manual(values = fill)
  UFFluor2
  
  # Fmax3 vs. Salinity
  fmax3 <- ggplot(UFData, aes(Salinity, Fmax3.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.08) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression("Fmax3/DOC"))
  UFFluor3<-fmax3 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                          panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position="top", legend.title = element_blank()) + scale_color_manual(values = fill)
  UFFluor3
  
  # Fmax4 vs. Salinity
  fmax4 <- ggplot(UFData, aes(Salinity, Fmax4.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.08) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression("Fmax4/DOC"))
  UFFluor4<-fmax4 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                          panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position="top", legend.title = element_blank()) + scale_color_manual(values = fill)
  UFFluor4
  
  # Fmax5 vs. Salinity
  fmax5 <- ggplot(UFData, aes(Salinity, Fmax5.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.08) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression("Fmax5/DOC"))
  UFFluor5<-fmax5 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                          panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position="top", legend.title = element_blank()) + scale_color_manual(values = fill)
  UFFluor5
  
  # Fmax6 vs. Salinity
  fmax6 <- ggplot(UFData, aes(Salinity, Fmax6.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.08) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression("Fmax6/DOC"))
  UFFluor6<-fmax6 + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                          panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position="top", legend.title = element_blank()) + scale_color_manual(values = fill)
  UFFluor6
  
  UFFmaxFigures <- ggarrange (UFFluor1, UFFluor2, UFFluor3, UFFluor4, UFFluor5, UFFluor6, ncol=3, nrow=2, common.legend = TRUE, legend = "top")
  UFFmaxFigures
  
  # Total Fmax vs. Salinity
  fmaxtotal <- ggplot(UFData, aes(Salinity, FmaxTotal.DOC)) + geom_line() +
    geom_point()+aes(color=Size_Fraction,shape=Size_Fraction)+ ylim(0,0.4) + xlim (0,30)+ xlab("Salinity (psu)") + ylab(expression("Fmax(total)/DOC"))
  UFFluortotal <-fmaxtotal + theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                   panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) + theme(legend.position="top", legend.title = element_blank()) + scale_color_manual(values = fill)
  UFFluortotal #Save as PNG 500 x 400
  