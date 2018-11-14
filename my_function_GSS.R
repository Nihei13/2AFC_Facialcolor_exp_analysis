readfiles <- function(x) {
  v <-
    scan(
      x,
      what = character(),
      sep = "\n",
      blank.lines.skip = F
    )
  
  Individual_info <- unlist(strsplit(v[2], "\\,"))
  Individual_info <- gsub(" ", "", Individual_info, fixed = TRUE)
  
  Subject_name <- Individual_info[2]
  
  Subject_gender <- Individual_info[4]
  
  Subject_age <- Individual_info[6]
  
  Header_name <- unlist(strsplit(v[8], "\\,"))
  Header_name <- gsub(" ", "", Header_name, fixed = TRUE)
  
  txtdat <- textConnection(v[9:length(v)])
  tmp_dat <- read.table(txtdat, sep = ",")
  tmp_dat <- tmp_dat[, 1:6]
  names(tmp_dat) <- Header_name
  
  tmp_dat2 <- tmp_dat[ ,3:6]
  tmp_dat2[tmp_dat2$CorrectedResponse == 2, "CorrectedResponse"] = 0
  tmp_dat2 <- tmp_dat2[order(tmp_dat2$ID),]
  
  tmp_dat3 <- data.frame( Subject = rep(Subject_name, times = nrow(tmp_dat)), 
                          Gender = rep(Subject_gender, times = nrow(tmp_dat)),
                          Age = rep(Subject_age, times = nrow(tmp_dat)),
                          FacialColor = c(rep("Neutral", times = nrow(tmp_dat)/2), 
                                          rep("Reddish", times = nrow(tmp_dat)/2)),
                          Race = c(rep(c(rep("Caucasian", times = nrow(tmp_dat)/4),
                                   rep("Asian", times = nrow(tmp_dat)/4)), times = 2)),
                          Gender_type = c(rep(c(rep("Male", times = nrow(tmp_dat)/8),
                                                rep("Female", times = nrow(tmp_dat)/8)),
                                              times = 4)),
                          Anger_Morph_level = c(rep(
                            c(0, 16.7, 33.4, 50, 66.7, 83.4, 100), 
                            times = 40)),
                          N = c(rep(1, times = nrow(tmp_dat)))
                          )
  
  dat <- cbind(tmp_dat3,tmp_dat2)
  
  return(dat)
}

preprocessing_PSE <- function(x) {
  dat1 <- aggregate(CorrectedResponse ~ Subject + FacialColor + Race + Anger_Morph_level, data = x, sum)
  dat2 <- aggregate(N ~ Subject + FacialColor + Race + Anger_Morph_level, data = x, sum)
  N <- dat2$N
  
  dat <- cbind(dat1, N)
  
  return(dat)
}

plotting_PSE <- function(x,filename) {
  condtion1_curve <- x$curves[x$curves$Race == 'Asian', ]
  condtion2_curve <- x$curves[x$curves$Race == 'Caucasian', ] 
  
  condtion1_point <- x$averages[x$averages$Race == 'Asian', ] 
  condtion2_point <- x$averages[x$averages$Race == 'Caucasian', ] 
  
  plot_asian <- ggplot()+
    theme(plot.title = element_text(size=20, face="bold"),
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          legend.position="top",
          legend.title = element_text(size=14,face="bold"),
          legend.text = element_text(size=14))+
    ggtitle("Asian") +
    geom_point(data = condtion1_point, aes(x = Anger_Morph_level, y = prob, color = FacialColor), size = 3) +
    scale_fill_manual(name="FacialColor",values=c("#576574", "#ff6b6b")) + 
    geom_line(data = condtion1_curve,
              aes(x = x, y = y, color = FacialColor), size = 1) +
    scale_color_manual(name="FacialColor",values=c("#576574", "#ff6b6b")) + 
    geom_linerange(data = x$thresholds[c(1,3),], 
                   aes(x = thre, ymin = 0, ymax = prob), lty =2) +
    xlab("Anger morphing level[%]")+ylab("Anger response rate")+
    ylim(0,1)+
    xlim(0,100)
  plot_asian
  
  plot_caucasian <- ggplot()+
    theme(plot.title = element_text(size=20, face="bold"),
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          legend.position="top",
          legend.title = element_text(size=14,face="bold"),
          legend.text = element_text(size=14))+
    ggtitle("Caucasian") +
    geom_point(data = condtion2_point, aes(x = Anger_Morph_level, y = prob, color = FacialColor), size = 3) +
    scale_fill_manual(name="FacialColor",c("#576574", "#ff6b6b")) + 
    geom_line(data = condtion2_curve,
              aes(x = x, y = y, color = FacialColor), size = 1) +
    scale_color_manual(name="FacialColor",values=c("#576574", "#ff6b6b")) + 
    geom_linerange(data = x$thresholds[c(2,4),], 
                   aes(x = thre, ymin = 0, ymax = prob), lty =2) +
    xlab("Anger morphing level[%]")+ylab("Anger response rate")+
    ylim(0,1)+
    xlim(0,100)
  plot_caucasian
  grid.arrange(plot_asian, plot_caucasian, ncol = 2) #arranges plots within grid
  
  #save
  g <- arrangeGrob(plot_asian, plot_caucasian, ncol = 2)
  # ggsave(filename,g)
  ggsave(paste(filename,".png"),g)
  ggsave(paste(filename,".eps"),g)
}

plotting_PSE_bar <- function(x,filename) {
  
  means_dat <- aggregate(thre ~ FacialColor + Race, data = x, mean)
  std_dat <- aggregate(thre ~ FacialColor + Race, data = x, sd)
  num_sub <- nrow(aggregate(thre ~ sub, data = sub_thre_all, mean))
  ci <- qnorm(0.975) * std_dat$thre/sqrt(num_sub)
  bar <- ggplot(means_dat,aes(x=Race, y=thre, fill=FacialColor))+
    theme(plot.title = element_text(size=20, face="bold"),
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          legend.position="top",
          legend.title = element_text(size=14,face="bold"),
          legend.text = element_text(size=14))+
    geom_bar(stat="identity",position="dodge", colour="black", size=.8)+
    geom_errorbar(aes(ymin=thre-ci, ymax=thre+ci),
                  size=.5,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9))+
    scale_fill_manual(values=c("#c8d6e5", "#ff6b6b"))+
    # scale_fill_discrete(name="FacialColor")+
    xlab("Races")+ylab("Mean PSE")+
    ylim(0,80)
  bar
  
  #save
  ggsave(paste(filename,".png"),bar)
  ggsave(paste(filename,".eps"),bar)
}