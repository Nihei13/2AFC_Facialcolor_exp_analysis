setwd( "/Users/nihei13/Documents/GSS")
source('~/Documents/GSS/my_function.R', encoding = 'UTF-8')
clearAll()

library(magrittr)
library(quickpsy)
library(ggplot2)
library(gridExtra)

# Load a script
source('~/Documents/GSS/my_function_GSS.R', encoding = 'UTF-8')
source('~/Documents/GSS/anovakun_482.txt', encoding = 'CP932')

# dat_path <- "data_kids/"
dat_path <- "data_adults/"
# dat_path <- "data_oa_kids/"
# dat_path <- "data_oa_adults/"
# dat_path <- "data_jp_adults/"
result_path <- "results/"

filesave_path <- paste(result_path, dat_path, sep = "", collapse = NULL)

if (!file.exists(filesave_path)){
  dir.create(filesave_path)
}

remove_sub <- data.frame(Subject = 
                           c("MYK01","MYK02","MYK03","MYK04","MYK05","MYK08","MYK07","MYK09","MYK10f","MYK19","MYK20", "MYA02","MYA10","MYA16","MYA000","JPA10"))

fl <- list.files(dat_path,full.names=T)

df_hoge <- as.data.frame(NULL)
df_hoge2 <- as.data.frame(NULL)
for (i in 1:length(fl)){
  df <- readfiles(fl[i])
  df_hoge2 <- rbind(df_hoge2,df)
  sub_name <- as.character(df$Subject)
  if(!charmatch(sub_name[1], remove_sub[,1], nomatch = 0) ){
    df_pse <- preprocessing_PSE(df)
    df_hoge <- rbind(df_hoge,df_pse)
  }
}
# df <- as.data.frame(NULL)
df_sub <- aggregate(N ~ Subject, data = df_hoge, sum)
sub_name <- as.character(df_sub$Subject)
target_sub <- "MYA21"
for (i in 1:length(df_sub$Subject)){
  
  if(charmatch(sub_name[i], target_sub, nomatch = 0) ){
    
    df_eachsub <- df_hoge[df_hoge$Subject == sub_name[i], ]
    fit_sub <- quickpsy(df_eachsub, Anger_Morph_level, CorrectedResponse, N,
                        grouping = .(FacialColor, Race))
    fit_sub
    
    plotting_PSE(fit_sub,sprintf('%s%s.png', filesave_path, sub_name[i]))
  }
}
