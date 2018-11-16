setwd("/Users/nihei13/Documents/GSS")
source('~/Documents/GSS/my_function.R', encoding = 'UTF-8')
clearAll()

library(magrittr)
library(quickpsy)
library(ggplot2)
library(gridExtra)

# Load a script
source('~/Documents/GSS/my_function_GSS.R', encoding = 'UTF-8')
source('~/Documents/GSS/anovakun_482.txt', encoding = 'CP932')

unit.def <- 'inches'
plot.length.def <- 6
legend.length.def <- 1.5

PLOT_SUB = TRUE;

# dat_path <- "data_kids/"
# dat_path <- "data_adults/"
# dat_path <- "data_oa_kids/"
# dat_path <- "data_oa_adults/"
dat_path <- "data_jp_adults/"
result_path <- "results/"

filesave_path <-
  paste(result_path, dat_path, sep = "", collapse = NULL)

if (!file.exists(filesave_path)) {
  dir.create(filesave_path)
}


remove_sub <- data.frame(
  Subject =
    c(
      "MYK01",
      "MYK02",
      "MYK03",
      "MYK04",
      "MYK05",
      "MYK08",
      "MYK07",
      "MYK09",
      "MYK10f",
      "MYK19",
      "MYK20",
      "MYA02",
      "MYA10",
      "MYA16",
      "MYA000",
      "JPA10",
      "JPA11",
      "JPA12",
      "JPA13",
      "JPA15",
      "JPA16"
    )
)

fl <- list.files(dat_path, full.names = T)

df_hoge <- as.data.frame(NULL)
df_hoge2 <- as.data.frame(NULL)
for (i in 1:length(fl)) {
  df <- readfiles(fl[i])
  df_hoge2 <- rbind(df_hoge2, df)
  sub_name <- as.character(df$Subject)
  if (!charmatch(sub_name[1], remove_sub[, 1], nomatch = 0)) {
    df_pse <- preprocessing_PSE(df)
    df_hoge <- rbind(df_hoge, df_pse)
  }
}
# df <- as.data.frame(NULL)

#
df_sub <- aggregate(N ~ Subject, data = df_hoge, sum)
sub_name <- as.character(df_sub$Subject)

sub_thre_all <- as.data.frame(NULL)
for (i in 1:length(df_sub$Subject)) {
  if (!file.exists(paste(filesave_path, sub_name[i], "_fit_data.Rdata"))) {
    df_eachsub <- df_hoge[df_hoge$Subject == sub_name[i],]
    fit_sub <-
      quickpsy(
        df_eachsub,
        Anger_Morph_level,
        CorrectedResponse,
        N,
        grouping = .(FacialColor, Race)
      )
    fit_sub
    
    sub_thre <-
      data.frame(sub = rep(sub_name[i], times = 4), fit_sub$thresholds[1:3])
    
    sub_thre_all <- rbind(sub_thre_all, sub_thre)
    
    save(sub_thre,
         fit_sub,
         file = paste(filesave_path, sub_name[i], "_fit_data.Rdata"))
  } else{
    load(paste(filesave_path, sub_name[i], "_fit_data.Rdata"))
    
    sub_thre_all <- rbind(sub_thre_all, sub_thre)
  }
  
  if (PLOT_SUB)
  {
    plotting_PSE(fit_sub, sprintf('%s%s', filesave_path, sub_name[i]))
  }
}
##

df_conc <-
  aggregate(CorrectedResponse ~ FacialColor + Race + Anger_Morph_level + N,
            data = df_hoge,
            mean)

fit <- quickpsy(df_conc,
                Anger_Morph_level,
                CorrectedResponse,
                N,
                grouping = .(FacialColor, Race))
fit

plotting_PSE(fit, paste(filesave_path, "fitting"))

plotting_PSE_bar(sub_thre_all, paste(filesave_path, "barplot"))

sink(paste(filesave_path, "anova.txt"))

anovakun(sub_thre_all, "sAB", long = T, cilm = T)

sink()
