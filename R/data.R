#' Emotions in music aggregate on BPM to interval multi label data.
#'
#' @format A interval structure with 59 rows and 71 variables divided in min
#' and max with 6 class:
#' \describe{
#'   \item{Mean_Acc1298_Mean_Mem40_Centroid}{}
#'   \item{Mean_Acc1298_Mean_Mem40_Rolloff}{}
#'   \item{Mean_Acc1298_Mean_Mem40_Flux}{}
#'   \item{Mean_Acc1298_Mean_Mem40_MFCC_0}{}
#'   ...
#'   \item{Mean_Acc1298_Mean_Mem40_MFCC_12}{}
#'   \item{Mean_Acc1298_Std_Mem40_Centroid}{}
#'   \item{Mean_Acc1298_Std_Mem40_Rolloff}{}
#'   \item{Mean_Acc1298_Std_Mem40_Flux}{}
#'   \item{Mean_Acc1298_Std_Mem40_MFCC_0}{}
#'   ...
#'   \item{Mean_Acc1298_Std_Mem40_MFCC_12}{}
#'   \item{Std_Acc1298_Mean_Mem40_Centroid}{}
#'   \item{Std_Acc1298_Mean_Mem40_Rolloff}{}
#'   \item{Std_Acc1298_Mean_Mem40_Flux}{}
#'   \item{Std_Acc1298_Mean_Mem40_MFCC_0}{}
#'   ...
#'   \item{Std_Acc1298_Mean_Mem40_MFCC_12}{}
#'   \item{Std_Acc1298_Std_Mem40_Centroid}{}
#'   \item{Std_Acc1298_Std_Mem40_Rolloff}{}
#'   \item{Std_Acc1298_Std_Mem40_Flux}{}
#'   \item{Std_Acc1298_Std_Mem40_MFCC_0}{}
#'   ...
#'   \item{Std_Acc1298_Std_Mem40_MFCC_12}{}
#'   \item{BH_LowPeakAmp}{}
#'   \item{BH_LowPeakBPM}{}
#'   \item{BH_HighPeakAmp}{}
#'   \item{BH_HighLowRatio}{}
#'   \item{BHSUM1}{}
#'   \item{BHSUM2}{}
#'   \item{BHSUM3}{}
#' }
#'
#' Class :
#' \describe{
#'   \item{amazed.suprised}{}
#'   \item{happy.pleased}{}
#'   \item{relaxing.calm}{}
#'   \item{quiet.still}{}
#'   \item{sad.lonely}{}
#'   \item{angry.aggresive}{}
#' }
#'
#' @source \url{http://mulan.sourceforge.net/datasets-mlc.html}
"inter_emotions"

#' Results of a chemical analysis of wines grown in the same region in Italy but
#' derived from three different cultivars, aggregate on sulfur dioxide to
#' interval simple label data.
#'
#' @format A interval structure with 132 rows and 10 variables divided in min
#' and max with 7 class:
#' \describe{
#'   \item{fixed.acidity}{}
#'   \item{volatile.acidity}{}
#'   \item{citric.acid}{}
#'   \item{residual.sugar}{}
#'   \item{chlorides}{}
#'   \item{total.sulfur.dioxide}{}
#'   \item{density}{}
#'   \item{pH}{}
#'   \item{sulphates}{}
#'   \item{alcohol}{}
#' }
#'
#' Class :
#' \describe{
#'   \item{Class3}{}
#'   \item{Class4}{}
#'   \item{Class5}{}
#'   \item{Class6}{}
#'   \item{Class7}{}
#'   \item{Class8}{}
#'   \item{Class9}{}
#' }
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets.html}
"inter_wine"

#' Temperature by month and humidity in european city.
#'
#' @format A interval structure with 68 rows and 13 variables divided in min
#' and max with 17 class:
#' \describe{
#'   \item{temp.jan}{}
#'   \item{temp.fev}{}
#'   \item{temp.mars}{}
#'   \item{temp.avr}{}
#'   \item{temp.mai}{}
#'   \item{temp.juin}{}
#'   \item{temp.juil}{}
#'   \item{temp.aout}{}
#'   \item{temp.sep}{}
#'   \item{temp.oct}{}
#'   \item{temp.nov}{}
#'   \item{temp.dec}{}
#'   \item{humid}{}
#' }
#'
#' Class :
#' \describe{
#'   \item{Allemagne}{}
#'   \item{Angleterre}{}
#'   \item{Autriche}{}
#'   \item{Belgique}{}
#'   \item{Bulgarie}{}
#'   \item{Croatie}{}
#'   \item{Danemark}{}
#'   \item{Espagne}{}
#'   \item{France}{}
#'   \item{Italie}{}
#'   \item{Pays-Bas}{}
#'   \item{Pologne}{}
#'   \item{Portugal}{}
#'   \item{Roumanie}{}
#'   \item{Russie}{}
#'   \item{Turquie}{}
#'   \item{Ukraine}{}
#' }
"inter_city"
