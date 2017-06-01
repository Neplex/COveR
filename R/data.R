#' Emotions in music aggregate to interval multi label data.
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

#' Scene aggregate to interval multi label data.
#'
#' @format A interval structure with 2089 rows and 294 variables divided in min
#' and max with 7 class:
#' \describe{
#'   \item{Att1}{}
#'   \item{Att2}{}
#'   \item{Att3}{}
#'   ...
#'   \item{Att292}{}
#'   \item{Att293}{}
#'   \item{Att294}{}
#' }
#'
#' Class :
#' \describe{
#'   \item{Beach}{}
#'   \item{Sunset}{}
#'   \item{FallFoliage}{}
#'   \item{Field}{}
#'   \item{Mountain}{}
#'   \item{Urban}{}
#' }
#'
#' @source \url{http://mulan.sourceforge.net/datasets-mlc.html}
"inter_scene"

#' Results of a chemical analysis of wines grown in the same region in Italy but
#' derived from three different cultivars, aggregate to interval simple label
#' data.
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

#' Cellular Localization Sites of Proteins aggregate to interval multi label data.
#'
#' @format A interval structure with 2393 rows and 102 variables divided in min
#' and max with 14 class:
#' \describe{
#'   \item{Att1}{}
#'   \item{Att2}{}
#'   \item{Att3}{}
#'   ...
#'   \item{Att101}{}
#'   \item{Att102}{}
#'   \item{Att103}{}
#' }
#'
#' Class :
#' \describe{
#'   \item{Class1}{}
#'   \item{Class2}{}
#'   \item{Class3}{}
#'   \item{Class4}{}
#'   \item{Class5}{}
#'   \item{Class6}{}
#'   \item{Class7}{}
#'   \item{Class8}{}
#'   \item{Class9}{}
#'   \item{Class10}{}
#'   \item{Class11}{}
#'   \item{Class12}{}
#'   \item{Class13}{}
#'   \item{Class14}{}
#' }
#'
#' @source \url{http://mulan.sourceforge.net/datasets-mlc.html}
"inter_yeast"
