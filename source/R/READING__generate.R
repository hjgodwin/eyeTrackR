library(magick)
library(randomcoloR)
 
#' Generate Reading Trial Image and Interest Area File
#'
#' @param preTargetText The sentence text before the target word.
#' @param targetText The sentence text comprising the target word only.
#' @param postTargetText The sentence text that follows the target word.
#' @param width The width of the display in pixels (default 1024).
#' @param height The height of the display in pixels (default 768)
#' @param fontSize The font size of the text (default 14)
#' @param letterWidth The letter width. This is used to space the letters out if they overlap. 
#' @param font The font to use (default is Courier New)
#' @param fontColor The colour of the font to use (default is black)
#' @param backgroundColor The background colour of the display (default is white)
#' @param trialLabel The label of your trial. This will be used to name your image and associated interest area file.
#' @param iaFolder The folder to save your interest area files into.
#' @param imgFolder The folder to save your images into.
#' @param startX The start position on the x axis for your sentence.
#' @param startY The start position on the y axis for your sentence.
#' @param highlightEachBox When set to TRUE, this puts a randomly-coloured box behind each letter. 
#' Used for making sure that letterWidth is set to something sensible. Default is FALSE.
#'
#' @return String saying 'DONE' when completed. Also saves an interest area file and an image.
#'
#' @examples
generate.reading.trial <- function(preTargetText = 'Life was a fly that faded and death a', 
                                   targetText=' drone', 
                                   postTargetText = ' that stung. ',
                                   width = 1024, 
                                   height=768, 
                                   fontSize=14, 
                                   letterWidth=8,
                                   font='courier new', 
                                   fontColor='black', 
                                   backgroundColor = 'white',
                                   trialLabel = 'sentence1',
                                   iaFolder = getwd(),
                                   imgFolder = getwd(),
                                   startX = 50, 
                                   startY=768/2, 
                                   highlightEachBox = FALSE){
  
  # GENERATE IA FILE TO WORK THROUGH AND CREATE THE IMAGE AND IA FILE
  allWords <- strsplit(paste(preTargetText, targetText, postTargetText, sep=''), " ")[[1]]
  iaFile <- data.table(SHAPE='RECTANGLE', IA_INDEX=seq(from=1, to=length(allWords)), LEFT=1, TOP=2, RIGHT=3, BOTTOM=4, IA_LABEL=allWords)
  targetIndex <- length(strsplit(preTargetText, split = ' ')[[1]]) + 1
  
  # ADD A SPACE BEFORE EACH WORD
  iaFile[IA_INDEX>1,IA_LABEL:=paste(' ', IA_LABEL, sep=''),list(IA_INDEX)]
  
  # SETUP BACKGROUND BLANK IMAGE
  readingStim <- image_blank(width, height, backgroundColor)

  # X CURSOR TRACKS OUR X POSITION
  xCursor <- startX
  
  # SET BOX COLOUR FOR DIAGNOSTICS
  boxColour = 'white'
  
  # GO THROUGH EACH WORD IN THE IA FILE
  for (i in seq(1:nrow(iaFile))){
    
    splitWord <- strsplit(iaFile$IA_LABEL[[i]], "")[[1]]
    
    currentWordStart <- xCursor
    
    for (j in seq(1:length(splitWord))){
      
      # COLOUR EACH LETTER DIFFERENTLY IF REQUIRED FOR DIAGNOSITCS
      if (highlightEachBox ==T) {boxColour = randomColor(count=1)}
    
      # ADD LETTER TO IMAGE
      readingStim <- image_annotate(readingStim, 
                                boxcolor = boxColour,
                                splitWord[[j]], 
                                size=fontSize, 
                                font = font, 
                                color= fontColor, 
                                location= paste('+', xCursor, '+', startY, sep=''))
      
      xCursor <- xCursor + letterWidth
    }
    
    currentWordEnd <- xCursor
    
    # UPDATE VALUES IN IA FILE
    iaFile$LEFT[i] <- currentWordStart
    iaFile$TOP[i] <- 0
    iaFile$RIGHT[i] <- currentWordEnd
    iaFile$BOTTOM[i] <- height
    
    # IF WE JUST ADDED THE TARGET WORD, UPDATE THIS IN THE IA FILE
    if (i==targetIndex){
      iaFile[IA_INDEX==i,IA_LABEL:=paste('TARGET__', IA_LABEL, sep=''),]
    }
  }
  
  # SAVE IA
  fwrite(iaFile, col.names=F, sep='\t', paste(trialLabel, '.ias', sep=''))
  
  # SAVE IMAGE
  image_write(readingStim, paste(trialLabel, '.png', sep=''), format='.png')
  
  return('DONE')
  
}
