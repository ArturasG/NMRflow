
args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      PLS-DA

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outDir=path - output folder
      --factorFile=path - a path to a factorfile
      --factorCol=num - a which column to use

      Example:
      ./plsda.R --input=inputFilePath --output=outputFilePath --outDir=outputDirPath --factorFile=factorFilePath --factorCol=num \n\n")
  q(save="no")
}

suppressMessages(require(pls))
# -------------------functions --------------------

make.factorMat = function(fact){
  fact = as.character(fact)
  unFact = unique(fact)
  N = length(unFact)
  out = do.call('rbind', lapply(fact, function(currFact) { y=rep(0,N); y[which(unFact == currFact)] = 1 ; y}))
  colnames(out) <- unFact
  out
}

# -------------------------------------------------


parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# import data

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
factorFile = read.table(args[['factorFile']], header=T, sep=',', stringsAsFactors = T, row.names=1)
factor = factorFile[,as.numeric(args[['factorCol']])]

factor_  = make.factorMat(factor)

comp.num = nrow(data) - 1;
if(comp.num > 8) comp.num = 8

res = plsr(factor_ ~ as.matrix(data), method='oscorespls', ncomp=comp.num)

makeHTML = function(res){

  # files - a list of files to display (files within each entry are displayed next to each other?)
  css.H1 <- '\"text-align: center;font-family:verdana; font-size:30px\"'
  css.textDiv <- '\"text-align=: center; font-family:verdana; font-size:10px; padding-top:35px;padding-bottom=25px\"'

  html <- c('<!DOCTYPE html>',
            '<html>',
            '<head>',
            '</head>',
            '<body','asldjfhaksldfh')


  html <- c(html,
            '</body>',
            '</html>')

  html
}

htmlCode <- makeHTML(res)

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)

# write outputs