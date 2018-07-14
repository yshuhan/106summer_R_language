source('pttTestFunction.R')
id = c(1:10)
URL = paste0("https://www.ptt.cc/bbs/Stock/index.html", id, ".html")
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, 
       URL = URL, filename = filename)

