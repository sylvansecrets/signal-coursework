library("dplyr")
library("qdap")
library("tictoc")
library("ggplot2")

setwd("C:/Users/User/Documents/GitHub/Signal-Data-Science")
linux_loc = "C:/Users/User/Documents/GitHub/Signal-Data-Science/linus.txt"
vim_loc = "C:/Users/User/Documents/GitHub/Signal-Data-Science/vim.txt"

linux_message = readChar(linux_loc, file.info(linux_loc)$size)
vim_message = readChar(vim_loc, file.info(vim_loc)$size)

check_linux = qdap::check_text(linux_message)

l = unlist(strsplit(linux_message, "\r\n"))
l = l[!(l=="")]
v = unlist(strsplit(vim_message, "\r\n"))
v = v[!(v=="")]


l_clean = replace_symbol(strip(incomplete_replace(scrubber(l))))
v_clean = replace_symbol(strip(incomplete_replace(scrubber(v))))



l_df = data.frame(l_clean, 0)
v_df = data.frame(v_clean, 1)
colnames(l_df) = c("text", "writer")
colnames(v_df) = colnames(l_df)
mixed_df = rbind(l_df,v_df)

polar_commit = polarity(text.var=mixed_df$text, grouping.var=mixed_df$writer)
polar_clean = dplyr::select(polar_commit$all, writer, polarity)
# polar_df = dplyr::select(polar_commit$all, cleaned_names, polarity)

(ggplot()+
  geom_histogram(aes(x=filter(polar_clean, writer==0, polarity!=0)$polarity), color="green")+
  geom_histogram(aes(x=filter(polar_clean, writer==1, polarity!=0)$polarity), color="purple"))
