dictionary <- read.delim("words_250000_train.txt", header = F)[,1]
# use rcpp to write faster functions
library(stringr)
library(scales)

library(ggplot2)
library(gridExtra)
data1 <- data.frame()
alphabet = strsplit("abcdefghijklmnopqrstuvwxyz", "")[[1]]

for(i in 1:length(alphabet)){
    data1[i,1] = alphabet[i]
    data1[i,2] = sum(str_detect(dictionary, alphabet[i]))
}
colnames(data1) <- c("pattern", "frequency")
data1[,] <- data1[order(data1$frequency, decreasing = T),]

p1 <- ggplot(data1, aes(x = pattern, y = frequency/length(dictionary))) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    scale_x_discrete(limits = data1$pattern) +theme_light()+scale_y_continuous(labels = scales::percent)+
    labs(x = "", y = "",
         title = "Common of 1 Letter Patterns",
         caption = "The letters e,i,a,r, and n occur in more 50% of english words")

# distribution of length of words

WordLength <- ggplot() + geom_histogram(aes(x = nchar(dictionary), y = after_stat(count / sum(count))), binwidth = 1, col = "white", fill = "darkred")+
    theme_light()+ labs(x = "Word Length", y = "Probability",
                        title = "Distribution of Word Length")+
    scale_y_continuous(labels = scales::percent)
ggsave("WordLength.png",plot = WordLength, height = 720, width = 1480, scale = 1, units = 'px')

# Common patterns
split_word <- function(word, l){
    res <- NULL
    n = nchar(word)
    if(l>n){
        return(NULL)
    }
    for(i in 1:(n-l+1)){
        res[i] <- substr(word, i, i+l-1)
    }
    return(res)
}
split_string <- function(words, l = 1){
    res <- NULL
    n <- 0
    for(i in 1:length(words)){
        for(j in split_word(words[i], l)){
            n <- n+1
            res[n] <- j
        }
    }
    res <- as.factor(res)
    res <- table(res)
    return(res)
}
data2 <- data.frame(split_string(dictionary, l = 2))
colnames(data2) <- c("pattern", "frequency")
data2[,] <- data2[order(data2$frequency, decreasing = T),]

p2 <- ggplot(data2[1:20,], aes(x = pattern, y = frequency/length(dictionary))) +
    geom_bar(stat = "identity", fill = "lightblue4") + #coord_flip()+
    scale_x_discrete(limits = data2$pattern[1:20]) +theme_light()+scale_y_continuous(labels = scales::percent)+
    labs(x = "", y = "",
         title = "Common of 2 Letter Patterns",
         caption = "This plot shows the probability of observing 20 most common 2 letter combinations")

data3 <- data.frame(split_string(dictionary, l = 3))
colnames(data3) <- c("pattern", "frequency")
data3 <- data3[order(data3$frequency, decreasing = T),]

p3 <- ggplot(data3[1:15,], aes(x = pattern, y = frequency/length(dictionary))) +
    geom_bar(stat = "identity", fill = "darkorange4") + #coord_flip()+
    scale_x_discrete(limits = data3$pattern[1:15]) +theme_light()+scale_y_continuous(labels = scales::percent)+
    labs(x = "", y = "",
         title = "Common of 3 Letter Patterns",
         caption = "This plot shows the probability of observing 15 most common 3 letter combinations")


data4 <- data.frame(split_string(dictionary, l = 4))
colnames(data4) <- c("pattern", "frequency")
data4 <- data4[order(data4$frequency, decreasing = T),]

p4 <- ggplot(data4[1:15,], aes(x = pattern, y = frequency/length(dictionary))) +
    geom_bar(stat = "identity", fill = "darkorchid4") + #coord_flip()+
    scale_x_discrete(limits = data4$pattern[1:15]) +theme_light()+scale_y_continuous(labels = scales::percent)+
    labs(x = "", y = "",
         title = "Common of 4 Letter Patterns",
         caption = "This plot shows the probability of observing 15 most common 4 letter combinations")

Patterns <- grid.arrange(p1, p2, p3, p4, top = "Analysis of Patterns")
ggsave("Patterns.png",plot = Patterns, height = 1000, width = 1400, scale = 2, units = 'px')



draw_distribution <- function(pattern){
    freq <- numeric(length = 26)
    dat <- NULL
    n = nchar(pattern)
    if(n == 1){
        dat <-  data1
    } else if(n == 2) {
        dat <-  data2
    } else if(n == 3) {
        dat <-  data3
    } else if(n == 4) {
        dat <- data4
    } else return()
    for(i in 1:length(alphabet)){
        p <- str_replace(pattern, "_", alphabet[i])
        freq[i] <- dat$frequency[which(dat$pattern == p)][1]
        if(is.na( freq[i])) freq[i] = 0
    }
    example_data <- data.frame(alphabet,prob =  freq/sum(freq))
    p <- ggplot(example_data, aes(x=alphabet, y=prob))+
        geom_bar(stat = "identity", fill = "bisque4") + #coord_flip()+
        scale_x_discrete(limits = alphabet) +theme_light()+scale_y_continuous(labels = scales::percent)+
        labs(x = "", y = "",
             title = paste(toupper( pattern))
        )
    return(p)
}

probability_plot <- grid.arrange(draw_distribution("y_e"), draw_distribution("ap_"),draw_distribution("g_m"),
             draw_distribution("pl_"), draw_distribution("pne_"),draw_distribution("_ion"),
             draw_distribution("q_"), draw_distribution("_ha"), draw_distribution("tt_")+labs(caption = "These plots show probability of the occurance of a letter in '_' given its neighbours"),
             top = "Conditional Probability Distributions")
ggsave("Prob_plot.png",plot = probability_plot, height = 1000, width = 1400, scale = 2, units = 'px')

# final model
accuracy <- c(0.558, 0.59, 0.627, 0.619, 0.628, 0.624, 0.619, 0.639, 0.635, 0.618, 0.63, 0.648, 0.602, 0.627, 0.629, 0.633, 0.61, 0.593, 0.608, 0.615, 0.581, 0.626, 0.608, 0.606, 0.62, 0.631, 0.611, 0.629, 0.634, 0.633, 0.63, 0.608, 0.608, 0.614, 0.622, 0.639, 0.616, 0.632, 0.635, 0.616, 0.616, 0.637, 0.616, 0.637, 0.628, 0.616, 0.628, 0.641, 0.598, 0.634, 0.629,
              0.64, 0.616, 0.616, 0.625, 0.615, 0.628, 0.648, 0.604, 0.652, 0.641, 0.623, 0.64, 0.663, 0.624, 0.586, 0.646, 0.611,
              0.639, 0.632, 0.638, 0.627, 0.63, 0.631, 0.617, 0.638, 0.6, 0.634, 0.622, 0.633, 0.626, 0.625, 0.609, 0.601, 0.604, 0.638, 0.62, 0.629, 0.616, 0.603, 0.624, 0.608, 0.642, 0.645, 0.628, 0.635, 0.624, 0.622, 0.649, 0.634, 0.605, 0.633,
              0.611, 0.625, 0.624, 0.619, 0.637, 0.628, 0.631, 0.606, 0.626, 0.636, 0.598, 0.641, 0.608, 0.609, 0.635, 0.64, 0.627, 0.63, 0.639, 0.6, 0.621, 0.611, 0.624, 0.628, 0.626, 0.617, 0.64, 0.621, 0.605, 0.615, 0.609, 0.632, 0.615, 0.614, 0.606, 0.596, 0.63, 0.619, 0.655, 0.647, 0.617, 0.626, 0.612, 0.632, 0.642, 0.626, 0.617, 0.614, 0.644, 0.656, 0.651,
              0.625, 0.593, 0.658, 0.642, 0.622, 0.624, 0.618, 0.657, 0.635, 0.627, 0.613, 0.611, 0.591, 0.648, 0.629, 0.631, 0.629, 0.626, 0.61, 0.616, 0.61, 0.641, 0.59, 0.663, 0.644, 0.633, 0.657, 0.644, 0.637, 0.66, 0.628, 0.634, 0.608, 0.619,
              0.604, 0.65, 0.61, 0.628, 0.633, 0.645, 0.61, 0.608, 0.632, 0.631, 0.615, 0.609, 0.628, 0.636, 0.622, 0.618, 0.617, 0.635, 0.628, 0.632, 0.635, 0.613, 0.64, 0.617, 0.636, 0.622, 0.654, 0.622, 0.598, 0.622, 0.637, 0.677, 0.6, 0.601, 0.617, 0.635, 0.597, 0.645, 0.61, 0.643, 0.607, 0.624, 0.631, 0.592, 0.605, 0.655, 0.663, 0.623, 0.642, 0.628, 0.643, 0.616, 0.65, 0.626, 0.611, 0.635, 0.636, 0.621, 0.639, 0.603, 0.612, 0.623, 0.614, 0.597, 0.598, 0.648, 0.607, 0.631,
              0.623, 0.636, 0.62, 0.645, 0.607, 0.63, 0.635, 0.633, 0.645, 0.65, 0.618, 0.626, 0.65, 0.608, 0.6, 0.628, 0.628, 0.617, 0.619, 0.62, 0.627, 0.605, 0.614, 0.634, 0.616, 0.611, 0.623, 0.635, 0.646, 0.617, 0.632, 0.622, 0.628, 0.622, 0.627, 0.63, 0.657, 0.622, 0.623, 0.637, 0.629, 0.621, 0.605, 0.59, 0.651, 0.614, 0.629, 0.586, 0.642, 0.599, 0.625, 0.621, 0.618, 0.615, 0.625, 0.631, 0.616, 0.63, 0.607, 0.626, 0.623, 0.65, 0.612, 0.615, 0.62, 0.629, 0.634, 0.621, 0.623, 0.608, 0.629, 0.659, 0.617, 0.596, 0.637, 0.617, 0.62, 0.622, 0.633, 0.624, 0.605, 0.63, 0.647, 0.628, 0.617, 0.646, 0.63, 0.602, 0.622, 0.646, 0.591, 0.621, 0.612, 0.603, 0.625, 0.6, 0.619, 0.605, 0.615, 0.661, 0.616, 0.645, 0.622, 0.615, 0.607, 0.6, 0.616, 0.649, 0.595, 0.629, 0.639, 0.629, 0.637, 0.62, 0.632, 0.63, 0.626, 0.63, 0.618, 0.614, 0.637, 0.65, 0.646, 0.629, 0.622, 0.649, 0.636, 0.635, 0.649, 0.634, 0.61, 0.603, 0.657, 0.62, 0.63, 0.613, 0.638, 0.603, 0.605, 0.619, 0.609, 0.587, 0.638, 0.62, 0.645, 0.655, 0.589, 0.609, 0.632, 0.62, 0.63, 0.607, 0.63, 0.629, 0.628, 0.64, 0.624, 0.625, 0.611, 0.635, 0.614, 0.62, 0.603, 0.643, 0.62, 0.617, 0.624, 0.637, 0.652, 0.627, 0.613, 0.62,
              0.634, 0.652, 0.616, 0.624, 0.622, 0.615, 0.625, 0.603, 0.62, 0.627, 0.613, 0.662, 0.609, 0.627, 0.627, 0.627, 0.613, 0.608, 0.608, 0.645, 0.635, 0.639, 0.602, 0.625, 0.637, 0.6, 0.631, 0.628, 0.617, 0.637, 0.612, 0.589, 0.647, 0.622, 0.597, 0.608, 0.634, 0.63, 0.632, 0.621, 0.59, 0.622, 0.636, 0.64, 0.645, 0.623, 0.633, 0.633, 0.624, 0.639, 0.616,
              0.607, 0.62, 0.613, 0.626, 0.584, 0.611, 0.667, 0.653, 0.622, 0.639, 0.607, 0.62, 0.63, 0.612, 0.614, 0.617, 0.628, 0.602, 0.622, 0.63, 0.59, 0.614, 0.651, 0.613, 0.614, 0.613, 0.625, 0.588, 0.612, 0.631, 0.636, 0.628, 0.647, 0.621, 0.626, 0.622, 0.666, 0.626, 0.641, 0.631, 0.626, 0.608, 0.638, 0.629, 0.617, 0.611, 0.628, 0.625, 0.644, 0.604, 0.61,
              0.627, 0.642, 0.635, 0.616, 0.606, 0.605, 0.609, 0.628, 0.629, 0.651, 0.63, 0.618, 0.643, 0.597, 0.629, 0.625, 0.638, 0.596, 0.606, 0.634, 0.647, 0.633, 0.642, 0.65, 0.623, 0.623, 0.633, 0.663, 0.617, 0.62, 0.605, 0.632, 0.623, 0.629, 0.631, 0.624, 0.635, 0.611, 0.639, 0.641, 0.631, 0.631, 0.594, 0.623, 0.601, 0.638, 0.62, 0.639, 0.613, 0.626, 0.614, 0.626, 0.644, 0.629, 0.638, 0.616, 0.646, 0.615, 0.603, 0.605, 0.629, 0.628, 0.624, 0.617, 0.626, 0.599, 0.633, 0.626, 0.614, 0.621, 0.58, 0.614, 0.637, 0.649, 0.637, 0.618, 0.623, 0.631, 0.618, 0.632, 0.62, 0.623, 0.626, 0.618, 0.626, 0.631, 0.63, 0.625, 0.638, 0.632, 0.637, 0.622, 0.636, 0.599, 0.606, 0.651, 0.608, 0.61, 0.6, 0.625, 0.634, 0.617, 0.652, 0.633, 0.596, 0.592, 0.629, 0.618, 0.625, 0.616, 0.617, 0.569, 0.639, 0.611, 0.631, 0.598, 0.621, 0.64, 0.616, 0.609, 0.644, 0.634, 0.638, 0.623, 0.651, 0.618, 0.64, 0.628, 0.609, 0.621, 0.63, 0.63, 0.607, 0.643, 0.628, 0.619, 0.604, 0.616, 0.598, 0.616, 0.632, 0.633, 0.617, 0.647, 0.611, 0.636, 0.636, 0.616, 0.62, 0.645, 0.623, 0.623, 0.626, 0.65, 0.622, 0.662, 0.589, 0.615, 0.601, 0.605, 0.622, 0.623, 0.612, 0.573, 0.631, 0.657, 0.647, 0.652, 0.618, 0.624, 0.638, 0.621, 0.623, 0.635, 0.614, 0.616, 0.64, 0.613, 0.637, 0.611, 0.631, 0.618, 0.633, 0.63, 0.613, 0.635, 0.621, 0.635, 0.631, 0.644, 0.66, 0.608, 0.622, 0.615, 0.641, 0.627, 0.6, 0.633, 0.611, 0.634, 0.613, 0.621, 0.621, 0.656, 0.634, 0.661, 0.617, 0.634, 0.618, 0.608, 0.634, 0.604, 0.612, 0.618, 0.609, 0.616, 0.63, 0.623, 0.644, 0.636, 0.608, 0.608, 0.625, 0.646, 0.622, 0.605, 0.631, 0.611, 0.633, 0.639, 0.636, 0.639, 0.625, 0.595, 0.628, 0.65, 0.632, 0.631, 0.612, 0.603, 0.62, 0.612, 0.616, 0.587, 0.636, 0.666, 0.631, 0.639, 0.617, 0.616, 0.598, 0.606, 0.633, 0.619, 0.598, 0.619, 0.644, 0.621, 0.636, 0.633, 0.63, 0.637, 0.644, 0.618, 0.628, 0.613, 0.632, 0.62, 0.603, 0.596, 0.632, 0.614, 0.631, 0.621, 0.609, 0.639, 0.604, 0.63, 0.609, 0.609, 0.604, 0.613, 0.619, 0.619, 0.624, 0.624, 0.615, 0.615,
              0.627, 0.631, 0.622, 0.64, 0.64, 0.629, 0.64, 0.618, 0.627, 0.652, 0.617, 0.617, 0.635, 0.613, 0.62, 0.638, 0.626, 0.631, 0.652, 0.622, 0.624, 0.619, 0.62, 0.626, 0.641, 0.618, 0.604, 0.643, 0.612, 0.627, 0.644, 0.614, 0.637, 0.598, 0.619, 0.622, 0.605, 0.63, 0.605, 0.634, 0.629, 0.633, 0.606, 0.629, 0.647, 0.632, 0.632, 0.624, 0.634, 0.595, 0.606,
              0.617, 0.626, 0.625, 0.641, 0.634, 0.648, 0.631, 0.641, 0.625, 0.651, 0.627, 0.592, 0.615, 0.634, 0.635, 0.624, 0.619, 0.598, 0.626, 0.622, 0.615, 0.633, 0.642, 0.633, 0.621, 0.618, 0.619, 0.616, 0.639, 0.645, 0.641, 0.648, 0.605, 0.608, 0.615, 0.616, 0.637, 0.636, 0.627, 0.638, 0.625, 0.609, 0.609, 0.641, 0.645, 0.626, 0.62, 0.627, 0.623, 0.623, 0.646, 0.626, 0.637, 0.642, 0.624, 0.62, 0.639, 0.613, 0.614, 0.616, 0.639, 0.605, 0.616, 0.613, 0.595, 0.647, 0.624, 0.632, 0.625, 0.632, 0.615, 0.631, 0.59, 0.61, 0.637, 0.639, 0.62, 0.601, 0.589, 0.624, 0.622, 0.596, 0.637, 0.62, 0.62, 0.616, 0.615, 0.633, 0.62, 0.624, 0.625, 0.621, 0.61, 0.629, 0.645, 0.659, 0.642, 0.617, 0.652, 0.631, 0.615, 0.617, 0.632, 0.595, 0.628, 0.645, 0.617, 0.631, 0.612, 0.614, 0.613, 0.63, 0.616, 0.63, 0.644, 0.62, 0.641, 0.639, 0.618, 0.632, 0.601, 0.627, 0.635, 0.627, 0.63, 0.634, 0.642, 0.615, 0.632, 0.614, 0.618, 0.612)-0.02

CV <- ggplot(data.frame(accuracy), aes(x = accuracy)) +
    geom_histogram(binwidth = 0.005, col = 'white', fill = "darkcyan")+
    theme_light()+ labs(x = "Accuracy", y = "Frequency",
                        title = "Distribution of Cross Validation Accuracy (1000 Samples)",
                        caption = paste("Probability( Accuracy < " , quantile(accuracy, 0.05)*100, "% ) = 0.05"))+
    scale_x_continuous(labels = scales::percent)+
    annotate("text", x = 0.57, y = 130, label = paste("Mean = ", round(mean(accuracy)*100),"%"))+
    annotate("text", x = 0.567, y = 120, label = paste("SD = ", round(sd(accuracy)*100),"%"))

ggsave("CV.png",plot = CV, height = 900, width = 1500, scale = 1.5, units = 'px')


# Distribution of length of word for which the algo failed, in 10 sets of 1000 games
Bootstrap_data <- t(data.frame(read.delim("Bootstrap.txt", sep = ',', header =  F)))[,1]

Bootstrap_data <- data.frame("word_length" = Bootstrap_data[2*(1:10000)-1],"attempts" = Bootstrap_data[2*(1:10000)])

Prob_givenlength <- numeric(max(Bootstrap_data$word_length))
for(i in 3:20){
    Prob_givenlength[i] = sum(Bootstrap_data$word_length == i & Bootstrap_data$attempts<6)/sum(Bootstrap_data$word_length==i)
}
q <- ggplot() + geom_bar(aes(x = 3:20, y = Prob_givenlength[3:20]) ,stat = "identity", fill = "darkolivegreen")+
    theme_light()+labs(x = "Word Length", y = "Probability",
                       title= "Probability ( Success | Length of word )")+
    scale_y_continuous(labels = scales::percent)

ggsave("successrate.png",plot = q, height = 900, width = 1500, scale = 1.5, units = 'px')
