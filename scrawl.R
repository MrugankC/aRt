
# Preliminaries -----------------------------------------------------------

# load packages
library(tidyverse)
library(ambient)
library(scico)
library(here)

# parameters
art_par = list(
  
  seed = 2,
  n_paths = 500,
  n_steps = 80,
  sz_step = 200,
  sz_slip = 70
  
)



# Setting up the Canvas ---------------------------------------------------

# set seed for generaing psuedo random numbers
set.seed(seed = art_par$seed)

# Creating the state of the drawing
state <- tibble(
  
  x = runif(n = art_par$n_paths,min = 0,max = 2),
  y = runif(n = art_par$n_paths,min = 0,max = 2),
  z = 0
)


# Including the path_id and step_id
state <- state %>% 
          mutate(
            path_id = 1 : art_par$n_paths,
            step_id = 1
          )




# Track the series of states
art_dat <- state



# Create the art in a loop ------------------------------------------------
stop_painting <- FALSE

while(stop_painting == FALSE) {
  
  # make a step
  step <- curl_noise(generator = gen_simplex,
                     x = state$x,
                     y = state$y,
                     z = state$z,
                     seed = c(1,1,1) * art_par$seed)
  
  
  # do some painting
  state <- state %>% 
            mutate(
              x = x + ((step$x / 10000) * art_par$sz_step),
              y = y + ((step$y / 10000) * art_par$sz_step),
              z = z + ((step$z / 10000) * art_par$sz_slip),
              step_id = step_id + 1
            )
  
  # append state to art_dat
  art_dat <- bind_rows(art_dat,state)
  
  
  # stop painting if number of steps >= value in art_par
  if(last(state$step_id) >= art_par$n_steps){
    
    stop_painting <- TRUE
  }
  
}


# draw the picture --------------------------------------------------------

pic <- ggplot(data = art_dat,mapping = aes(x = x, y = y, group = path_id, color = "turquoise")) +
  geom_path(size = 0.5,
            alpha = 0.5) +
  coord_equal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
pic




# Save the file -----------------------------------------------------------

filename <- "scrawl.png"
ggsave(filename = filename,
       plot = pic,
       width = 10,
       height = 10,
       dpi = 300,
       device = "png",
       path = here())
