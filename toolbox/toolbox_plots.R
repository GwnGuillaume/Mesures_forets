plot_wav_sample_df <- function(df_sample, plot_name){
  wav_channel_name <- str_remove(colnames(df_sample), "time_axis")[2]
  df_sample <- df_sample %>% rename(wav_channel = wav_channel_name)
  # df_sample <- df_sample[!is.na(df_sample$seq1_0.5m), ]
  wav_sample_plt <- ggplot(data = df_sample, aes(x = time_axis, y = wav_channel)) + 
    geom_line(aes(y = wav_channel, colour = plot_name, group = 1, 
                  text = paste("<b><br>Time: </br>", sprintf("%0.5f", time_axis), " s</b>", 
                               "<b><br>Sound pressure: </br>", sprintf('%.2e', wav_channel), " Pa</b>"))) + 
    labs(x = "Time (s)", y = "Sound pressure level (dB)") + 
    theme(legend.position = "none")
    # + guides(color=guide_legend("Signal"))
  return(wav_sample_plt)
}
plot_wav_sample_df_with_filt <- function(df_sample, plot_name){
  wav_sample_plt <- ggplot(data = df_sample, aes(x = time_axis, y = wav_channel)) + 
    geom_line(aes(y = wav_channel, colour = plot_name, group = 1, 
                  text = paste("<b><br>Time: </br>", sprintf("%0.5f", time_axis), " s</b>", 
                               "<b><br>Sound pressure: </br>", sprintf('%.2e', wav_channel), " Pa</b>"))) + 
    labs(x = "Time (s)", y = "Sound pressure (Pa)") + 
    geom_line(aes(y = wav_channel_filt, colour = "Filtered signal", group = 1, 
                  text = paste("<b><br>Time: </br>", sprintf("%0.5f", time_axis), " s</b>", 
                               "<b><br>Sound pressure: </br>", sprintf('%.2e', wav_channel_filt), " Pa</b>"))) + 
    theme(legend.position = "none")
  # + guides(color=guide_legend("Signal"))
  return(wav_sample_plt)
}
ggplot_to_plotly <- function(ggplt){
  ggpltly <- ggplotly(ggplt, tooltip = c("text"))
  return(ggpltly)
}
# plotly_wav_sample <- function(df_sample){
#   wav_sample_plt <- plot_ly(df_sample, type = 'scatter', mode = 'lines') %>% 
#   add_trace(x = ~time_axis, y = ~wav_channel, name = 'Raw signal', 
#             hovertemplate = paste('<i>Time</i>: %{x:.3f} s', 
#                                   '<br><b>Sound pressure</b>: %{y} Pa<br></b>')) %>% 
#   add_trace(x = ~time_axis, y = ~wav_channel_filt, name = 'Filtered signal', 
#             hovertemplate = paste('<i>Time</i>: %{x:.3f} s', 
#                                   '<br><b>Sound pressure</b>: %{y} Pa<br></b>'))
# options(warn = -1)
# wav_sample_plt <- wav_sample_plt %>% 
#   layout(showlegend = TRUE, 
#          xaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff', title = "Time (s)"), 
#          yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff', title = "Sound pressure (Pa)"), 
#          plot_bgcolor='#e5ecf6', # width = 900, 
#          hovermode = "x unified", 
#          updatemenus = list(list(active=0, x = 1.2, xanchor = 'left',y = 0.8, yanchor = 'top', 
#                                  buttons = list(list(method = "restyle", args = list("visible", list(TRUE, TRUE)), label = "Raw and filtered signals"), 
#                                                 list(method = "restyle", args = list("visible", list("legendonly", TRUE)), label = "Raw signal"), 
#                                                 list(method = "restyle", args = list("visible", list(TRUE, "legendonly")), label = "Filtered signal"))
#                                  )
#                             )
#         )
# return(wav_sample_plt)
# }
spectro = function(data, nfft=1024, window=256, overlap=128, t0=0, plot_spec = TRUE, normalize = FALSE, return_data = FALSE){
  # extract signal
  snd <- data@left
  # demean to remove DC offset
  snd <- snd - mean(snd)
  # determine duration
  dur <- length(snd) / data@samp.rate
  # create spectrogram
  spec <- specgram(x = snd, 
                   n = nfft, 
                   fs = data@samp.rate, 
                   window = window, 
                   overlap = overlap)
  # discard phase info
  P <- abs(spec$S)
  # normalize
  if(normalize){
    P <- P/max(P)  
  }
  # convert to dB
  P <- 10*log10(P)
  # config time axis
  if(t0==0){
    t <- as.numeric(spec$t)
  } else {
    if(is.numeric.POSIXt(t0)) {
      t <- as.POSIXct(spec$t, origin = t0)
    } else {
      t <- as.numeric(t0 + spec$t)
    }
  }
  # rename freq
  f <- spec$f
  if(plot_spec){
    # change plot colour defaults
    par(bg = "white")  # "black"
    par(col.lab="black")  # "white"
    par(col.axis="black")  # "white"
    par(col.main="black")  # "white"
    # plot spectrogram
    imagep(t, f, t(P), col = oce.colorsViridis, drawPalette = TRUE, 
           ylab = 'Frequency [Hz]', xlab = 'Time [s]', axes = FALSE)
    box(col = "black")  # "white"
    axis(2, labels = TRUE, col = "black")  # col = "white"
    # add x axis
    if(t0==0 | isFALSE(is.numeric.POSIXt(t0))){
      axis(1, labels = TRUE, col = "black")  # col = "white"
    } else {
      axis.POSIXct(seq.POSIXt(t0, t0+dur, 10), side = 1, format = '%H:%M:%S', col = "black", las = 1)  # col = "white"
      mtext(paste0(format(t0, '%B %d, %Y')), side = 1, adj = 0, line = 2, col = "black")  # col = "white"
    }
  }
  if(return_data){
    # prep output
    spec <- list(t = t, 
                 f = f, 
                 p = t(P))
    return(spec)  
  }
}