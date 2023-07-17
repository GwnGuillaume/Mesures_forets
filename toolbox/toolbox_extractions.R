# Generate white noise and return wav object
gen_white_noise <- function(sig_len, f_samp, out_wav_filename){
  gen_wav_wn <- savewav(wave = noisew(f = f_samp, d = sig_len/f_samp, type = "unif", listen = TRUE, output = "Wave"), f = f_samp, channel = 1, filename = out_wav_filename)
  return(readWave(filename = out_wav_filename))
}

# Extract wav sample objects for all distances
extract_wav_samples_for_seq <- function(filename, timeline, seq_id, t.beg, t.end, noise_type){
  seq_timeline = timeline[[paste0('timeline', seq_id)]]
  wav_samples <- sapply(seq_timeline, 
                        function(t) readWave(filename, from = t + t.beg , to = t + t.end, units = 'seconds') )
  names(wav_samples) <- paste(timeline$distance, "m")
  # Save the audio samples for this sequence at all distances in wav files
  sapply(seq(1, length(seq_timeline)), 
         function(dist_id) savewav(wave = wav_samples[[dist_id]], f = fs_rec, channel = 1, 
                                   filename = paste0("./output/wav_samples/", noise_type, "/", noise_type, "_seq_", seq_id, "_dist_", timeline[dist_id, 1], "m.wav")))
  return(wav_samples)
}
# Extract left channel signals for all distances from the wav objects
extract_left_channel_for_seq <- function(wav_samples, timeline, seq_id, t.beg, t.end){
  bytes = wav_samples[[1, 1]]@bit
  wav_left_channel <- sapply(seq(1, nrow(wav_samples)), 
                             function(dist_id) extractWave(wav_samples[[dist_id, seq_id]], from=t.beg, to=t.end, xunit = "time")@left / 2^(bytes-1))
  colnames(wav_left_channel) <- paste0('seq', seq_id, "_", timeline[, 1], 'm')
  return(wav_left_channel)
}
# Extract wav sample objects for all measurement sequences
extract_wav_samples_for_all_seqs <- function(filename, timeline, t.beg, t.end, noise_type){
  wav_samples = list()
  for (id_seq in seq(1, ncol(timeline)-1)){
    wav_sample <- extract_wav_samples_for_seq(filename = filename, timeline = timeline, seq_id = id_seq, t.beg, t.end, noise_type)
    wav_samples <- cbind(wav_samples, wav_sample)
  }
  seqs_names <- paste0("seq", seq(1, ncol(timeline)-1))
  colnames(wav_samples) <- seqs_names
  return(wav_samples)
}
# Extract left channel signals for all measurement sequences from the wav objects
extract_wav_samples_channel_for_all_seqs <- function(wav_samples, timeline, fs_rec, t.beg, t.end, noise_type){
  if (exists('wav_samples_channel') && is.data.frame(get('wav_samples_channel'))){
    remove(wav_samples_channel)
  }
  seqs_names <- paste0("seq", seq(1, ncol(timeline)-1))
  col_names <- str_split(str_flatten(sapply(seq(1, ncol(timeline)-1), function(seq_id) paste0(seqs_names[seq_id], "_", timeline$distance, "m", collapse = " ")), 
                                     collapse = " "), pattern = " ")[[1]]
  for (ids in seq(seqs_names)){
    seq_col_names <- col_names[str_detect(string = col_names, pattern = seqs_names[ids])]
    seq_data <- extract_left_channel_for_seq(wav_samples = wav_samples, timeline = timeline, seq_id = ids, t.beg = t.beg, t.end = t.end)
    if (isFALSE(exists('wav_samples_channel') && is.data.frame(get('wav_samples_channel')))){
      wav_samples_channel <- data.frame(matrix(ncol = length(seq_col_names)+1, nrow = nrow(seq_data)))
      colnames(wav_samples_channel) <- c("time_axis", seq_col_names)
      wav_samples_channel$time_axis = seq(from = t.beg, length.out = nrow(seq_data), by = 1/fs_rec)
    }
    for (idc in seq(length(seq_col_names))){
      wav_samples_channel[[paste0(seq_col_names[idc])]] <- seq_data[, idc]
    }
  }
  return(wav_samples_channel)
}
# Audio player
player_audio <- function(wav_filename, player_title){
  html_text <- paste0('<figure>',
                      '    <figcaption id="test_audio">', player_title, '</figcaption>',
                      '    <audio',
                      '        controls',
                      '        src="', wav_filename, '">',
                      '    </audio>',
                      '</figure>')
  player <- htmltools::HTML(text = html_text)
  return(player)
}