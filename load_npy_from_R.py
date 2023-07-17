import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

noise_types = ['background_noise', 'white_noise', 'chirp']
noise_type = noise_types[-1]

raw_data = np.load("./output/df_samples/" + noise_type + "/" + noise_type + ".npy", allow_pickle=True)
# Results dataframe for the 3 sequences and all source-receiver distances
raw_time_data = pd.DataFrame(raw_data).astype(np.float64)
raw_time_data.columns = ["time_axis", "seq1_0.5m", "seq1_1m", "seq1_10m", "seq1_20m", "seq1_30m", "seq1_40m",
                         "seq2_0.5m", "seq2_1m", "seq2_10m", "seq2_20m", "seq2_30m", "seq2_40m", "seq3_0.5m",
                         "seq3_1m", "seq3_10m", "seq3_20m", "seq3_30m", "seq3_40m"]
raw_time_data.columns
plt.close("all")

raw_time_data.plot(x='time_axis', y=[s for s in raw_time_data.columns if 'time_axis' not in s])
plt.show()