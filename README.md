# Service Quality Monitoring in Confined Spaces Through Mining Twitter Data
The proposed method comprises of two main tasks, namely Aspect Extraction and Detecting Events of Interest.

![Flowchart](https://raw.githubusercontent.com/mmrahimi/sq_monitoring_ae/master/Image/flowchart.png)

# Detecting Events of Interest (DEoI)
## Introduction 
This is an implementation of Frequency and Sentiment-based Event Detection (FSED) in the context of service quality of public transport. This repository compares the result of the proposed method with the following state-of-the-art time-series-based event detection approaches:

- [Bivariate Outlier Detection](https://github.com/cran/aplpack)
- [Seasonal Hybrid ESD (S-H-ESD)](https://github.com/twitter/AnomalyDetection) 
- [TSOutliers](https://github.com/cran/tsoutliers)
- [Anomalize](https://github.com/business-science/anomalize)
- [Long Short Term Memory (LSTM)](https://github.com/rwanjohi/Time-series-forecasting-using-LSTM-in-R)


## Dataset
In this project, two major transport hubs are considered as the case studies due to their current importance on transferring a large number of people. First, a Twitter dataset comprising of more than 32 million tweets is collected. This data is obtained from the Australian Urban Research Infrastructure Network ([AURIN](www.aurin.org.au)). Keywords and spatial proximity to hubs are employed to detect relevant tweets. 

Next, tweets are manually labelled and mapped to different aspects of SQ (``Safety, View, Information, Service Reliability, Comfort, Personnel, and Additional Services``). Those tweets that do not fall into any of these aspects are considered as irrelevant to the SQ of public transport and therefore, are discarded (Class -1). 

Finally, a list of events happened inside of SCS are manually-labeled for the study period. The resulted events then get verified using a list of events provided from SCS authorities as a ground-truth. 

## Requirements
This code was tested on R version 3.5.3. Other required packages are as follows: 

- pacman
- dplyr
- magrittr
- lexicon
- fitdistrplus
- AnomalyDetection
- aplpack
- tidyverse
- anomalize
- xlsx
- reticulate
- fBasics
- keras
- tsoutliers
- rstudioapi
- parsedate   

(See [requirements.txt](https://github.com/mmrahimi/Detecting_events_of_interest/blob/master/requirements.txt))
 
## Citation
```
Rahimi, M.M., Naghizade, E., Stevenson M., Winter, S., Service Quality Monitoring in Confined Spaces Through Mining Twitter Data In Journal of Spatial Information Science.
```

## Author
Masoud Rahimi
- Email: <mmrahimi@student.unimelb.edu.au>
- Linkedin: [Here](https://www.linkedin.com/in/rahimimasoud/)
