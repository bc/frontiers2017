# frontiers2017

# Installation

# Put dataset on the EC2 Server:
via https://www.rstudio.com/products/rstudio/download-server/
```
git config --global core.editor "vim"
sudo apt-get update
sudo apt-get -y install r-base libssl-dev libcurl4-openssl-dev
sudo apt-get install r-base libapparmor1 gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.0.153-amd64.deb
yes | sudo gdebi rstudio-server-1.0.153-amd64.deb
sudo rstudio-server verify-installation
sudo rstudio-server license-manager status
sudo apt-get -y awscli
```

# Enter information for a new user with a simple password
```
sudo adduser brian
#then log in as the user after setting a password
sudo su - brian
sudo mkdir -p /home/brian/Resilio\ Sync/data
sudo aws s3 cp s3://bc-frontiers-2017/realTimeData2017_08_16_13_23_42.txt realTimeData2017_08_16_13_23_42.txt
```
# Import the data to local AWS disk
```
aws s3 cp ~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds s3://bc-frontiers-2017/
```
on aws CPU
```
git clone git@github.com:bc/frontiers2017.git && cd frontiers2017/analytics
rscript main.r
```

```
scp -r ubuntu@ec2-54-215-246-21.us-west-1.compute.amazonaws.com:/home/ubuntu/output ~/Downloads
```
# TODO
pick suggested reviewers

12k words including citations, but excluding abstract, igure captions, and funding statements, and acknowledgement and references. abstract 350 words, must have running title <= 5 words
All figures:
RGB TIFF format
300DPI


MUST indicate number of words and number of figures on the first page of the manuscript

Title Case for all headings, up to 5 indentation levels of headings allowed.
New paragraphs will be separated with a single empty line

Consider also giving a list of non-standard abbreviations at the end, immediately before the Acknowledgments.

minimum width 8.5cm, max width 20cm

Introduction has no subheadings
Results has no footnotes
Discussion:
Key findings
prior art to place the novelty in context
potential shortcomings and limitations on interpretations
integration into current understanding of the problme
how it advances current views
speculate on future direction of research
freely postulate theories that can be tested in the future

Include website URL's as footnotes

upload the 35.7GB file as supplmentary material

figs + tables num max is 15

#Kian TODO
register here:
https://www.frontiersin.org/Registration/Register.aspx
