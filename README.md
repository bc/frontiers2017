# frontiers2017

# Installation

# Put dataset on the EC2 Server:
via https://www.rstudio.com/products/rstudio/download-server/
```
git config --global core.editor "vim"
sudo apt-get -y install gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.0.153-amd64.deb
yes | sudo gdebi rstudio-server-1.0.153-amd64.deb
sudo rstudio-server verify-installation
sudo rstudio-server license-manager status
```

# Enter information for a new user with a simple password
```
sudo adduser brian
#then log in as the user after setting a password
sudo su - brian
mkdir Resilio\ Sync
cd Resilio\ Sync && mkdir data && cd ..
```
# Import the data to local AWS disk
```
scp -i ~/.ssh/cohn_frontiers2017.pem ~/Resilio\ Sync/data/realTimeData2017_08_16_13_23_42.txt  ubuntu@ec2-54-241-165-222.us-west-1.compute.amazonaws.com:/home/ubuntu
```
on aws CPU
```
sudo mv realTimeData2017_08_16_13_23_42.txt /home/brian/Resilio\ Sync/data/realTimeData2017_08_16_13_23_42.txt
git clone git@github.com:bc/frontiers2017 && cd frontiers2017/figures/
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
