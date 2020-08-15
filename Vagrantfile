Vagrant.configure("2") do |config|

  config.vm.box = "ubuntu/trusty64"
  config.vm.box_url = "https://vagrantcloud.com/ubuntu/trusty64"

  config.vm.network "forwarded_port", guest: 3838, host: 4002

  config.vm.provider "virtualbox" do |vb|

    vb.memory = "4096"
  end

$script = <<BOOTSTRAP
  export LANGUAGE=en_US.UTF-8
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8
  locale-gen en_US.UTF-8
  sudo dpkg-reconfigure locales 
  sudo echo "deb https://cloud.r-project.org/bin/linux/ubuntu trusty-cran35/" >> /etc/apt/sources.list
  sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
  sudo apt-get update
  sudo apt-get -y install r-base r-base-dev
  sudo apt-get -y install libX11-dev freeglut3 freeglut3-dev
  sudo apt-get -y install libssl-dev
  sudo apt-get -y install libcurl4-openssl-dev
  sudo apt-get -y install libxml2-dev
  sudo apt-get -y install git-core

  sudo R -e "install.packages('shiny', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('tidyverse', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('BiocManager', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "BiocManager::install('phyloseq')"
  sudo R -e "install.packages('shinyFiles', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('shinyjs', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('phangorn', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('DT', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('VennDiagram', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('ggrepel', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('plotly', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('pvclust', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('GUniFrac', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('dendextend', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('Hmisc', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('ggforce', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "BiocManager::install('remotes')"
  sudo R -e "BiocManager::install('zdk123/SpiecEasi')"
  sudo R -e "install.packages('visNetwork', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('bsplus', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('MathJax', repos = 'http://cran.rstudio.com/', dep = TRUE)"
  sudo R -e "install.packages('ggsignif', repos = 'http://cran.rstudio.com/', dep = TRUE)"

  sudo apt-get -y install gdebi-core
      # Install the latest SSO build
  wget "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb" -O ss-latest.deb  
  sudo gdebi -n ss-latest.deb
  rm *.deb

  sudo usermod -a -G vagrant shiny
  sudo usermod -aG sudo shiny
  sudo chmod -R 777 /srv/shiny-server
  sudo mkdir /srv/shiny-server/data -m 777
  
  git clone https://github.com/raytonghk/genepiper.git /srv/shiny-server/genepiper
  sudo mv /srv/shiny-server/genepiper/* /srv/shiny-server
  sudo rm -rf /srv/shiny-server/genepiper
  
  sudo usermod -g root vagrant
  sudo usermod -aG vagrant,vboxsf vagrant
  ln -s /media /srv/shiny-server/extdata

  sudo chmod -R 777 /srv/shiny-server
  
BOOTSTRAP
  
  #config.vm.provision :shell, :inline => $script
  
  config.vm.provision "setup", type: "shell" do |setup|
    setup.inline = $script
  end
  
$fix = <<BOOTSTRAP
  sudo chmod -R 777 /srv/shiny-server
  git clone https://github.com/raytonghk/genepiper.git /srv/shiny-server/genepiper
  sudo cp -R /srv/shiny-server/genepiper/* /srv/shiny-server/
  sudo rm -rf /srv/shiny-server/genepiper
BOOTSTRAP

  config.vm.provision "fix", run: "never", type: "shell" do |fix|
    fix.inline = $fix
  end
  
$fix2 = <<BOOTSTRAP
  sudo usermod -g root vagrant
  sudo usermod -aG vagrant,vboxsf vagrant
  ln -s /media /srv/shiny-server/extdata
BOOTSTRAP

  config.vm.provision "fix2", run: "never", type: "shell" do |fix2|
    fix2.inline = $fix2
  end
  
  
end










