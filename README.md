# GenePiper

##### GenePiper is a standalone R shiny application for NGS data mining. It provides over 20 different analysis methods with user-friendly GUI. Recently, 6 main categories (Diversity, Descriptive, Oridination, Correlation, Clustering and Non-parametric) of analytic modules were included.


### Installation

---

### Software requirements:
* [VirtualBox](https://www.virtualbox.org)
* [Vagrant](https://www.vagrantup.com)


### Installation:
1. Download the [Vagrantfile](https://github.com/raytonghk/genepiper/blob/master/Vagrantfile) (press ALT + click "Raw" at top right corner)
2. Create a working directory and copy the `Vagrantfile` into it.
3. If the `Vagrantfile` contains any extension, e.g. `.txt`, remove it.
4. Run `vagrant up` in the working directory in terminal (Linux / Mac) or command prompt (Windows).
5. After installation, use any web browser (such as Chrome, Firefox, Safari) to host machine: type in the address bar `http://localhost:4002`.

Installation involves building a virtual environment, installing the operation system (linux), all the essential tools and packages, R Shiny server setup and configuration of the environment. This one-time process may take over 30 minutes.

### Sample data:

Random generated sample data is provided in `sampleData` folder which contains 4 files. 
`otuTableSample.tsv` is a tab separated OTU table file with 100 samples and 2000 taxa. 
`taxTableSample.tsv` is a tab separated Taxonomy table file with 2000 taxa and 7 taxonomical rank.
`samDataSample.tsv` is a tab separated sample mapping file with 100 samples and 12 feature columns.
`treeSample.tre` is a phylogenetic tree file with 2000 taxa.

### Tutorials:

For detailed walkthrough of each modules, please refer to [Wiki](https://github.com/raytonghk/genepiper/wiki/01.-Introduction) page.
