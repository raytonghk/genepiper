# GenePiper

##### GenePiper is a standalone R shiny application for NGS data mining. It provides over 20 different analysis methods with user-friendly GUI. Recently, 6 main categories of analytic modules  (Diversity, Descriptive, Oridination, Correlation, Clustering and Non-parametric test) are included.


### Installation

---

### Software requirements:
* [VirtualBox](https://www.virtualbox.org)
* [Vagrant](https://www.vagrantup.com)


### Installation:
1. Download the [Vagrantfile](https://github.com/raytonghk/genepiper/blob/master/Vagrantfile): press ALT (or option key in Apple keyboard) + click "Raw" at the top right corner
2. Create a working directory and copy the `Vagrantfile` into it.
3. If the `Vagrantfile` contains any extension, e.g. `.txt`, remove it.
4. Run `vagrant up` in the working directory in terminal (Linux / Mac) or command prompt (Windows).
5. After installation, use any web browser (such as Chrome, Firefox, Safari) to host machine: type in the address bar `http://localhost:4002`.

Installation involves building a virtual environment, installing the operation system (linux), all the essential tools and packages, R Shiny server setup and configuration of the environment. This one-time process may take over 30 minutes.


### To update:
We are actively updating and improving GenePiper, users are advised to update the package time to time for new patches and newly added functions. To update GenePiper, click on the "Update Genepiper" button when you first launch the package in your browser.

### Sample data:

Sample data `GlobalPatterns` from `phyloseq` package is provided in RDS format `gp.rds` in the `sampleData` folder.  Individual table files had been extracted as sample data:
* `gpOtu.tsv` is a tab separated OTU table file with 26 samples and 19216 taxa.
* `gpTax.tsv` is a tab separated Taxonomy table file with 19216 taxa and 7 taxonomical rank.
* `gpSamData.tsv` is a tab separated sample data (mapping) file with 26 samples and 7 feature columns.
* `gpTree.tre` is a phylogenetic tree file with 19216 taxa.

Reference:

Caporaso, J. G., et al. (2011). Global patterns of 16S rRNA diversity at a depth of millions of sequences per sample. PNAS, 108, 4516-4522. PMCID: PMC3063599

Biom sample file `rich_dense_otu_table.biom` is also provided.

### Tutorials:

For detailed walkthrough of each module, please refer to our [Wiki](https://github.com/raytonghk/genepiper/wiki/01.-Introduction) page.

### Future updates:
* New import module that supports QIIME2 files (in progress)
* Area-proportional venn diagram (maybe?)
* Differential abundances by DeSeq2 (maybe?)
* Animated graphics: taxonomic bar-charts and ordinations (maybe?)
* Expansion to support raw sequence processing (maybe?)


### Citation:

Please cite GenePiper as follows: "Tong WM, Chan Y. 2020. GenePiper: a graphical user interface tool for microbiome sequence data mining. Microbiol Resour Announc 9:e01195-19. https://doi.org/10.1128/MRA.01195-19."
