This is the AMsterdam ALignment GenerAtion MEtatool (amalgame)
This open source tool is part of the EuropeanaConnect project.

To get the correct version of the ClioPatria submodule, run:

shell> git submodule update --init

To setup the amalgame server for the first time, run:

shell> cd startup
shell> ../ClioPatria/configure

To start the server run:
shell> ./run.pl

Authors: Victor, Jan & Jacco (VU University Amsterdam)

matchers/		built-in matchers to find (candidate) correspondences
rankers/		built-in rankers that to (re)rank lists of candidate correspondeces

edoal/			Predicates to simplify reading & writing of the EDAOL mapping format
ontologies/		includes RDF/OWL Schema for http://knowledgeweb.semanticweb.org/heterogeneity/alignment

examples		Examples to demonstrate use of above modules
