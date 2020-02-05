#!/bin/sh

mv pmo.json pmo.json.bak
mv pmo_owl.json pmo_owl.json.bak
mv pmo_searchable_terms.tsv pmo_searchable_terms.tsv.bak

wget https://raw.githubusercontent.com/hurwitzlab/planet-microbe-ontology/master/pmo.json
wget https://raw.githubusercontent.com/hurwitzlab/planet-microbe-ontology/master/pmo_owl.json
wget https://raw.githubusercontent.com/hurwitzlab/planet-microbe-ontology/master/pmo_searchable_terms.tsv
