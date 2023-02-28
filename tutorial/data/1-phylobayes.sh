#!/usr/bin/env sh

export afn=strassert-136taxa.phy
export tfn=strassert-136taxa.unrooted.tre
export md=lg

pb -d $afn -T $tfn -ncat 1 -$md -dgam 4 -x 10 10000 "${afn}_${tfn}_${md}+g4"
