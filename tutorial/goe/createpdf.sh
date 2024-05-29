#!/bin/bash

cat <<EOF > tutorialtopdf.md
---
numbersections: true
title: A geological timescale for bacterial evolution and oxygen adaptation -- an mcmc-date tutorial
author: Lenard L. Szantho

header-includes:
 - \lstset{breaklines = true, postbreak = \mbox{$\hookrightarrow$}, basicstyle = \small\ttfamily}
 - \usepackage[in]{fullpage}
---
EOF

sed 's/A geological timescale for bacterial evolution and oxygen adaptation -- an mcmc-date tutorial/Introduction/' tutorial_goe.md | sed 's/^##/#/' >> tutorialtopdf.md

pandoc -i tutorialtopdf.md -o tutorial_goe.pdf --listings -V colorlinks=true -V linkcolor=blue -V urlcolor=red -V toccolor=gray

rm tutorialtopdf.md

