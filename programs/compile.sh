#!/bin/bash
flite $1 -r6:4:2:1:8 -i1 > $(basename $1 .hs).red
