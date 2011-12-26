#!/bin/bash

ls *.hs | xargs -n 1 ./compile.sh
