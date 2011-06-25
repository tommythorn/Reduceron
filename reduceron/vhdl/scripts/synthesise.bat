sed s/CHANGEME/%1/ template.prj > top.prj
sed s/CHANGEME/%1/ template.vhd > top.vhd
xst -intstyle ise -ifn top.xst -ofn top.syr
ngdbuild -intstyle ise -dd _ngo -uc base.ucf -p xc2v2000-6-bf957 top.ngc top.ngd
map -intstyle ise -p xc2v2000-6-bf957 -cm area -pr b -k 4 -c 100 -tx off -o top_map.ncd top.ngd top.pcf
par -intstyle ise -w -ol std -t 1 top_map.ncd top.ncd top.pcf
trce -intstyle ise -e 3 -l 3 -xml top top.ncd -o top.twr top.pcf
bitgen -intstyle ise -f top.ut top.ncd
copy top.bit %1.bit
