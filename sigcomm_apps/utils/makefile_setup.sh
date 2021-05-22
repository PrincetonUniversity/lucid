#setup dirs

for dir in apps/*; do [ -d "$dir" ] && 
    cp utils/app_makefile $dir/makefile
done