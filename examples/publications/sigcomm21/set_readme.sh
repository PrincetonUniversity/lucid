# fill the patch date and commit hash info in the readme
sed -i '' -e "s/\[PATCH_DATE\]/$(date +'%B %d, %Y')/" README.md
sed -i '' -e "s/\[COMMIT_HASH\]/$(git rev-parse HEAD)/" README.md