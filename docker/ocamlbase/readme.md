This builds a docker image with all the libraries 
required to run the lucid interpreter / compiler. 
The latest version is hosted on dockerhub at: 
jsonch/lucid:ocamlbase.

After updating the image, rebuild and push 
to dockerhub with:

```
docker build -t jsonch/lucid:ocamlbase .
docker push jsonch/lucid:ocamlbase
```