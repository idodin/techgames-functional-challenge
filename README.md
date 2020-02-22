# Compete McGill Tech Games Functional Programming Challenge

To Run the container, mount file systems as follows: 

```bash
docker run --rm \
  -v .:/repository:ro \
  -v learn-ocaml-sync:/sync \
  -p 80:8080 --name learn-ocaml-server \
  ocamlsf/learn-ocaml:dev
```
