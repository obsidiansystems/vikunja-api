# Updating the API

Get the OpenAPI docs json file from the Vikunja API doc site. Let's say it's called `docs-0.99.9.json`.

```bash
nix-build default.nix --arg specFile './docs-0.99.9.json'

cp -r result/* ../
```
