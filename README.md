# hydra-tools

Early WIP package for extracting useful information from [Hydra](https://hydra.nixos.org).

Executable currently only dumps JSON of the `EvalInfo` of a jobset evaluation, which includes problematic dependencies.
```bash
stack run 1785043 > evalinfo.json # takes ~8 mins
cat evalinfo.json | jq
```
