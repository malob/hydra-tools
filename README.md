# hydra-tools

Early WIP package for extracting useful information from [Hydra](https://hydra.nixos.org).

Executable currently only dumps JSON of the `EvalInfo` of a jobset evaluation, which includes problematic dependencies.

```shell
❯ nix run . -- 1785043 > evalinfo.json # takes ~8 mins
Collecting build information ...
Finding problematic dependencies ...

❯ cat evalinfo.json | jq
{
  "id": 1785043,
  "builds": {
    ...
    "197545751": {
      "status": "Succeeded",
      "id": 197545751,
      "job": "emacsPackages.gsettings.aarch64-darwin",
      "stoptime": 1667511372,
      "nixname": "emacs-gsettings-20210407.2045",
      "system": "aarch64-darwin"
    },
    "197545752": {
      "status": "Dependency failed",
      "id": 197545752,
      "job": "python310Packages.delorean.aarch64-darwin",
      "stoptime": 1667446743,
      "nixname": "python3.10-Delorean-1.0.0",
      "system": "aarch64-darwin"
    },
    "197545753": {
      "status": "Succeeded",
      "id": 197545753,
      "job": "xcpretty.aarch64-darwin",
      "stoptime": 1667499672,
      "nixname": "xcpretty-0.3.0",
      "system": "aarch64-darwin"
    },
    ...
  },
  "problemdeps": {
    "196768182": [
      197581319
    ],
    "197078638": [
      197559962
    ],
    "197124145": [
      197573925
    ],
    "197198562": [
      197545752,
      ...
      197952260
    ],
    ...
  }
}
```
