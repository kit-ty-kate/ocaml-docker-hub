v0.2.0 (03/01/2024)
-------------------

- Large API rework: Add support for fetching single manifests, config, rootfs, ... (#1)
- Remove the dependency on `lwt_ppx`

v0.1.1 (08/03/2023)
-------------------

- Add support for http-lwt-client >= 0.2.0 (#3)
- Add support for requesting default images (e.g. "alpine", aka. "library/alpine")
- Fix handling of requests for some images (those returning application/vnd.docker.distribution.manifest.v2+json)

v0.1.0 (11/01/2022)
-------------------

- `Docker_hub` contains a simple API to fetch image digests from docker hub
