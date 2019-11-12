# Interactive spherical geometry in the web browser.

A real time editor for geometric constructions on the sphere.

# [Try it online](https://sphere.erdos.dev/)

The project is a successor of [Sphaerica, the Interactive Spherical Geometry Software](https://github.com/erdos/sphaerica). I started working on the current implementation in 2014. The goal of the project is to provide an interactive virtual environment for spherical geometry constructions. It is a tought experiment on DSL development, spherical geometry, functional programming and other fascinating areas.

[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/erdos/stencil/issues)
[![HitCount](http://hits.dwyl.io/erdos/sphere.svg)](http://hits.dwyl.io/erdos/sphere)
[![EPL 2.0](https://img.shields.io/badge/License-EPL%202.0-red.svg)](https://www.eclipse.org/legal/epl-2.0/)


## Contribution

Feel free to reach out with bug reports or new ideas. Contact with direct message, or open an Issue.


### Development

1. Install [Boot](https://boot-clj.com/) for Clojure.
2. To get started in development mode, type: `boot dev`
3. Open the browser window at `http://localhost:8000/index.html` for a live view of the project.

Alternatively, you can use Docker Compose to run a development session: `$ docker-compose -f dev.yml up`

The project is written in [ClojureScript](https://github.com/clojure/clojurescript). It uses React (with [Reagent](https://reagent-project.github.io/)) to render the sphere in an SVG component.


## License

Copyright © 2014, 2016, 2019- Janos Erdos

Copyright (c) Janos Erdos. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/) which can be found in the file LICENSE.txt at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.
