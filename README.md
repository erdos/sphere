# Interactive spherical geometry in the web browser.

It is a real time editor for geometric constructions on the sphere.

# [Try it online](https://sphere.erdos.dev/)

The project is a successor of [Sphaerica, the Interactive Spherical Geometry Software](https://github.com/erdos/sphaerica). I started working on the current implementation in 2014. The goal of the project is to provide an interactive virtual environment for spherical geometry constructions. It is a tought experiment on DSL development, spherical geometry, functional programming and other fascinating areas.


## Contribution

Feel free to reach out with bug reports or new ideas. Contact with direct message, or open an Issue.


### Development

1. Install [Boot](https://boot-clj.com/) for Clojure.
2. To get started in development mode, type: `boot dev`
3. Open the browser window at `http://localhost:8000/index.html` for a live view of the project.

Alternatively, you can use `docker-compose` to run a development session: `docker-compose -f dev.yml up`

The project is written in ClojureScript. It uses React (with Reagent) to render the sphere in an SVG component.


## License

Copyright © 2014, 2016, 2019- Janos Erdos

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
