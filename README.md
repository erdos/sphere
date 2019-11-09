# erdos.lenart

An interactive environment for spherical geometry in the web browser.

It displays a real time view of the sphere in an SVG component. It also gives a small DSL to describe constructions on the sphere.

This project is a successor of [Sphaerica, the Interactive Spherical Geometry Software](https://github.com/erdos/sphaerica). I started working on this implementation around 2014. The project has little practical value. It is, however, an interesting tought experiment on DSL development, spherical geometry, functional programming and other fascinating areas.

G. E. O. M. E. T. R. Why? Because I gotta.


## Usage

Go to the [Live Demo](https://erdos.dev/sgdl.html) page.


## Contribution

Feel free to reach out with bug reports or new ideas. Contact with direct message, or open an Issue.


### Development

1. Install [Boot](https://boot-clj.com/) for Clojure.
2. To get started in development mode, type: `boot dev`
3. Open the browser window at `http://localhost:8000/index.html` for a live view of the project.

The project is written in ClojureScript. It uses React (with Reagent) to render the sphere in an SVG component.

## License

Copyright © 2014, 2016, 2019- Janos Erdos

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
