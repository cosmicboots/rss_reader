# RSS Reader

A dead simple RSS reader written in OCaml.

## Building the Project

This project is written in OCaml, and thus it's recommended to use the
[Dune](https://dune.build/) build system to build the project.

Assuming you have the OCaml tool chain (including Dune and
[opam](https://opam.ocaml.org/)) installed, you can setup an environment and
install all the project dependencies with the following:

```shell
opam switch create .
```

At this point you should have a local switch (OCaml's term for environment)
setup in the `./_opam` directory.

You can then build the project with:

```shell
dune build
```

Which will output the `rss_reader` binary at
`./_build/install/default/bin/rss_reader`.

To start the web server, run:
```shell
dune exec rss_reader web-server
```

