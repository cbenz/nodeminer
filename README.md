# NoteMiner

A note-taking tool (written in [Elm](http://elm-lang.org/))

Demo: https://cbenz.github.io/noteminer/

## Development

- Clone this repo and go in directory
- Start `elm-reactor`
- Open http://localhost:8000/index.html in your browser

## Production

```
elm-make src/Main.elm --warn --output=docs/main.js
```

## References

- localstorage code is taken from [elm-todomvc](https://github.com/evancz/elm-todomvc).
