# NoteMiner

A note-taking tool (written in [Elm](http://elm-lang.org/))

Demo: https://cbenz.github.io/noteminer/

## Features

Still in development!

I'd like to implement:

- [ ] a unique tree with no size limit, movable nodes, undo/redo, history, no deletion but archive
- [ ] "capture" external contents in an "Inbox" node and remember source URL
- [ ] decorate each node with #tags, @contacts, !dates and lat/lon coordinates
- [ ] allow rich contents in nodes like clickable URLs, images, geo maps and why not esoteric things like music notes, perhaps with plugins
- [ ] export nodes in Markdown, to publish on a web site for example
- [ ] share nodes in read only or editable mode via an URL with a secret token
- [ ] generate views (computed derivated trees) by queries (#tag, @contact, !date, lat/lon or fulltext, and sorting)
- [ ] allow tags of type #key:value to create "records"
- [ ] secret nodes (hidden by default then revealed by doing a special action, or locked by a password)
- [ ] realtime editing like etherpad
- [ ] store contents in Markdown files (stored in a Git repo or a cloud-based drive) and mount them in the tree

Inspired from many tools I know or use: Google Keep, Emacs Org-mode, Workflowy, Etherpad and Google Wave (discontinued).

I develop this essentially to discover the [Elm language](http://elm-lang.org/) and to have fun!

## Development

- Clone this repo and go in directory
- Start `elm-reactor`
- Open http://localhost:8000/index.html in your browser

## Production

To compile and deploy to GitHub static page in `gh-pages` branch, run `./production/deploy.sh`.

## References

- localstorage code is taken from [elm-todomvc](https://github.com/evancz/elm-todomvc).
