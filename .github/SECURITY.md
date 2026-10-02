# Security policy

## Supported versions

Security fixes go into the latest release of webrarian (0.1.x at present).

## Reporting a vulnerability

Please report a vulnerability privately, not in a public issue:

- through GitHub's private vulnerability reporting:
  <https://github.com/coatless-wasm/webrarian/security/advisories/new>;
- or by e-mail to james.balamuta@gmail.com.

Say what is affected, how to reproduce it and what an attacker gains. You should hear
back within a week, and we will agree a disclosure date with you once a fix is ready.

## What counts

webrarian runs on your computer, with your rights, when you build or preview a site. A
vulnerability is input you do not control making it do something you did not ask for:
for example, a `_webrarian.yml`, a package repository index or a file name that makes
`bind()` or `collection_mirror()` write or delete outside the collection and its output
directory, that injects markup or script into the page `bind()` writes, or that lets the
preview server (`reading_room()`) serve a file from outside the site.

A generated site is a sandbox in its visitor's browser. A share link can add files and
packages to it and run code there: that is the share-link feature, which each site sets
with `repl.share-links` (`open`, `fixed` or `off`). Code on a site's address can also read
the edits the page keeps for its visitors (`repl.persist-edits`). Neither is a
vulnerability in itself; a way around the setting a site chose is, and so is a request
to another origin from a site built with `build.offline: true`.
