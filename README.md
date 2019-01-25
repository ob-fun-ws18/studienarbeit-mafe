# IRCServer [![Build Status](https://www.travis-ci.com/ob-fun-ws18/studienarbeit-mafe.svg?branch=master)](https://www.travis-ci.com/ob-fun-ws18/studienarbeit-mafe)

_Marita Segl, Felix Maurer_

[Documentation](https://ob-fun-ws18.github.io/studienarbeit-mafe/index.html) |
[Coverage](https://ob-fun-ws18.github.io/studienarbeit-mafe/hpc/index.html)

## How to build
```sh
stack build
```

## How to run
```sh
stack exec IRCServer-exe
```

The server has been tested with these clients:
- irssi
- HexChat
- Textual

## Responsibilities
### Marita Segl
- Parser
- Behandlung der Events
- Build/Travis
### Felix Maurer
- Grundlegende Server-Struktur
- Behandlung der Events
- Konzept und Helper für Test von Funktionen mit IO/Handles
