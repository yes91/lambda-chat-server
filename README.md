# lambda-chat-server

A Haskell server implementation for the lambda-chat protocol.

## Installation

To install, simply install the [stack](https://docs.haskellstack.org/en/stable/README/) build tool, `cd` into the project directory and run the following:

``stack install``

This will install the program into a configurable location. The default location may not be on your `PATH` so keep that in mind.

To run the program directly, one may also use the command:

``stack run -- ARGS``

Where `ARGS` are passed on the command line to the program.

## Usage

The program consists of a single binary, `lambda-chat-server`.

Here is the output of `--help`, for reference:

```
Usage: lambda-chat-server [-p|--port PORT]
  Run a chat server at port PORT

Available options:
  -p,--port PORT           The port at which to run the chat
                           service. (default: "3000")
  -h,--help                Show this help text
```
