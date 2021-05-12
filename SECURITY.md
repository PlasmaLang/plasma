# Security Policy

## Supported Versions

The master branch is currently the only "supported" version.  In the future
there will be stable / development versions with various support lengths.

The bytecode/runtime is not designed as a secure execution environment.  Bad
bytecode can run arbitrary code / cause crashes.
For a secure bytecode/interpreter consider using
[WebAssembly](https://webassembly.org).

## Reporting a Vulnerability

Write an e-mail to `bugs@plasmalang.org`.  Do not submit a PR or issue, Github does not support "private" PRs and they
shouldn't be used to share information that could lead to users being harmed if shared publicly.

