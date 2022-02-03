# mongoose_jid

[![Hex pm](http://img.shields.io/hexpm/v/mongoose_jid.svg?style=flat)](https://hex.pm/packages/mongoose_jid)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/mongoose_jid/)
[![Downloads](https://img.shields.io/hexpm/dt/mongoose_jid.svg)](https://hex.pm/packages/mongoose_jid)
[![License](https://img.shields.io/hexpm/l/mongoose_jid.svg)](https://github.com/esl/mongoose_jid/blob/master/LICENSE)

An XMPP library for parsing JIDs, fully compliant with [RFC6122](https://datatracker.ietf.org/doc/html/rfc6122)

It offers functionality for parsing jids and turning them into jid records, normalising their parts or skipping normalisation when the input is trusted, and also to build binaries for the jid representation. It is highly performant, uses NIFs where it's best to, and has a wide API that can be consulted in the documentation.
