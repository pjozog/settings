#!/usr/bin/env python3
"""Validate a JSONC file (JSON with // and /* */ comments), e.g. waybar config.

Usage: check_jsonc.py <file> [<file> ...]

Strips comments, then parses with the stdlib json module. Exits non-zero if
any file fails to parse. This catches structural mistakes (trailing commas,
unbalanced braces, bad quoting) without needing waybar or a display.
"""
import json
import re
import sys

# Remove /* ... */ block comments and // line comments, without touching
# anything inside double-quoted strings.
_TOKEN = re.compile(
    r'"(?:\\.|[^"\\])*"'        # a complete string literal
    r'|/\*.*?\*/'               # block comment
    r'|//[^\n]*'                # line comment
    , re.DOTALL)


def _strip_comments(text: str) -> str:
    def repl(m: 're.Match') -> str:
        s = m.group(0)
        return s if s.startswith('"') else ''
    return _TOKEN.sub(repl, text)


def main() -> int:
    rc = 0
    for path in sys.argv[1:]:
        try:
            with open(path, 'r') as handle:
                json.loads(_strip_comments(handle.read()))
            print('OK  %s' % path)
        except (OSError, ValueError) as err:
            print('ERR %s: %s' % (path, err))
            rc = 1
    return rc


if __name__ == '__main__':
    sys.exit(main())
