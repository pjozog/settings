#!/usr/bin/env python3
"""Verify the result of running applysettings.py into a sandbox HOME.

Usage: verify_install.py <repo_root> <sandbox_home>

Checks:
  1. A minimum set of expected symlinks exist (the installer actually ran).
  2. Every symlink under HOME that points *into the repo* resolves to a file
     that exists. This catches dead link pairs (e.g. a source file that was
     deleted but still referenced in the link table).
  3. Symlinks pointing outside the repo (the user's private ~/ownCloud files)
     are allowed to dangle and are only reported, not failed.

Exits non-zero if any check fails.
"""
import os
import sys

# Links whose source lives outside the repo and is intentionally not shipped
# here (private files). These are allowed to be missing in CI.
EXTERNAL_OK_SUFFIXES = (
    os.path.join('ownCloud', ''),  # anything under ~/ownCloud/...
)

# A few links that must always be created, as a sanity check that the
# installer did its job rather than silently doing nothing.
EXPECTED_LINKS = [
    '.bashrc',
    '.zshrc',
    '.profile',
    '.gitconfig',
    '.emacs.d',
    os.path.join('.config', 'waybar'),
    '.sway',
    os.path.join('.config', 'gtk-3.0', 'settings.ini'),
]


def main() -> int:
    if len(sys.argv) != 3:
        print(__doc__)
        return 2
    repo_root = os.path.abspath(sys.argv[1])
    home = os.path.abspath(sys.argv[2])

    errors = []
    external = []

    # 1. Expected links present.
    for rel in EXPECTED_LINKS:
        path = os.path.join(home, rel)
        if not os.path.islink(path):
            errors.append('expected symlink missing: ~/%s' % rel)

    # 2 & 3. Walk every symlink in HOME.
    for dirpath, dirnames, filenames in os.walk(home):
        for name in list(dirnames) + filenames:
            path = os.path.join(dirpath, name)
            if not os.path.islink(path):
                continue
            target = os.path.realpath(path)
            raw = os.readlink(path)
            inside_repo = os.path.abspath(
                os.path.join(os.path.dirname(path), raw)
            ).startswith(repo_root + os.sep)
            if inside_repo:
                if not os.path.exists(target):
                    errors.append(
                        'in-repo link target missing: ~/%s -> %s'
                        % (os.path.relpath(path, home), raw))
            else:
                if not os.path.exists(target):
                    external.append(
                        '~/%s -> %s' % (os.path.relpath(path, home), raw))

    if external:
        print('Note: external links not resolvable in CI (expected):')
        for line in sorted(external):
            print('  ' + line)

    if errors:
        print('\nFAIL:')
        for line in errors:
            print('  ' + line)
        return 1

    print('\nOK: installer produced the expected symlinks and every '
          'in-repo link target exists.')
    return 0


if __name__ == '__main__':
    sys.exit(main())
