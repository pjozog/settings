# Type linting instructions

1. After running `applysettings.py` (to put `mypy.ini` in the right place), create a virtual environment:
```
$ python -m venv my-venv
```

2. Install the requirements.txt:
```
$ . my-venv/bin/activate
$ pip install --upgrade pip
$ pip install -r path/to/requirements.txt
```

3. Install the following packages (as of Oct 27, 2023):
```
pip install python-lsp-server[all]
pip install pylsp-mypy  # NOT pyls-mypy!!! (without the 'p')
```

4. Run emacs as normal; type linting should work, yay!
