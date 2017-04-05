# This is an example per-project config file for ycmd
#
# To get a list of the right include directories, see the #include <...> section
# of this command:
#
# $ echo | clang++-3.8 -v -E -x c++ -std=c++11 -
#
# Copy this to the root directory of the project you're working on (call it
# .ycm_extra_conf.py (note the dot '.')), and modify the flags accordingly (use
# -isystem for headers which should not throw warnings, -I for your project
# headers)
def FlagsForFile( filename, **kwargs ):
  return {
    'flags': [
        '-std=c++11',
        '-x', 'c++',
        '-Wall',
        '-isystem', '/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/c++/5.4.0',
        '-isystem', '/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/x86_64-linux-gnu/c++/5.4.0',
        '-isystem', '/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/c++/5.4.0/backward',
        '-isystem', '/usr/local/include',
        '-isystem', '/usr/lib/llvm-3.8/bin/../lib/clang/3.8.0/include',
        '-isystem', '/usr/include/x86_64-linux-gnu',
        '-isystem', '/usr/include',
    ]
  }
