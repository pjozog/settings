import os

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
def CppOptions():
  return {
    'flags': [
        '-std=c++11',
        '-x', 'c++',
        '-Wall',

        # Should put project-specific stuff first
        '-isystem', '/usr/include/eigen3',

        # "Real" system headers go here
        '-isystem', '/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/c++/5.4.0',
        '-isystem', '/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/x86_64-linux-gnu/c++/5.4.0',
        '-isystem', '/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/c++/5.4.0/backward',
        '-isystem', '/usr/include/clang/5.0.0/include',
        '-isystem', '/usr/local/include',
        '-isystem', '/usr/include/x86_64-linux-gnu',
        '-isystem', '/usr/include',
    ]
  }

def COptions():
  return {
    'flags': [
        '-x', 'c',
        '-Wall',
        '-isystem', '/usr/local/include',
        '-isystem', '/usr/lib/llvm-3.8/bin/../lib/clang/3.8.0/include',
        '-isystem', '/usr/include/x86_64-linux-gnu',
        '-isystem', '/usr/include',
    ]
  }

def IsCFile(filename):
  return os.path.splitext(filename)[-1] == '.c'

def IsHeader(filename):
  return os.path.splitext(filename)[-1] == '.h'

def HasCFile(filename):
  return os.path.exists(os.path.splitext(filename)[0] + '.c')

def FlagsForFile(filename, **kwargs):
  if IsCFile(filename):
    return COptions()

  if IsHeader(filename):
    if HasCFile(filename):
      return COptions()
    else:
      return CppOptions()

  return CppOptions()

if __name__ == '__main__':
  print FlagsForFile('foo.cc')
  print FlagsForFile('foo.c')
  print FlagsForFile('foo.h')
