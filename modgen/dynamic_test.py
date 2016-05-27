import subprocess
import os
import glob
import difflib
import sys
from modgen import *

def hilite(string, color):
  attr = []
  if color == 'green':
    attr.append('32')
  elif color == 'red':
    attr.append('31')
  return '\x1b[%sm%s\x1b[0m' % (';'.join(attr), string)

def main():

  if len(sys.argv) < 2:
    print('usage : python3 modgen.py reference yours [save_wrongfiles_at="wrongfiles"] [funcnum=5] [statnum=10] [statlength=100]')
    exit()

  reference_program = sys.argv[1]
  your_program = sys.argv[2]

  savewrongfiles = "wrongfiles"
  if len(sys.argv) >= 4:
    savewrongfiles = sys.argv[3]
  funcnum = 5
  if len(sys.argv) >= 5:
    funcnum = int(sys.argv[4])
  statnum = 10
  if len(sys.argv) >= 6:
    funcnum = int(sys.argv[5])
  statlength = 100
  if len(sys.argv) >= 7:
    statlength = int(sys.argv[6])

  total = 0
  success = 0
  wrongcount = 0

  while True:
    m = Module("test", funcnum, statnum, statlength)
    with open("temp.mod", "w") as modfile:
      print(m, file=modfile)

    with open("temp.mod", "r") as f, open("ref", "w") as refout, open("you", "w") as youout:
      ref = subprocess.Popen([reference_program, "temp.mod"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
      refout.write(ref.stdout.read().decode())
      refout.write(ref.stderr.read().decode())

      yours = subprocess.Popen([your_program, "temp.mod"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
      youout.write(yours.stdout.read().decode())
      youout.write(yours.stderr.read().decode())

    with open("ref", "r") as refin, open("you", "r") as youin:
      reflines = refin.readlines()
      youlines = youin.readlines()
      for i in range(len(reflines)):
        reflines[i] = reflines[i].replace('tRBrak', 'tRSqrBrak')
        reflines[i] = reflines[i].replace('tVarDecl', 'tVar')

      d = difflib.Differ()
      diff = d.compare(reflines, youlines)

      diffs = []
      for d in diff:
        diffs.append(d)
      diff = diffs

      different = False
      for line in diff:
        if line.startswith('-') or line.startswith('+'):
          different = True
          break

      if different: #different
        subprocess.Popen(["cp", "temp.mod", os.path.join(savewrongfiles, "wrong" + str(wrongcount) + ".mod")]).communicate()
        with open(os.path.join(savewrongfiles, "log" + str(wrongcount)), "w") as log:
          for line in diff:
            if line[0] == '-':
              if sys.stdout.isatty():
                print(hilite(line, 'red'), end='', file=log)
              else:
                print(line, end='', file=log)
            elif line[0] == '+':
              if sys.stdout.isatty():
                print(hilite(line, 'green'), end='', file=log)
              else:
                print(line, end='', file=log)
        wrongcount += 1
      else: # success
        success += 1

      total += 1
      print(str(success) + '/' + str(total), end="\r")

    subprocess.Popen(["rm", "ref"]).communicate()
    subprocess.Popen(["rm", "you"]).communicate()

    subprocess.Popen(["rm", "temp.mod"]).communicate()


if __name__ == "__main__":
  main()
