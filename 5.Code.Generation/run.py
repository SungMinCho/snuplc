import subprocess
import os
import glob
import difflib
import sys

def hilite(string, color):
  attr = []
  if color == 'green':
    attr.append('32')
  elif color == 'red':
    attr.append('31')
  return '\x1b[%sm%s\x1b[0m' % (';'.join(attr), string)

def main():

  total = 0
  success = 0

  testdir = None
  if(len(sys.argv) == 2):
    testdir = sys.argv[1]
  else:
    testdir = "./test"

  for root, dirs, files in os.walk(testdir):
    for f in files:
      if f.endswith(".mod"):
        total += 1
        f = os.path.join(root, f)
        
        ref = subprocess.Popen(["./reference/snuplc", f], stdout=subprocess.PIPE)
        subprocess.Popen(["cp", f+".s", "ref.s"]).communicate()        

        yours = subprocess.Popen(["./snuplc/snuplc", f], stdout=subprocess.PIPE)
        subprocess.Popen(["cp", f+".s", "yours.s"]).communicate()

        with open("ref.s", "r") as refin, open("yours.s", "r") as youin:
          reflines = refin.readlines()
          youlines = youin.readlines()
          for i in range(len(reflines)):
            reflines[i] = reflines[i].replace('tRBrak', 'tRSqrBrak')
            reflines[i] = reflines[i].replace('tVarDecl', 'tVar')

          d = difflib.Differ()
          diff = d.compare(reflines, youlines)
          cnt = 0
          for line in diff:
            if line[0] == '-':
              if sys.stdout.isatty():
                print(hilite(line, 'red'), end='')
              else:
                print(line, end='')
              cnt += 1
            elif line[0] == '+':
              if sys.stdout.isatty():
                print(hilite(line, 'green'), end='')
              else:
                print(line, end='')
              cnt += 1

        if cnt == 0:
          success += 1
          #print("all same")
        else:
          print("above differences at file : ", f)

        subprocess.Popen(["rm", "ref.s"]).communicate()
        subprocess.Popen(["rm", "yours.s"]).communicate()

  print()
  print("score", success, "/", total)


if __name__ == "__main__":
  main()
