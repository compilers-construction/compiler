# Scheme Compiler

## Git

You'll be implementing this (and future) assignments by adding code to a Git repository. The first thing you need to 
do is a download and install a copy of [Git](https://git-scm.com/downloads).

Note: Hebrew text in **WILL** break your vagrant (see below) setup. Make sure not to place your project in a 
directory with Hebrew characters in its path (i.e. the directory itself or a parent directory).

### Using Git

To get a local copy of your git repository, you need to enter the command `git clone <repository_url>`. Afterwards, the 
general Git workflow for your work is: 
- `<implement parts of the assignment>`
- `git add <modified_file_1> <modified_file_2>` or, to include all changed files `git add *` 
- `git commit -m "<short description of what you did>"`
- `git push`

**If you don't `add, commit and push` your code, you didn't submit it**

Any part in angle-brackets should be filled out by your specific input

### git clone

The `git clone` command copies a repository from a remote URL down to your computer and links your local copy with that 
remote repository so you can upload code back into that remote repository.

A reference where you can find your repository URL:
![git clone URL](/readme.d/git_clone.png?raw=true)

###  git add
The `git add` command tells Git to track the files you selected (or all files in the current directory, recursively if 
you used `*`). Tracking means these files will be included in the next commit.

To see what files are "added", you can use the  `git status` command and look under "Changes to be committed". Only 
files listed there will be included if you run `git commit` (see below)

### git commit
The `git commit` command tells Git to collect all tracked files (see [git add](#git-add)) into a single "transaction" 
called a "commit" and attach a message to that commit (using the `-m` parameter).

Commits are the basic unit of work in Git.

### git push
The `git push` command takes all the commits you created locally, and sends them to the remote repository (by default, 
the URL from which you cloned the repository). This command actually uploads your work to the server, at which point 
you can see the changes on GitHub.

## VM

Since we'll be grading your work on a Linux machine, and since we wanted you all to use the same software versions as we 
do for grading, we provide you with a VM. However, since setting up the VM for efficient use might take some effort, 
and can be frustrating we'll be using two industry standard tools to run this VM - VirtualBox and Vagrant. You've used 
VirtualBox before, so we'll skip that bit.

### Vagrant

This repository contains a Vagrant setup file called `Vagrantfile` (paraphrasing on `makefile`). Vagrant is a tool for 
sharing VMs. It includes the VM image, setup, configuration, network connections and just about anything you'd want to 
configure before you start working. The vagrantfile contains all the commands required to setup your VM.

#### Using Vagrant
First, download and install [Vagrant](https://www.vagrantup.com/downloads) for your OS. Next, open a terminal in the 
project's root directory (where this `README.md` file is stored) and run `vagrant up`. When running this command
for the first time it might seem like it's stuck on "Downloading" for up to 3 hours, depending on your internet
connection. It's not stuck. 

Note: On windows, Vagrant doesn't give you any indication of download progress, so you can take a look in 
`%HOMEPATH%/. vagrant.d/tmp` and see the file there that represents the downloading VM. Once it reaches 7GB, Vagrant
will continue setting up your VM

The `vagrant up` command will read the Vagrantfile, download the correct VM image, create the VM using VirtualBox, 
create a shared folder between the VM and the host, setup an SSH connection between the VM and start the VM. The first
time you run `vagrant up` the VM image it downloads is rather large (7GB). If you run `vagrant up` again (e.g. if to 
rebuild your VM), Vagrant will use a local cache of the VM image, so it will run MUCH faster (less than a minute).

To connect to your VM, you need to run `vagrant ssh`. This will open an SSH connection to your VM and provide you a 
shell in which you can run your code. Note, the project's folder is mapped to `~/compiler` in the VM (when you log 
in with `vagrant SSH` it will take you to the home folder at `~`), so the first command you execute should be 
`cd compiler`.

If you want to stop your VM, you can execute `vagrant halt`. It will gracefully terminate the VM.

When you want to delete your VM you can execute `vagrant destroy` (helpful if you accidentally destroy you VM somehow). 
Note, if you don't want/can't use `vagrant destroy`, you need to delete the VM from VirtualBox **AND** delete the 
`.vagrant` folder inside the project's root with `rm -rf ./.vagrant` (executed from within the project's root).

### Connecting to the VM graphically
The VM created by `vagrant up` is a regular VM in VirtualBox, so if you'd like, you can just open up the VirtualBox
dashboard, and double-click the relevant VM. The password for the Vagrant user is `vagrant`. 

The VM comes with VScode, Intellij and Emacs all three with their Ocaml plugins. Also it comes preinstalled with Scheme, 
Ocaml, utop, gcc, gdb, nasm and whatever tool we figured you might need to work on this and future assignments. Of course
You can always install any other tool you like from the Ubuntu repositories with `apt install`

## Testing the Reader 

To help improve your chances at a good grade, we HIGHLY recommend you use a form of unit testing. However, we often see 
cases of students who write their testing code inside their assignment (i.e. inside `reader.ml`), which often leads to 
failures during grading because of leftover testing code.

The correct way to unit test your work is to create a separate testing file that loads your code (i.e. `reader.ml`) and 
applies tests to it. To encourage you to test your code correctly and quickly we're including the testing framework that 
will be used in grading your work (under the `tests` directory).

The way this testing framework works is by taking all the files in `tests/cases`, running them through your reader, 
converting the results back into s-expressions and using Scheme's `equal?` function to compare the original input with 
your output.

To run these tests you simply open a terminal in your project's root dir and run `tests/test_cases.sh` and you should 
see the result for each tested file. So for a fresh repo you should see:
```
$ cat tests/cases/boolean.scm
#t #f

$ tests/test_cases.sh 
--------------------boolean.scm--------------------:
Exception: X_not_yet_implemented.
FAIL: 
```

And after your first implement Booleans incorrectly (for the sake of demonstration) you might see something like:
```
$ tests/test_cases.sh 
--------------------boolean.scm--------------------:
FAIL: `(tests/cases/boolean.scm: ,(equal? '(#t #f) '(#t #\f)))
```

And finally, after implementing some parsers (correctly) and adding a test of your own called `my_test.ml` to 
`tests/cases`, you will see:
```
# cat tests/cases/my_test.scm
#t #\t 1 abc

$ tests/test_cases.sh 
--------------------boolean.scm--------------------:
PASS: (tests/cases/my_test.scm: #t)
--------------------my_test.scm--------------------:
PASS: (tests/cases/my_test.scm: #t)
```

Again, **we highly encourage you to add your tests to the tests/cases/** so you can test your code quickly and often.

## Testing your Tag-Parser
With the skeleton you got for your tag-parser, you also got a testing setup that should be simple to use and extend.

The entire testing setup can be found in `./tests/test_tag_parser.ml` - cases and testing code.

When you first run `utop ./tests/test_tag_parser.ml` you should get the following output:
```
$ utop tests/test_tag_parser.ml 
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: #t: Boolean
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: #\a: Char
.
.
.
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: `(#\a b ,c ,@d 'e (f)): QQ-list
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: `#(a ,b ,@c "d"): QQ-vector
```
Each line represents a test-case. The first part of the line is the result (currently all tests result in a thrown
exception), the second part is the name of the test (e.g. "Boolean" or "QQ-vector" above)

The abovce exceptions mean your tag parser failed to match the S-expression input to a known Scheme expression. Of 
course as you implement parts of your parser, these test will pass, and eventually, your output should be:
```
PASS: Boolean
PASS: Char
.
.
.
PASS: QQ-list
PASS: QQ-vector
```

The tests coupled with this assignment are rather shallow. You are encouraged to add tests to this test file.
A test case is an Ocaml record of the form `{name: string; input: sexpr; expected: expr}`, where name is just a string 
used to help the user figure what failed and what passed, the input is sent to `tag_parse_experessions` and `expected` 
is the expected output to compare with. Add new cases to the `cases` list in `test_tag_parser.ml` to extend your testing 
setup. 

## Testing your Semantic-Analyser

Same as with the Tag-parser, you are provided with a test setup to test your Semantic Analyser automatically. The 
test setup can be found in `tests/test_semantic_analyser.ml`.

When you first run  `utop ./tests/test_semantic_analyser.ml` you should get the following output:
```
$ utop ./tests/test_semantic_analyser.ml 
Exception: Syntax not yet implemented: Expr-lexical annotation
Exception: Syntax not yet implemented: Expr'-TP annotation
Exception: Syntax not yet implemented: Expr'-box

```

The test cases for the Semantic analyser are divided into two types. Tests that take as input an `expr` type (to test
the lexical addressing stage), and tests that take as input an `expr'` type (to test the other two stages). You can
see the name of the test is prefixed with the test type ("Expr"/"Expr'").

After you implement your Semantic Analyser the output you should see is:
```
$ utop ./tests/test_semantic_analyser.ml 
PASS: Expr-lexical annotation
PASS: Expr'-TP annotation
PASS: Expr'-box
```

You are encouraged to add your own tests to `test_semantic_analyser.ml` since this is how we'll be testing your
Semantic Analyser.

## Testing your Compiler
In this assignment, we will treat your compiler as a black box, using the provided `Makefile` as the interface. 
This means that we will provide a text file containing valid code, and expect as output an executable, which we will 
run. We will not look at the results or behavior of the internal components of your compiler.

### Testing Extended Syntax
Since your compiler supports an extended syntax that isn't part of Scheme (e.g. interpolated strings), our test setup
supports taking an "expected output" file. This "expected output" should contain Scheme code equivalent to the input 
file but which does not use the extended syntax.

Assuming you want to test `extended.scm` which contains some of our extended syntax. Include `extended.expected` file
in the same directory as `extended.scm`, and the test procedure will use that `.expected` file to define the expected
outout.

### Test Procedure 

We will run your compiler on various test files. For each test case, for example `foo` (which tests the file `foo.scm`),
we will do three things:
1. Test if `foo.expected` exists
    1. If so run `foo.expected` in Chez Scheme
    1. Otherwise, run `foo.scm` in Chez Scheme
1. Collect the output from Chez Scheme in a list
1. Run your compiler on `foo.scm`, obtaining an executable `foo`. We shall then execute `foo` and collect its output in 
a list
1. The two outputs collected from Chez and from the executable shall be compared using the `equal?` in Chez Scheme 
(Chez's implementation of `equal?`, not that in your `stdlib.scm`).

If the `equal?` returns `#t`, you passed the test case. You could lose a point if your code OCaml (*reader*, 
*tag-parser*, *semantic-analyser*, *code-generator*) processed the input incorrectly, if `nasm` failed to assemble the 
assembly file, if `gcc` failed to link your file, if the resulting executable caused a segmentation fault, if the 
resulting executable generated unnecessary output, or if the `equal?` predicate in Chez Scheme returned anything other 
than `#t` when comparing the two lists.

Assuming you want to test the file `foo.scm`, you should run, from the root directory of your proect:
``` bash 
  testfile=foo; \
  echo testfile = $testfile; \
  make -f ./compiler/Makefile $testfile; \
  expected_file=$([ -f $testfile.expected ] && echo $testfile.expected || echo $testfile.scm); \
  echo expected_file = $expected_file; \
  o1=`scheme -q < $expected_file`; \
  o2=`./$testfile`; \
  echo "(equal? '($o1) '($o2))" > test.scm; \
  scheme -q < test.scm
```

#### Test Script
The same testing procedure is implemented in `tests/test_compiler.sh`, except it defines `testfile` using a argument.
To run the `foo` test case described above you should execute:
``` 
  tr -d \r tests/test_compiler.sh
  tests/test_compiler.sh foo
```

Note that first command (the one starting with `tr`). That will fix any cases where a Windows OS has ruined the test
script by changing the end of line from UNIX style (ASCII 10) to Windows style (ASCII 10 followed by ASCII 13). It's not
a part of the test, just a workaround for that destructive behavior for Git on Windows.

## Submission
**To submit your code, you need to commit and push your work to this repository and associate your repository with
your BGU user**.

When you push your work to the remote repository, you update the "main" branch for the remote repository. The latest 
commit in your "main" branch at the time when the deadline expires will be your submitted work. 

**To associate your work with your BGU user**, you'll need to submit a single file to the submission system. You should
execute: `git remote get-url origin > submission`. This command will create a file called `submission` that contains
the URL to your repository. You should upload that file, and nothing else, to the submission system.
