# How to Contribute

Thank you for having interest in this repository. You may come to this page to know how you can fix your inconvenience. To contribute/improve codes of Haskell parts of this project, you should follow step A. To contribute/improve codes of C/LLVM parts of this project, you should follow step B.

## Step A: For Haskell

### 0. Prerequisites

To download and build the haskell codes in this project completely, you need the following commands/tools.

- [`git`][git]
- [`stack`][stack]
- [`LLVM`][LLVM] (with `llc` and `opt`)

However, if you want to change only a part of this project, you may not need all of these. For more detail,

- `minicute-base`, `minicute-g-machinizer`, and `minicute-parser`
  You don't need `llvm`.
- `minicute-codegenerator` and `minicute`
  You need all of those tools.

### 1. Download this Project

You can download this repository using `git`.

``` shell
git clone https://github.com/CUTE-Lang/miniCUTE.git
cd miniCUTE
```

### 2. Compile and Test this Project

To compile and to test this project, we recommend using [`stack`](https://docs.haskellstack.org/en/stable/README/).

If you already have `stack`, use following command to compile this project,

``` shell
stack build                      # To bulid all packages (require LLVM)
stack build minicute-base # To build minicute-base only
```

and use following command to test this.

``` shell
stack test                      # To test all packages (require LLVM)
stack test minicute-base # To test minicute-base only
```

When you make changes, please check whether your changes are buildable and pass the tests.

### 3. Fork this Repository and Make a PR

After making changes, you may want to share your modifications. To share it on this repository, you need to make a PR.

To make a PR,

1. Fork this repository with "Fork" button.
1. Push your changes to your forked repository via git.
1. Go to "Pull requests" tab of this repository.
1. Click "New pull request" button.
1. Click "compare across forks" text.
1. Choose your forked repository and your updated branch.
1. Click "Preview" tab to select template.
1. Choose an appropriate template.
1. Fill the form you get.
1. Click "Create pull request".

After making a PR, we will review your PR.

Again, thank you for paying attention to this project!

## Step B: For C/LLVM

### 0. Prerequisites

To download and build the C codes in this project completely, you need the following commands/tools.

- [`git`][git]
- [`LLVM`][LLVM] (with `clang`, `llc`, `opt`)

### 1. Download this Project

You can download this repository using `git`.

``` shell
git clone https://github.com/CUTE-Lang/miniCUTE.git
cd miniCUTE
```

### 2. Compile this Project

To build the C parts, use the following commands in the `runtime` directory of cloned directory.

```
make
```

### 3. Fork this Repository and Make a PR

After making changes, you may want to share your modifications. To share it on this repository, you need to make a PR.

To make a PR,

1. Fork this repository with "Fork" button.
1. Push your changes to your forked repository via git.
1. Go to "Pull requests" tab of this repository.
1. Click "New pull request" button.
1. Click "compare across forks" text.
1. Choose your forked repository and your updated branch.
1. Click "Preview" tab to select template.
1. Choose an appropriate template.
1. Fill the form you get.
1. Click "Create pull request".

After making a PR, we will review your PR.

Again, thank you for paying attention to this project!

[git]: https://git-scm.com/
[stack]: https://docs.haskellstack.org/
[LLVM]: https://llvm.org/
