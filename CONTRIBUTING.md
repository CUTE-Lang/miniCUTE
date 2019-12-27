# How to Contribute

Thank you for having interest in this repository. You may come to this page to know how you can fix your inconvenience. To contribute/improve codes of Haskell parts of this project, please follow step A. To contribute/improve codes of C/LLVM parts of this project, please follow step B.

## Step A: For Haskell

### 0. Prerequisites

To download and build the haskell codes in this project completely, you need the following commands/tools.

- [`git`][git homepage]
- [`stack`][stack homepage]
- [`LLVM`][LLVM homepage] (with `llc` and `opt`)

However, if you want to change only a part of this project, you may not need all of these. More specifically,

- All packages other than `minicute-llvm-generator` and `minicute-minicute-compiler`
  These packages do not require `llvm` (including `llc` and `opt`).
- `minicute-llvm-generator` and `minicute-minicute-compiler`
  These packages require all of those tools.

### 1. Download this Project

You can download this repository using `git`.

``` shell
git clone https://github.com/CUTE-Lang/miniCUTE.git
cd miniCUTE
```

### 2. Compile and Test this Project

To compile and to test this project, we recommend using [`stack`][stack homepage].

After installing `stack`, use the following commands to compile this project,

``` shell
stack build                        # To bulid all packages (require LLVM)
stack build minicute-common-syntax # To build minicute-common-syntax only
```

and use the next commands to test this.

``` shell
stack test                        # To test all packages (require LLVM)
stack test minicute-common-syntax # To test minicute-common-syntax only
```

When you change codes, please check whether your patches are compiled and pass the tests.

### 3. Fork this Repository and Make a PR

After making changes, you may want to share your modifications. To share it on this repository, you need to make a PR.

To make a PR,

1. Fork this repository with "Fork" button.
1. Push your changes to your forked repository via `git`.
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

- [`git`][git homepage]
- [`LLVM`][LLVM homepage] (with `clang`, `llc`, `opt`)

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
1. Push your changes to your forked repository via `git`.
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

[git homepage]: https://git-scm.com/
[stack homepage]: https://docs.haskellstack.org/
[LLVM homepage]: https://llvm.org/
